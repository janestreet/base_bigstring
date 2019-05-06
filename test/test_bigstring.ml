open! Import
open Base_bigstring

module Bigstring_sequence = struct
  type nonrec t = t

  let create ~len = create len
  let get = get
  let set = set
  let length = length
end

module Bytes_sequence = struct
  type t = bytes [@@deriving sexp_of]

  let create ~len = Bytes.create len
  let get = Bytes.get
  let set = Bytes.set
  let length = Bytes.length
end

module Blit_elt = struct
  include Char

  let of_bool b = if b then 'a' else 'b'
end

include Base_for_tests.Test_blit.Test (Blit_elt) (Bigstring_sequence) (Base_bigstring)

include Base_for_tests.Test_blit.Test_distinct (Blit_elt) (Bytes_sequence)
    (Bigstring_sequence)
    (Base_bigstring.From_bytes)

include Base_for_tests.Test_blit.Test_distinct (Blit_elt) (Bigstring_sequence)
    (Bytes_sequence)
    (Base_bigstring.To_bytes)

let%test_unit "roundtrip" =
  let string_gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Quickcheck.Generator.small_non_negative_int in
    Quickcheck.Generator.list_with_length length Quickcheck.Generator.char_print
    >>| String.of_char_list
  in
  Quickcheck.test string_gen ~f:(fun str ->
    let bstr = of_string str in
    [%test_eq: Base_bigstring.t] bstr (of_string (to_string bstr));
    [%test_eq: Base_bigstring.t] bstr (of_bytes (to_bytes bstr));
    [%test_eq: Base_bigstring.t] bstr (t_of_sexp (sexp_of_t bstr)))
;;

let%test "bigstring created with create are not mmapped" = not (is_mmapped (create 2))

let%expect_test "checking setters (should end in [_exn])" =
  let test setters z =
    List.iteri setters ~f:(fun i setter ->
      let t = init 8 ~f:(fun _ -> '\000') in
      match setter t ~pos:0 z with
      | () -> raise_s [%message "didn't raise" (z : Int.Hex.t) (i : int) (t : t)]
      | exception _ -> ())
  in
  test [ set_int8_exn ] 0x80;
  test [ set_uint8_exn ] (-1);
  test [ set_int16_le_exn; set_int16_be_exn ] 0x8000;
  test [ set_uint16_le_exn; set_uint16_be_exn ] (-1);
  Option.iter (Int64.to_int 0x8000_0000L) ~f:(fun z ->
    test [ set_int32_le_exn; set_int32_be_exn ] z);
  test [ set_uint32_le_exn; set_uint32_be_exn ] (-1);
  test [ set_uint64_le_exn; set_uint64_be_exn ] (-1)
;;

let%test_module "truncating setters (should end in [_trunc] or begin with [unsafe_])" =
  (module struct
    let test setters z =
      List.iter setters ~f:(fun setter ->
        let t = init 8 ~f:(fun _ -> '\000') in
        setter t ~pos:0 z;
        to_string t |> String.iter ~f:(fun c -> Char.to_int c |> printf "%x ");
        printf "; ")
    ;;

    let%expect_test "all word sizes" =
      test [ unsafe_set_int8 ] 0x9080;
      [%expect {| 80 0 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint8 ] (-1);
      [%expect {| ff 0 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_int16_le; unsafe_set_int16_be ] 0x90_8070;
      [%expect {| 70 80 0 0 0 0 0 0 ; 80 70 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint16_le; unsafe_set_uint16_be ] (-1);
      [%expect {| ff ff 0 0 0 0 0 0 ; ff ff 0 0 0 0 0 0 ; |}];
      test [ unsafe_set_uint32_le; unsafe_set_uint32_be ] (-1);
      [%expect {| ff ff ff ff 0 0 0 0 ; ff ff ff ff 0 0 0 0 ; |}];
      test [ unsafe_set_uint64_le; unsafe_set_uint64_be ] (-1);
      [%expect {| ff ff ff ff ff ff ff ff ; ff ff ff ff ff ff ff ff ; |}]
    ;;

    let%expect_test (_[@tags "64-bits-only"]) =
      Option.iter (Int64.to_int 0x90_8070_6050L) ~f:(fun z ->
        test [ unsafe_set_int32_le; unsafe_set_int32_be ] z);
      [%expect {| 50 60 70 80 0 0 0 0 ; 80 70 60 50 0 0 0 0 ; |}]
    ;;
  end)
;;

let getter_t ~first_byte = init 8 ~f:(fun i -> i + first_byte |> Char.of_int_exn)

let%test_module "truncating getters (should end in [_trunc] or begin with [unsafe_])" =
  (module struct
    let test getter =
      List.iter
        [ 0x81 (* positive if top bit truncated *)
        ; 0xc1
          (* negative if top bit truncated *)

        ]
        ~f:(fun first_byte ->
          let i = getter (getter_t ~first_byte) ~pos:0 in
          (* Signed hex is not clear; make sure the hex is unsigned.  Include the signed
             decimal form mainly to indicate the sign. *)
          printf !"0x%x (= %d)\n" i i)
    ;;

    let%expect_test ("63-bit int"[@tags "64-bits-only"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x887868584838281 (= 614607782171345537)
          0x48c7c6c5c4c3c2c1 (= -3978993193046523199) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x182838485868788 (= 108793946209421192)
          0x41c2c3c4c5c6c7c8 (= -4484807029008447544) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x887868584838281 (= 614607782171345537)
          0x48c7c6c5c4c3c2c1 (= -3978993193046523199) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x182838485868788 (= 108793946209421192)
          0x41c2c3c4c5c6c7c8 (= -4484807029008447544) |}]
    ;;

    let%expect_test ("31-bit int"[@tags "32-bits-only", "no-js"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x4838281 (= 75727489)
          0x44c3c2c1 (= -993803583) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x5868788 (= 92702600)
          0x45c6c7c8 (= -976828472) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x4838281 (= 75727489)
          0x44c3c2c1 (= -993803583) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x5868788 (= 92702600)
          0x45c6c7c8 (= -976828472) |}]
    ;;

    let%expect_test ("32-bit int"[@tags "js-only"]) =
      test get_int64_le_trunc;
      [%expect
        {|
          0x84838281 (= -2071756159)
          0xc4c3c2c1 (= -993803583) |}];
      test get_int64_be_trunc;
      [%expect
        {|
          0x85868788 (= -2054781048)
          0xc5c6c7c8 (= -976828472) |}];
      test unsafe_get_int64_le_trunc;
      [%expect
        {|
          0x84838281 (= -2071756159)
          0xc4c3c2c1 (= -993803583) |}];
      test unsafe_get_int64_be_trunc;
      [%expect
        {|
          0x85868788 (= -2054781048)
          0xc5c6c7c8 (= -976828472) |}]
    ;;
  end)
;;

let%expect_test "checking getters (should end in [_exn])" =
  let test getters ~first_bigstring_byte =
    List.iteri getters ~f:(fun i getter ->
      let t = getter_t ~first_byte:first_bigstring_byte in
      match getter t ~pos:0 with
      | z -> raise_s [%message "didn't raise" (i : int) (z : Int.Hex.t)]
      | exception _ -> ())
  in
  test
    (* These should check that the 64th bit in the string representation is redundant, so
       the value is representable as a 63-bit [int] result. *)
    [ get_int64_le_exn
    ; get_int64_be_exn
    ; unsafe_get_int64_le_exn
    ; unsafe_get_int64_be_exn
    ; get_uint64_le_exn
    ; get_uint64_be_exn
    ; unsafe_get_uint64_le_exn
    ; unsafe_get_uint64_be_exn
    ]
    ~first_bigstring_byte:0x80;
  test
    (* These should additionally check that the represented integer is not negative. *)
    [ get_uint64_le_exn
    ; get_uint64_be_exn
    ; unsafe_get_uint64_le_exn
    ; unsafe_get_uint64_be_exn
    ]
    ~first_bigstring_byte:0xc0
;;
