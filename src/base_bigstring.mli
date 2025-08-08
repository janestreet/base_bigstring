(** String type based on [Bigarray], for use in I/O and C-bindings. *)

open! Base
open Stdlib.Bigarray

(** {2 Types and exceptions} *)

(** Type of bigstrings *)
type t = (char, int8_unsigned_elt, c_layout) Array1.t
[@@deriving compare ~localize, equal ~localize, globalize, sexp, sexp_grammar]

(** Type of bigstrings which support hashing. Note that mutation invalidates previous
    hashes. *)
type t_frozen = t
[@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

(** {2 Creation and string conversion} *)

(** [create length]
    @return a new bigstring having [length]. Content is undefined. *)
val create : int -> t

(** [empty] is a bigstring of length 0 *)
val empty : t

(** [init n ~f] creates a bigstring [t] of length [n], with [t.{i} = f i]. *)
val init : int -> f:(int -> char) -> t

(** [of_string ?pos ?len str]
    @return
      a new bigstring that is equivalent to the substring of length [len] in [str]
      starting at position [pos].

    @param pos default = 0
    @param len default = [String.length str - pos] *)
val of_string : ?pos:int -> ?len:int -> string -> t

(** [of_bytes ?pos ?len str]
    @return
      a new bigstring that is equivalent to the subbytes of length [len] in [str] starting
      at position [pos].

    @param pos default = 0
    @param len default = [Bytes.length str - pos] *)
val of_bytes : ?pos:int -> ?len:int -> bytes -> t

(** [to_string ?pos ?len bstr]
    @return
      a new string that is equivalent to the substring of length [len] in [bstr] starting
      at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise Invalid_argument if the string would exceed runtime limits. *)
val to_string : ?pos:int -> ?len:int -> t -> string

(** [to_bytes ?pos ?len bstr]
    @return
      a new byte sequence that is equivalent to the substring of length [len] in [bstr]
      starting at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise Invalid_argument if the bytes would exceed runtime limits. *)
val to_bytes : ?pos:int -> ?len:int -> t -> bytes

(** [concat ?sep list] returns the concatenation of [list] with [sep] in between each. *)
val concat : ?sep:t -> t list -> t

(** {2 Checking} *)

(** [check_args ~loc ~pos ~len bstr] checks the position and length arguments [pos] and
    [len] for bigstrings [bstr].
    @raise 
      Invalid_argument if these arguments are illegal for the given bigstring using [loc]
      to indicate the calling context. *)
val check_args : loc:string -> pos:int -> len:int -> t -> unit

(** [get_opt_len bstr ~pos opt_len]
    @return
      the length of a subbigstring in [bstr] starting at position [pos] and given optional
      length [opt_len]. This function does not check the validity of its arguments. Use
      {!check_args} for that purpose. *)
val get_opt_len : t -> pos:int -> int option -> int

(** {2 Accessors} *)

(** [length bstr]
    @return the length of bigstring [bstr]. *)
val length : t -> int

(** [get t pos] returns the character at [pos] *)
external get : (t[@local_opt]) -> int -> char = "%caml_ba_ref_1"

(** [unsafe_get t pos] returns the character at [pos], without bounds checks. *)
external unsafe_get : (t[@local_opt]) -> int -> char = "%caml_ba_unsafe_ref_1"

(** [set t pos] sets the character at [pos] *)
external set : (t[@local_opt]) -> int -> char -> unit = "%caml_ba_set_1"

(** [unsafe_set t pos] sets the character at [pos], without bounds checks. *)
external unsafe_set : (t[@local_opt]) -> int -> char -> unit = "%caml_ba_unsafe_set_1"

(** [is_mmapped bstr]
    @return whether the bigstring [bstr] is memory-mapped. *)
external is_mmapped : (t[@local_opt]) -> bool = "bigstring_is_mmapped_stub"
[@@noalloc]

(** {2 Blitting} *)

(** [blit ~src ?src_pos ?src_len ~dst ?dst_pos ()] blits [src_len] characters from [src]
    starting at position [src_pos] to [dst] at position [dst_pos].

    @raise Invalid_argument if the designated ranges are out of bounds. *)

include Blit.S with type t := t

val copy : t -> t

module To_string : sig
  val blit : (t, bytes) Blit.blit
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.blit] instead"]

  val blito : (t, bytes) Blit.blito
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.blito] instead"]

  val unsafe_blit : (t, bytes) Blit.blit
  [@@deprecated "[since 2017-10] use [Bigstring.To_bytes.unsafe_blit] instead"]

  include Blit.S_to_string with type t := t
end

module From_string : Blit.S_distinct with type src := string with type dst := t
module To_bytes : Blit.S_distinct with type src := t with type dst := bytes
module From_bytes : Blit.S_distinct with type src := bytes with type dst := t

(** [memset t ~pos ~len c] fills [t] with [c] within the range [\[pos, pos + len)]. *)
val memset : t -> pos:int -> len:int -> char -> unit

(** [unsafe_memset t ~pos ~len c] fills [t] with [c] within the range [\[pos, pos + len)],
    without bounds checks. *)
val unsafe_memset : t -> pos:int -> len:int -> char -> unit

(** Memcmp *)

(** [memcmp t1 ~pos1 t2 ~pos2 ~len] is like [compare t1 t2] except performs the comparison
    on the subregions of [t1] and [t2] defined by [pos1], [pos2], and [len]. *)
val memcmp : t -> pos1:int -> t -> pos2:int -> len:int -> int

(** [memcmp_bytes], for efficient [memcmp] between [Bigstring] and [Bytes] data. *)
val memcmp_bytes : t -> pos1:int -> Bytes.t -> pos2:int -> len:int -> int

(** [memcmp_string], for efficient [memcmp] between [Bigstring] and [string] data. *)
val memcmp_string : t -> pos1:int -> string -> pos2:int -> len:int -> int

(** Compares up to [len] characters of two (potentially null-terminated) strings beginning
    at [pos1] and [pos2] of their respective [Bigstring]s. This function starts comparing
    the first character of each string. If they are equal to each other, it continues with
    the following pairs until the characters differ, until a terminating null-character is
    reached, or until [len] characters match in both strings, whichever happens first.

    [unsafe_strncmp] does no bounds checking. *)
external unsafe_strncmp
  :  (t[@local_opt])
  -> pos1:int
  -> (t[@local_opt])
  -> pos2:int
  -> len:int
  -> int
  = "bigstring_strncmp"
[@@noalloc]

(** {2 Search} *)

(** [find ?pos ?len char t] returns [Some i] for the smallest [i >= pos] such that
    [t.{i} = char], or [None] if there is no such [i].

    @param pos default = 0
    @param len default = [length bstr - pos] *)
val find : ?pos:int -> ?len:int -> char -> t -> int option

(** [rfind ?pos ?len char t] returns [Some i] for the largest [i >= pos] such that
    [t.{i} = char], or [None] if there is no such [i].

    @param pos default = 0
    @param len default = [length bstr - pos] *)
val rfind : ?pos:int -> ?len:int -> char -> t -> int option

(** Same as [find], but does no bounds checking, and returns a negative value instead of
    [None] if [char] is not found. *)
external unsafe_find
  :  (t[@local_opt])
  -> char
  -> pos:int
  -> len:int
  -> int
  = "bigstring_find"
[@@noalloc]

(** Same as [rfind], but does no bounds checking, and returns a negative value instead of
    [None] if [char] is not found. *)
external unsafe_rfind
  :  (t[@local_opt])
  -> char
  -> pos:int
  -> len:int
  -> int
  = "bigstring_rfind"
[@@noalloc]

(** Search for the position of (a substring of) [needle] in (a substring of) [haystack]. *)
val memmem
  :  haystack:t
  -> needle:t
  -> ?haystack_pos:int
  -> ?haystack_len:int
  -> ?needle_pos:int
  -> ?needle_len:int
  -> unit
  -> int option

(** As [unsafe_find] for [memmem]. *)
external unsafe_memmem
  :  haystack:(t[@local_opt])
  -> needle:(t[@local_opt])
  -> haystack_pos:int
  -> haystack_len:int
  -> needle_pos:int
  -> needle_len:int
  -> int
  = "bigstring_memmem_bytecode" "bigstring_memmem"
[@@noalloc]

(** {v
 {2 Accessors for parsing binary values, analogous to [Binary_packing]}

    These are in [Bigstring] rather than a separate module because:

    1. Existing [Binary_packing] requires copies and does not work with [bigstring]s.
    2. The accessors rely on the implementation of [bigstring], and hence should change
    should the implementation of [bigstring] move away from [Bigarray].
    3. [Bigstring] already has some external C functions, so it didn't require many
    changes to the [jbuild] ^_^.

    In a departure from [Binary_packing], the naming conventions are chosen to be close to
    C99 stdint types, as it's a more standard description and it is somewhat useful in
    making compact macros for the implementations.  The accessor names contain endian-ness
    to allow for branch-free implementations

    <accessor>  ::= <unsafe><operation><type><endian>
    <unsafe>    ::= unsafe_ | ''
    <operation> ::= get_ | set_
    <type>      ::= int8 | uint8 | int16 | uint16 | int32 | uint32 | int64 | uint64
    <endian>    ::= _le | _be | ''

    The [unsafe_] prefix indicates that these functions do no bounds checking and silently
    truncate out-of-range numeric arguments.
    v} *)

val get_int8 : t -> pos:int -> int
val set_int8_exn : t -> pos:int -> int -> unit
val get_uint8 : t -> pos:int -> int
val set_uint8_exn : t -> pos:int -> int -> unit
val unsafe_get_int8 : t -> pos:int -> int
val unsafe_set_int8 : t -> pos:int -> int -> unit
val unsafe_get_uint8 : t -> pos:int -> int
val unsafe_set_uint8 : t -> pos:int -> int -> unit

(** {2 16-bit methods} *)

val get_int16_le : t -> pos:int -> int
val get_int16_be : t -> pos:int -> int
val set_int16_le_exn : t -> pos:int -> int -> unit
val set_int16_be_exn : t -> pos:int -> int -> unit
val unsafe_get_int16_le : t -> pos:int -> int
val unsafe_get_int16_be : t -> pos:int -> int
val unsafe_set_int16_le : t -> pos:int -> int -> unit
val unsafe_set_int16_be : t -> pos:int -> int -> unit
val get_uint16_le : t -> pos:int -> int
val get_uint16_be : t -> pos:int -> int
val set_uint16_le_exn : t -> pos:int -> int -> unit
val set_uint16_be_exn : t -> pos:int -> int -> unit
val unsafe_get_uint16_le : t -> pos:int -> int
val unsafe_get_uint16_be : t -> pos:int -> int
val unsafe_set_uint16_le : t -> pos:int -> int -> unit
val unsafe_set_uint16_be : t -> pos:int -> int -> unit

(** {2 32-bit methods} *)

val get_int32_le : t -> pos:int -> int
val get_int32_be : t -> pos:int -> int
val set_int32_le_exn : t -> pos:int -> int -> unit
val set_int32_be_exn : t -> pos:int -> int -> unit
val unsafe_get_int32_le : t -> pos:int -> int
val unsafe_get_int32_be : t -> pos:int -> int
val unsafe_set_int32_le : t -> pos:int -> int -> unit
val unsafe_set_int32_be : t -> pos:int -> int -> unit
val get_uint32_le : t -> pos:int -> int
val get_uint32_be : t -> pos:int -> int
val set_uint32_le_exn : t -> pos:int -> int -> unit
val set_uint32_be_exn : t -> pos:int -> int -> unit
val unsafe_get_uint32_le : t -> pos:int -> int
val unsafe_get_uint32_be : t -> pos:int -> int
val unsafe_set_uint32_le : t -> pos:int -> int -> unit
val unsafe_set_uint32_be : t -> pos:int -> int -> unit

(** Similar to the usage in binary_packing, the below methods are treating the value being
    read (or written), as an ocaml immediate integer, as such it is actually 63 bits. If
    the user is confident that the range of values used in practice will not require
    64-bit precision (i.e. Less than Max_Long), then we can avoid allocation and use an
    immediate. If the user is wrong, an exception will be thrown (for get). *)

(** {2 64-bit signed values} *)

val get_int64_le_exn : t -> pos:int -> int
val get_int64_be_exn : t -> pos:int -> int
val get_int64_le_trunc : t -> pos:int -> int
val get_int64_be_trunc : t -> pos:int -> int
val set_int64_le : t -> pos:int -> int -> unit
val set_int64_be : t -> pos:int -> int -> unit
val unsafe_get_int64_le_exn : t -> pos:int -> int
val unsafe_get_int64_be_exn : t -> pos:int -> int
val unsafe_get_int64_le_trunc : t -> pos:int -> int
val unsafe_get_int64_be_trunc : t -> pos:int -> int
val unsafe_set_int64_le : t -> pos:int -> int -> unit
val unsafe_set_int64_be : t -> pos:int -> int -> unit

(** {2 64-bit unsigned values} *)

val get_uint64_be_exn : t -> pos:int -> int
val get_uint64_le_exn : t -> pos:int -> int
val set_uint64_le_exn : t -> pos:int -> int -> unit
val set_uint64_be_exn : t -> pos:int -> int -> unit
val unsafe_get_uint64_be_exn : t -> pos:int -> int
val unsafe_get_uint64_le_exn : t -> pos:int -> int
val unsafe_set_uint64_le : t -> pos:int -> int -> unit
val unsafe_set_uint64_be : t -> pos:int -> int -> unit

(** {2 32-bit methods with full precision} *)

val get_int32_t_le : t -> pos:int -> Int32.t
val get_int32_t_be : t -> pos:int -> Int32.t
val set_int32_t_le : t -> pos:int -> Int32.t -> unit
val set_int32_t_be : t -> pos:int -> Int32.t -> unit
val unsafe_get_int32_t_le : t -> pos:int -> Int32.t
val unsafe_get_int32_t_be : t -> pos:int -> Int32.t
val unsafe_set_int32_t_le : t -> pos:int -> Int32.t -> unit
val unsafe_set_int32_t_be : t -> pos:int -> Int32.t -> unit

(** {2 64-bit methods with full precision} *)

val get_int64_t_le : t -> pos:int -> Int64.t
val get_int64_t_be : t -> pos:int -> Int64.t
val set_int64_t_le : t -> pos:int -> Int64.t -> unit
val set_int64_t_be : t -> pos:int -> Int64.t -> unit
val unsafe_get_int64_t_le : t -> pos:int -> Int64.t
val unsafe_get_int64_t_be : t -> pos:int -> Int64.t
val unsafe_set_int64_t_le : t -> pos:int -> Int64.t -> unit
val unsafe_set_int64_t_be : t -> pos:int -> Int64.t -> unit

(** {2 String methods}

    These are alternatives to [to_string] that follow the conventions of the int
    accessors, and in particular avoid optional arguments. *)

val get_string : t -> pos:int -> len:int -> string
val unsafe_get_string : t -> pos:int -> len:int -> string

module Local : sig
  val get_int64_t_le : t -> pos:int -> Int64.t
  val get_int64_t_be : t -> pos:int -> Int64.t
  val unsafe_get_int64_t_le : t -> pos:int -> Int64.t
  val unsafe_get_int64_t_be : t -> pos:int -> Int64.t
  val get_string : t -> pos:int -> len:int -> string
  val unsafe_get_string : t -> pos:int -> len:int -> string
end

module Int_repr : sig
  include Int_repr.Get with type t := t
  include Int_repr.Set with type t := t

  module Unsafe : sig
    include Int_repr.Get with type t := t
    include Int_repr.Set with type t := t
  end
end

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val sign_extend_16 : int -> int
end
