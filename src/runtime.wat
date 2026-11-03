(module
   (@if $portable-int
   (@then
      (import "env" "int_val_32_exn"
         (func $int_val_32_exn (param (ref eq)) (param (ref eq)) (result i32)))
   ))
   (import "env" "caml_ba_create"
      (func $caml_ba_create
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_get_1"
      (func  $caml_ba_get_1 (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_ba"
      (func $bigstring_blit_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_bytes_to_ba"
      (func $bigstring_blit_bytes_bigstring_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_string_to_ba"
      (func $bigstring_blit_string_bigstring_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_bytes"
      (func $bigstring_blit_bigstring_bytes_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memset"
      (func $bigstring_memset_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "env" "caml_bigstring_memcmp"
      (func $bigstring_memcmp_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memcmp_string"
      (func $bigstring_memcmp_bytes_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_strncmp"
      (func $bigstring_strncmp
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memchr"
      (func $bigstring_find
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "env" "caml_bigstring_memrchr"
      (func $bigstring_rfind
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "env" "caml_hash_mix_bigstring"
      (func $caml_hash_mix_bigstring
         (param i32) (param (ref eq)) (result i32)))
   (import "env" "Int32_val" (func $Int32_val (param (ref eq)) (result i32)))
   (import "env" "caml_copy_int32"
      (func $caml_copy_int32 (param $i i32) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (@if $portable-int
   (@then
      ;; Mirrors native [Long_val] truncated to a uint32.
      (import "env" "portable_int_val_32"
         (func $state_val (param (ref eq)) (result i32)))

      ;; In portable-int mode, [Sys.word_size] is 64, so the internal hash
      ;; state is a plain OCaml int holding a uint32 (mirroring the native
      ;; stubs); since a uint32 does not fit in an i31, it may be a boxed
      ;; large int.
      (type $ocaml_large_int (struct (field i64)))

      (func $val_state (param $h i32) (result (ref eq))
         ;; Mirrors native [Val_long] of a uint32: always non-negative.
         (if (result (ref eq))
             (i32.lt_u (local.get $h) (i32.const 0x40000000))
            (then (ref.i31 (local.get $h)))
            (else
               (struct.new $ocaml_large_int
                  (i64.extend_i32_u (local.get $h))))))
   ))

   (@string $bb_range "Base_bigstring.memmem: index out of range")

   (func (export "bigstring_alloc_v2")
      (param $size (ref eq)) (result (ref eq))
      (return_call $caml_ba_create
        (ref.i31 (i32.const 12)) ;; kind: Char
        (ref.i31 (i32.const 0)) ;; layout: c_layout
        (array.new_fixed $block 2 (ref.i31 (i32.const 0)) (local.get $size))))

   (func (export "bigstring_is_mmapped_stub") (param (ref eq)) (result (ref eq))
       (ref.i31 (i32.const 0)))

   (export "bigstring_blit_stub" (func $bigstring_blit_stub))

   (export "bigstring_blit_bytes_bigstring_stub"
      (func $bigstring_blit_bytes_bigstring_stub))

   (export "bigstring_blit_bigstring_bytes_stub"
      (func $bigstring_blit_bigstring_bytes_stub))

   (export "bigstring_blit_string_bigstring_stub"
      (func $bigstring_blit_string_bigstring_stub))

   (export "bigstring_memset_stub" (func $bigstring_memset_stub))

   (export "bigstring_memcmp_stub" (func $bigstring_memcmp_stub))

   (export "bigstring_memcmp_bytes_stub" (func $bigstring_memcmp_bytes_stub))

   (export "bigstring_strncmp" (func $bigstring_strncmp))

   (@if $portable-int
   (@then
      (func (export "internalhash_fold_bigstring")
         (param (ref eq)) (param (ref eq)) (result (ref eq))
         (return_call $val_state
            (call $caml_hash_mix_bigstring
               (call $state_val (local.get 0))
               (local.get 1))))
   )
   (@else
      (func (export "internalhash_fold_bigstring")
         (param (ref eq)) (param (ref eq)) (result (ref eq))
         (return_call $caml_copy_int32
            (call $caml_hash_mix_bigstring
               (call $Int32_val (local.get 0))
               (local.get 1))))
   ))

   (export "bigstring_find" (func $bigstring_find))

   (export "bigstring_rfind" (func $bigstring_rfind))

   (func (export "bigstring_memmem_bytecode")
      (param $haystack (ref eq)) (param $needle (ref eq))
      (param $v_haystack_pos (ref eq)) (param $v_haystack_len (ref eq))
      (param $v_needle_pos (ref eq)) (param $v_needle_len (ref eq))
      (result (ref eq))
      (local $haystack_pos i32) (local $haystack_len i32)
      (local $needle_pos i32) (local $needle_len i32)
      (local $i i32) (local $j i32) (local $lim i32)
      (local.set $haystack_pos
         (@if $portable-int
         (@then
            (call $int_val_32_exn (local.get $v_haystack_pos)
               (global.get $bb_range)))
         (@else
            (i31.get_s (ref.cast (ref i31) (local.get $v_haystack_pos))))
         ))
      (local.set $haystack_len
         (@if $portable-int
         (@then
            (call $int_val_32_exn (local.get $v_haystack_len)
               (global.get $bb_range)))
         (@else
            (i31.get_s (ref.cast (ref i31) (local.get $v_haystack_len))))
         ))
      (local.set $needle_pos
         (@if $portable-int
         (@then
            (call $int_val_32_exn (local.get $v_needle_pos)
               (global.get $bb_range)))
         (@else
            (i31.get_s (ref.cast (ref i31) (local.get $v_needle_pos))))
         ))
      (local.set $needle_len
         (@if $portable-int
         (@then
            (call $int_val_32_exn (local.get $v_needle_len)
               (global.get $bb_range)))
         (@else
            (i31.get_s (ref.cast (ref i31) (local.get $v_needle_len))))
         ))
      (local.set $lim
         (i32.sub (local.get $haystack_len) (local.get $needle_len)))
      (loop $outer
         (if (i32.le_s (local.get $i) (local.get $lim))
            (then
               (local.set $j (i32.const 0))
               (loop $inner
                  (if (i32.lt_s (local.get $j) (local.get $needle_len))
                     (then
                        (if (ref.eq
                               (call $caml_ba_get_1 (local.get $haystack)
                                  (ref.i31
                                     (i32.add (local.get $haystack_pos)
                                        (i32.add (local.get $i)
                                           (local.get $j)))))
                               (call $caml_ba_get_1 (local.get $needle)
                                  (ref.i31
                                     (i32.add (local.get $needle_pos)
                                        (local.get $j)))))
                           (then
                              (local.set $j
                                 (i32.add (local.get $j) (i32.const 1)))
                              (br $inner)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $outer))))
               (return
                  (ref.i31
                     (i32.add (local.get $haystack_pos) (local.get $i)))))))
      (ref.i31 (i32.const -1)))
)
