## Release v0.17.0

- Removed parameters and attributes:
  * Removed `max_mem_waiting_gc_in_bytes` parameter from `Base_bigstring.create`

- Added functions:
  * `Base_bigstring.unsafe_memset` for filling a range with a character without bounds checks
  * `Base_bigstring.memcmp_string` for efficient comparison between `Bigstring` and `string` data
  * `Base_bigstring.get_string` and `Base_bigstring.unsafe_get_string` (also in `Base_bigstring.Local`) for obtaining strings with specified position and length, avoiding optional arguments

- Improved performance of accessors which convert a 64-bit integer to an `int`:
  * `Base_bigstring.get_int64_le_exn`
  * `Base_bigstring.get_int64_be_exn`
  * `Base_bigstring.get_uint64_le_exn`
  * `Base_bigstring.get_uint64_be_exn`
  * `Base_bigstring.unsafe_get_int64_le_exn`
  * `Base_bigstring.unsafe_get_int64_be_exn`
  * `Base_bigstring.unsafe_get_uint64_le_exn`
  * `Base_bigstring.unsafe_get_uint64_be_exn`

## Release v0.16.0:

- Improved support for locals (stack allocation):
  * Add `[@local]` attribute to the `t` arguments of `length`, `unsafe_set_int8`,
    `unsafe_set_int16_le`, `unsafe_set_int16_be`, `unsafe_set_int32_le` and
    `unsafe_set_int32_be`.
  * Likewise for the `Int64.t` arguments of `set_int64_t_le`, `set_int64_t_be`,
    `unsafe_set_int64_t_le`, and `unsafe_set_int64_t_be`.
  * Add new `Local` module providing `get_int64_t_le`, `get_int64_t_be`,
    `unsafe_get_int64_t_le`, and `unsafe_get_int64_t_be` functions that return
    local `Int64.t` values.

- New functions:
  * `unsafe_get`: retrieves character at specified position, without bounds checks
  * `unsafe_set`: sets character at specified position, without bounds checks
  * `memmem`: searches for the position of a substring of `needle` in a substring of
    `haystack` with optional parameters for positions and lengths; returns an `int option`
  * `unsafe_memmem`: unsafe version of `memmem` function, without bounds checks
