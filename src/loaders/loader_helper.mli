open Bitstring

exception LoadError

val read : in_channel -> int -> bytes
val read_bitstring : in_channel -> int -> Bitstring.t
val read_int32 : in_channel -> int32

val rle_decode : bytes -> int list
val rle2_decode : bytes -> int list
val rle3_decode : bytes -> int list