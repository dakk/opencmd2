open Bitstring

exception LoadError

val read : in_channel -> int -> bytes
val read_bitstring : in_channel -> int -> Bitstring.t