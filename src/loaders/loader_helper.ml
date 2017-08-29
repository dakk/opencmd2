open Bitstring;;

exception LoadError

let read ic n =
  let buff = Bytes.make n (Char.chr 0x00) in
  let _ = input ic buff 0 n in 
  buff
;;

let read_bitstring ic n = read ic n |> bitstring_of_string;;

let read_int32 ic = match%bitstring read_bitstring ic 4 with
| {| n : 4 * 8 : littleendian |} -> n
;;