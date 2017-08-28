open Bitstring;;

let read ic n =
  let buff = Bytes.make n '0' in
  let _ = input ic buff 0 n in 
  buff
;;

let read_bitstring ic n = read ic n |> bitstring_of_string;;
