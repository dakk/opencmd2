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


let rle_decode data = 
  let rec rled data codecnum acc = match Bytes.length data with
  | 0 -> acc
  | n' -> 
    let d' = Bytes.get data 0 |> Char.code in
    let drest = Bytes.sub data 1 @@ n'-1 in
    if codecnum mod 4 = 0 then (
      (*let last = List.rev acc |> List.hd in*)
      let rec mklist n e = match n with | 0 -> [] | n' -> e :: mklist (n' - 1) e in
      rled drest (codecnum + 1) acc @ (mklist d' (-1))
    ) else (
      let rec enq dr n acc' = match n with
      | 0 -> (dr, acc')
      | n' -> 
        let d' = Bytes.get data 0 |> Char.code in
        let drest = Bytes.sub data 1 @@ n'-1 in
        enq drest (n-1) @@ acc' @ [d']
      in
      let dr, acc' = enq drest d' [] in  
      rled dr (codecnum + 1) acc @ acc'
    )
  in rled data 0 []
;;


let rle2_decode data = failwith "not implemented"; [];;
let rle3_decode data = failwith "not implemented"; [];;
