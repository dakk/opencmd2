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
    if codecnum mod 4 = 0 then (
      []
    ) else (
      []
    )
  in rled data 0 []
;;

(*
            Dim ret As New Queue(Of Int32)
            Dim CodecNum As Integer = 0
            For n As Integer = 0 To Code.Length - 1
                If CodecNum Mod 4 = 0 Then
                    For i As Integer = 0 To Code(n) - 1
                        ret.Enqueue(-1)
                    Next
                Else
                    For i As Integer = n + 1 To n + 1 + Code(n) - 1
                        ret.Enqueue(Code(i))
                    Next
                    n += Code(n)
                End If
                CodecNum += 1
            Next
            Return ret.ToArray
*)
;;

let rle2_decode data = [];;
let rle3_decode data = [];;
