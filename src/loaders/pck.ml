open Bitstring;;
open Printf;;
open Loader_helper;;

type t = int;;

let load path =
  let ic = open_in_bin path in
  match%bitstring read_bitstring ic 4 with 
  | {| 
    magic : 4 * 8 : string
  |} -> 
    if magic <> "DATA" then raise LoadError;
    0
;;