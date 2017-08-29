open Bitstring;;
open Printf;;
open Loader_helper;;
open OgamlGraphics;;

module Terrain = struct
  type major = TIERRA | NIEVE | AGUA | ORILLA | SUBMARINO;;

  type minor = ASFALTO | HIERBA | TIERRA | ADOQUINES | AZULEJOS
    | MADERA | ARENA | NIEVE | HIELO | ROCAS | ARBUSTOS | METAL
    | METAL_ENCHARCADO | ORILLA | AGUA_PROFUNDA | GRAVILLA;;

  type t = major * minor;;
end

type t = {
  tokens : string list;
};;

let load path = 
  let rec read_tokens ic tn tokens = match tn with
  | 0l -> tokens
  | tn' -> read_tokens ic (Int32.sub tn' Int32.one) @@ tokens @ [read ic 32]
  in
  let ic = open_in_bin path in
  match%bitstring read_bitstring ic 12 with 
  | {| 
    version : 8 * 8 : littleendian;
    ntoken : 4 * 8 : littleendian
  |} -> 
    let tokens = read_tokens ic ntoken [] in
    match%bitstring read_bitstring ic 32 with
    | {| ident : 32 * 8 : string |} ->
      if String.sub ident 0 4 <> "MAP1" then raise LoadError else
      match%bitstring read_bitstring ic 16 with 
      | {| 
        npoints : 4 * 8 : littleendian;
        nborders : 4 * 8 : littleendian;
        ndistricts : 4 * 8 : littleendian;
        nspecialdistricts : 4 * 8 : littleendian
      |} ->
      (* Read points*)
      (* Read borders*)
      (* Read districts*)
      {
        tokens= tokens
      }
 ;;