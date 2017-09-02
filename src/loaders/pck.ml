open Bitstring;;
open Printf;;
open Loader_helper;;

type ftree = Directory of (string * ftree list) | File of (string * int32 * int32);;

type t = {
  file : in_channel;
  tree : ftree
};;


let close pck = close_in pck.file;;

let read_dir pck path = 
  []
;;

let print_tree pck = 
  let rec pt ftl l = match ftl with
  | [] -> ()
  | File (n, s, a) :: ftl' -> 
    Printf.printf "%sf %s\n%!" (String.make l ' ') n;
    pt ftl' l
  | Directory (n, ft') :: ftl' -> 
    Printf.printf "%sd %s\n%!" (String.make l ' ') n;
    pt ft' @@ l + 1;
    pt ftl' l
  in pt [pck.tree] 1
;;


let load path =
  let rec read_fdbs ic =
    match%bitstring read_bitstring ic 48 with 
   | {| 
      filename : 36 * 8 : string;
      ftype : 4 * 8 : littleendian;
      length : 4 * 8 : littleendian;
      address : 4 * 8 : littleendian
    |} -> 
      match ftype with
      | 0x0l -> (* File *)
        File (filename, length, address) :: read_fdbs ic
      | 0x1l -> (* Directory *)
        let cpos = pos_in ic in
        let _ = seek_in ic @@ Int32.to_int address in
        let files = read_fdbs ic in
        let _ = seek_in ic cpos in
        Directory (filename, files) :: read_fdbs ic
      | 0xffl -> (* Directory tail *)
        []
      | x -> Printf.printf "%s\n%!" @@ Int32.to_string x; []
  in
  let ic = open_in_bin path in {
    file= ic;
    tree= List.hd @@ read_fdbs ic
  }
;;