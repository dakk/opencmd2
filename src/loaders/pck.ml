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
  Printf.printf "%sd %s\n%!" (String.make l ' ') n;
    pt ftl' l
  | Directory (n, ft') :: ftl' -> 
    Printf.printf "%sd %s\n%!" (String.make l ' ') n;
    pt ft' @@ l + 1;
    pt ftl' l
  in pt [pck.tree] 0
;;

let open_file pck path = pck.file;;

let read_file pck path = "";;

let load path =
  let rec read_fdbs ic =    
    let rec fix_align ic = 
      if (pos_in ic) mod 0x800 <> 0 then (
        seek_in ic @@ (pos_in ic) + 1; 
        fix_align ic
      ) else (Printf.printf "asd\n%!")
    in
    match%bitstring read_bitstring ic 48 with 
   | {| 
      filename : 36 * 8 : string;
      ftype : 1 * 8 : littleendian;
      unused : 3 * 8 : string;
      length : 4 * 8 : littleendian;
      address : 4 * 8 : littleendian
    |} -> 
      match ftype with
      | 0x0 -> (* File *)
        File (filename, length, address) :: read_fdbs ic
      | 0x1 -> (* Directory *)
        Printf.printf "dir %s\n%!" filename;
        let cpos = pos_in ic in
        let _ = seek_in ic @@ Int32.to_int address in
        let files = read_fdbs ic in
        Directory (filename, files) :: read_fdbs ic
      | 0xff -> (* Directory tail *)
        Printf.printf "tail\n%!";
        []
      | _ -> read_fdbs ic
  in
  let ic = open_in_bin path in {
    file= ic;
    tree= Directory ("ROOT", read_fdbs ic)
  }
;;