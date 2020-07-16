open Bitstring;;
open Printf;;
open Loader_helper;;
open OgamlGraphics;;


module Palette = struct
  type t = {
    name : string;
    offset : int32;
    data : (int * int * int) list;
  };;

  let read_data s offset pal =
    let rec read_palette s n acc = match n with
    | 0 -> acc
    | n -> 
      match%bitstring read_bitstring s 3 with
      | {| 
        r : 8 * 1 : littleendian;
        g : 8 * 1 : littleendian;
        b : 8 * 1 : littleendian 
        |} -> read_palette s (n - 1) @@ acc @ [(r, g, b)]
    in
    seek_in s (offset + (Int32.to_int pal.offset));
    read_palette s 256 []
  ;;

  
  let from_stream s = 
    match%bitstring read_bitstring s 44 with
    | {|
      name : 32 * 8 : string;
      offset : 4 * 8 : littleendian;
      unknown : 8 * 8 : littleendian
    |} -> {
      name= name;
      offset= offset;
      data= [];
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Image = struct
  type t = {
    name : string;
    offset : int32;
    length : int32;
    palette_index: int32;
    compression : int32;
    width : int32;
    height : int32;
    centerx : int32;
    centery : int32;
    data : int list;
  };;

  let to_image im palette = 
    let rec to_raw bm acc = match bm with
    | [] -> string_of_bitstring (concat (List.rev acc))
    | b :: bm' ->
      let (r, g, b) = List.nth palette b in
      to_raw bm' @@ [%bitstring {| r : 8; g : 8; b : 8; 0xff : 8 |}] :: acc
    in 
    Printf.printf "%s - %d %d = %d - %d\n" im.name (Int32.to_int im.width) (Int32.to_int im.height) ((Int32.to_int im.width) * (Int32.to_int im.height)) (List.length im.data);
    
    Image.create @@ `Data ({x=Int32.to_int im.width; y=Int32.to_int im.height}, to_raw im.data [])
  ;;
  

  let read_data s offset im =
    Printf.printf "loading %s\n%!" im.name;
    let rec read_line_address s n acc = match n with
    | 0 -> acc
    | n' -> read_line_address s (n-1) @@ acc @ [read_int32 s]
    in
    seek_in s (offset + (Int32.to_int im.offset) + 4);
    let line_address = read_line_address s (Int32.to_int im.height) [] in
    let offset' = pos_in s in
    let decoder = (match im.compression with
    | 0x2l | 0x4l -> rle_decode
    | 0x142l -> rle2_decode
    | _ -> rle3_decode) in
    let rec decode_lines lprev laddr acc = match laddr with
    | [] -> 
      seek_in s (offset' + Int32.to_int lprev);
      let line = read s (Int32.to_int im.length - Int32.to_int lprev) |> decoder in
      if List.length line <> Int32.to_int im.width then (Printf.printf "%s lineerr\n%!" im.name; failwith "parse error") else 
      List.append acc line
    | l :: laddr' ->
      seek_in s (offset' + Int32.to_int lprev);
      let line = read s (Int32.to_int l - Int32.to_int lprev) |> decoder in
      if List.length line <> Int32.to_int im.width then (Printf.printf "%s lineerr\n%!" im.name; failwith "parse error") else 
      decode_lines l laddr' @@ List.append acc line
    in decode_lines (List.hd line_address) (List.tl line_address) []
  ;;

  let from_stream s = 
    match%bitstring read_bitstring s 64 with
    | {|
      name : 32 * 8 : string;
      offset : 4 * 8 : littleendian;
      length : 4 * 8 : littleendian;
      palette_index : 4 * 8 : littleendian;
      compression : 4 * 8 : littleendian;
      width : 4 * 8 : littleendian;
      height : 4 * 8 : littleendian;
      centerx : 4 * 8 : littleendian;
      centery : 4 * 8 : littleendian
    |} -> {
      name= name;
      offset= offset;
      length= length;
      palette_index= palette_index;
      compression= compression;
      width= width;
      height= height;
      centerx= centerx;
      centery= centery;
      data= [];
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end



type t = {
  images : Image.t list;
  palettes : Palette.t list;
};;

let load path =
  let ic = open_in_bin path in
  match%bitstring read_bitstring ic 24 with
  | {|
    identif : 4 * 8 : string;
    version : 4 * 8 : littleendian;
    imagen : 4 * 8 : littleendian;
    paletten : 4 * 8 : littleendian;
    imageinfolen : 4 * 8 : littleendian;
    paletteinfolen : 4 * 8 : littleendian
  |} ->
    if identif <> "GFRL" then raise LoadError;
    let images = Image.read_all_from_stream ic imagen in
    let palettes = Palette.read_all_from_stream ic paletten in
    let offset = pos_in ic in
    {
      images= List.map (fun x -> try { x with Image.data= Image.read_data ic offset x } with | _ -> x) images;
      palettes= List.map (fun x -> { x with Palette.data= Palette.read_data ic offset x }) palettes;
    }
;;

let save_images grl path = 
  List.iter (fun im -> 
    try
      let i = Image.to_image im (List.nth (List.rev grl.palettes) (Int32.to_int im.palette_index)).data in
      OgamlGraphics.Image.save i @@ sprintf "%s/%s.png" path im.name
    with 
    | _ -> ()
  ) grl.images
;;