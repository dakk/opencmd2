open Bitstring;;
open Printf;;
open Loader_helper;;
open OgamlGraphics;;


module Version = struct
  type t = C1 | C2 | C3 | UNKNOWN;;

  let of_byte b = match b with
  | 0x31 -> C2
  | 0x32 -> C3
  | _ -> UNKNOWN
  ;;

  let to_string v = match v with 
  | C1 -> "Commandos1"
  | C2 -> "Commandos2"
  | C3 -> "Commandos3"
  | UNKNOWN -> "Unknown"
  ;;
end

module Point = struct
  type t = float * float * float;;

  let from_stream ic = 
    match%bitstring read_bitstring ic 12 with
    | {| 
      x : 8 * 4 : littleendian;
      y : 8 * 4 : littleendian;
      z : 8 * 4 : littleendian
    |} -> (Int32.float_of_bits x, Int32.float_of_bits y, Int32.float_of_bits z)
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end

module District = struct 
  type t = {
    attribute : int;
    texture_id: int;
    point_uvs : (int*float*float) list
  };;

  let from_stream s = 
    let rec point_uvs ic n = match n with
    | 0 -> []
    | n -> 
      match%bitstring read_bitstring ic 6 with
      | {| 
        point : 8 * 2 : littleendian;
        u : 8 * 2 : littleendian;
        v : 8 * 2 : littleendian
      |} -> 
        (point, float (u) /. 4096., float (v) /. 4096.) :: (point_uvs ic @@ n - 1)
    in
    match%bitstring read_bitstring s 2 with
    | {| 
      n : 8 * 1 : littleendian;
      texid : 8 * 1 : littleendian
    |} -> {
      attribute= 0;
      texture_id= texid;
      point_uvs= point_uvs s n
    }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Obj = struct
  type t = {
    name                : string;
    start_district      : int32;
    next_start_district : int32;
  };;

  let from_stream s = 
    match%bitstring read_bitstring s 52 with
    | {| 
      name : 8 * 44 : string;
      start_d : 8 * 4 : littleendian;
      next_start_d : 8 * 4 : littleendian
    |} -> { name=name; start_district= start_d; next_start_district=next_start_d }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Texture = struct
  type t = {
    width: int;
    height: int;
    palette: (int * int * int) list;
    bitmap: int list;
  };;

  let to_image tex = 
    let rec to_raw bm acc = match bm with
    | [] -> string_of_bitstring (concat (List.rev acc))
    | b :: bm' ->
      let (r, g, b) = List.nth tex.palette b in
      to_raw bm' @@ [%bitstring {| r : 8; g : 8; b : 8; 0xff : 8 |}] :: acc
    in Image.create @@ `Data ({x=tex.width; y=tex.height}, to_raw tex.bitmap [])
  ;;
  
  let from_stream s = 
    let rec read_bitmap s pxs = match pxs with
    | 0 -> []
    | pxs' -> 
      match%bitstring read_bitstring s 1 with
      | {| b : 8 * 1 : littleendian |} -> b :: read_bitmap s (pxs' - 1)
    in
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
    match%bitstring read_bitstring s 12 with
    | {| 
      skip : 8 * 4 : string;
      width : 8 * 4 : littleendian;
      height : 8 * 4 : littleendian
    |} -> {
      width= Int32.to_int width;
      height= Int32.to_int height;
      palette= read_palette s 256 [];
      bitmap= read_bitmap s @@ (Int32.to_int width) * (Int32.to_int height);
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


type t = {
  version   : Version.t;
  points    : Point.t list;
  districts : District.t list;
  objects   : Obj.t list;
  textures  : Texture.t list;
};;


let load path = 
  let ic = open_in_bin path in
  match%bitstring read_bitstring ic 12 with 
  | {| 
    version : 1 * 8 : littleendian;
    identif : 3 * 8 : string;
    npoints : 4 * 8 : littleendian;
    ndistricts : 4 * 8 : littleendian
  |} -> 
    if identif <> "IBM" then raise LoadError;
    let points = Point.read_all_from_stream ic npoints in
    let districts = District.read_all_from_stream ic ndistricts in 
    (* read objects *)
    match%bitstring read_bitstring ic 4 with 
    | {| nob : 4 * 8 : littleendian |} -> 
      let objs = Obj.read_all_from_stream ic nob in 
    (* read textures *)
    match%bitstring read_bitstring ic 4 with 
    | {| not : 4 * 8 : littleendian |} -> 
      let texts = Texture.read_all_from_stream ic not in {
        version= Version.of_byte version;
        points= points;
        districts= districts;
        objects= objs;
        textures= texts;
      }
;;


let to_obj mbi path = 
  let rec write_points oc ps = match ps with
  | [] -> ()
  | (x,y,z) :: ps' -> fprintf oc "v  %f %f %f\n" x y z; write_points oc ps'
  in
  let rec write_districts oc (ds:District.t list) vt = match ds with
  | [] -> ()
  | d :: ds' ->
    fprintf oc "g o%d\n" vt;
    fprintf oc "usemtl %d\n" d.texture_id;
    let rec write_district_vt oc dps = match dps with
    | [] -> ()
    | (p, u, v) :: dps' -> 
      fprintf oc "vt %f %f\n" u (1.0 -. v); 
      write_district_vt oc dps'
    in
    let rec write_district_points oc dps vt = match dps with
    | [] -> fprintf oc "\n"; vt
    | (p, u, v) :: dps' -> 
      fprintf oc "%d/%d " (p + 1) (vt + 1); 
      write_district_points oc dps' (vt + 1)
    in
      write_district_vt oc d.point_uvs;
      fprintf oc "f ";
      let vt = write_district_points oc d.point_uvs vt in
      write_districts oc ds' vt
  in
  let rec save_textures mtl txl i = match txl with
  | [] -> close_out mtl
  | t::txl' -> 
    let im = Texture.to_image t in
    Image.save im @@ sprintf "%s/%d.png" path i;
    fprintf mtl "newmtl %d\nillum 0\nmap_Kd %d.png\nKa 0.2 0.2 0.2\nKd 0.8 0.8 0.8\n\n" i i;
    save_textures mtl txl' @@ i + 1
  in
  let oc = open_out @@ path ^ "/scenery.obj" in
  fprintf oc "mtllib scenery.mtl\n";
  fprintf oc "# NumPoint: %d\n" @@ List.length mbi.points;
  fprintf oc "# NumDistrict: %d\n" @@ List.length mbi.districts;
  fprintf oc "# NumObject: %d\n" @@ List.length mbi.objects;
  fprintf oc "# NumTextures: %d\n" @@ List.length mbi.textures;
  save_textures (open_out @@ path ^ (sprintf "/scenery.mtl")) (List.rev mbi.textures) 0;
  write_points oc @@ List.rev mbi.points;
  write_districts oc mbi.districts 0;
  close_out oc;
;;