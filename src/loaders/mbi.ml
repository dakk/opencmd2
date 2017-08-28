open Bitstring;;

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
    let buff = Bytes.make 12 '0' in
    let _ = input ic buff 0 12 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
    | {| 
      x : 8 * 4 : string;
      y : 8 * 4 : string;
      z : 8 * 4 : string
    |} -> (x, y, z)
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end

module District = struct 
  type t = {
    attribute : int;
    texture_id: int;
    point_uvs : (int*int*int) list
  };;

  let from_stream s = 
    let rec point_uvs ic n = match n with
    | 0 -> []
    | n -> 
      let buff = Bytes.make 6 '0' in
      let _ = input ic buff 0 6 in
      (* TODO these are floats!*)
      match%bitstring bitstring_of_string buff with
      | {| 
        point : 8 * 2 : littleendian;
        u : 8 * 2 : littleendian;
        v : 8 * 2 : littleendian
      |} -> 
        (point, u, v) :: point_uvs ic (n-1)
    in
    let buff = Bytes.make 2 '0' in
    let _ = input s buff 0 2 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
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
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end


module Obj = struct
  type t = {
    name                : string;
    start_district      : int32;
    next_start_district : int32;
  };;

  let from_stream s = 
    let buff = Bytes.make 52 '0' in
    let _ = input s buff 0 52 in
    match%bitstring bitstring_of_string buff with
    | {| 
      name : 8 * 44 : string;
      start_d : 8 * 4 : littleendian;
      next_start_d : 8 * 4 : littleendian
    |} -> { name=name; start_district= start_d; next_start_district=next_start_d }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end


module Texture = struct
  type t = {
    width: int;
    height: int;
    palette: int32 list;
    rectangle: int list list;
  };;
  
  let from_stream s = 
    let rec read_palette s n = match n with
    | 0 -> []
    | n -> 
      let buff = Bytes.make 3 '0' in
      let _ = input s buff 0 3 in
      match%bitstring bitstring_of_string buff with
      | {| pal : 8 * 3 : littleendian |} -> pal :: read_palette s (n - 1)
    in
    let buff = Bytes.make 52 '0' in
    let _ = input s buff 0 52 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
    | {| 
      skip : 8 * 4 : string;
      width : 8 * 4 : littleendian;
      height : 8 * 4 : littleendian
    |} -> {
      width= Int32.to_int width;
      height= Int32.to_int height;
      palette= read_palette s 256;
      rectangle= [];
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
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
  let buff = Bytes.make 13 '0' in
  let _ = input ic buff 0 13 in
  match%bitstring bitstring_of_string buff with 
  | {| 
    version : 1 * 8 : littleendian;
    identif : 3 * 8 : string;
    npoints : 4 * 8 : littleendian;
    ndistricts : 4 * 8 : littleendian
  |} -> 
    Printf.printf "%d %s %d %d\n%!" version identif (Int32.to_int npoints) (Int32.to_int ndistricts); 
    let points = Point.read_all_from_stream ic @@ Int32.to_int npoints in
    let districts = District.read_all_from_stream ic @@ Int32.to_int ndistricts in
    (* read objects *)
    let buff = Bytes.make 4 '0' in
    let _ = input ic buff 0 4 in
    match%bitstring bitstring_of_string buff with 
    | {| nob : 4 * 8 : littleendian |} -> 
      let objs = Obj.read_all_from_stream ic @@ Int32.to_int nob in
    (* read textures *)
    let buff = Bytes.make 4 '0' in
    let _ = input ic buff 0 4 in
    match%bitstring bitstring_of_string buff with 
    | {| not : 4 * 8 : littleendian |} -> 
      
    ()
;;
