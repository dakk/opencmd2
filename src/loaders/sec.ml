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


module Point = struct
  type t = float * float;;

  let from_stream ic = 
    match%bitstring read_bitstring ic 8 with
    | {| 
      x : 8 * 4 : littleendian;
      y : 8 * 4 : littleendian
    |} -> (Int32.float_of_bits x, Int32.float_of_bits y)
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Border = struct
  type t = {
    start_point : int32;
    end_point : int32;
    parent_district : int32;
    neighbor_district : int32;
    unknown : int32;
  };;

  let from_stream ic = 
    match%bitstring read_bitstring ic 20 with
    | {| 
      sp : 8 * 4 : littleendian;
      ep : 8 * 4 : littleendian;
      pd : 8 * 4 : littleendian;
      nd : 8 * 4 : littleendian;
      un : 8 * 4 : littleendian
    |} -> {
      start_point= sp;
      end_point= ep;
      parent_district= pd;
      neighbor_district= nd;
      unknown= un
    }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module District = struct
  type t = {
    terrain : Terrain.t;
    kx : float;
    ky : float;
    bz : float;
    unknown : int32;
    minpx : float;
    minpy : float;
    minpz : float;
    maxpx : float;
    maxpy : float;
    maxpz : float;
    borders : int32 list;
  }
  
  let from_stream ic = 
    let rec read_borders ic nborders = match nborders with 
    | 0l -> []
    | nb' -> match%bitstring read_bitstring ic 4 with
      | {| border : 8 * 4 : littleendian |} -> border :: read_borders ic (Int32.sub nb' Int32.one)
    in
    match%bitstring read_bitstring ic 51 with
    | {| 
      nborders : 4 * 8 : littleendian;
      terrain : 8 * 8 : littleendian;
      kx : 4 * 8 : littleendian;
      ky : 4 * 8 : littleendian;
      bz : 4 * 8 : littleendian;
      unknown : 3 * 8 : littleendian;
      minpx : 4 * 8 : littleendian;
      minpy : 4 * 8 : littleendian;
      minpz : 4 * 8 : littleendian;
      maxpx : 4 * 8 : littleendian;
      maxpy : 4 * 8 : littleendian;
      maxpz : 4 * 8 : littleendian
    |} -> 
      let borders = read_borders ic nborders in {
        terrain= (TIERRA, TIERRA);
        kx= Int32.float_of_bits kx;
        ky= Int32.float_of_bits ky;
        bz= Int32.float_of_bits bz;
        unknown= unknown;
        minpx= Int32.float_of_bits minpx;
        minpy= Int32.float_of_bits minpy;
        minpz= Int32.float_of_bits minpx;
        maxpx= Int32.float_of_bits maxpx;
        maxpy= Int32.float_of_bits maxpy;
        maxpz= Int32.float_of_bits maxpz;
        borders= borders;
      }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end

type t = {
  tokens : string list;
  points : Point.t list;
  borders: Border.t list; 
  districts : District.t list;
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
      let points = Point.read_all_from_stream ic npoints in
      (* Read borders*)
      let borders = Border.read_all_from_stream ic nborders in
      (* Read districts*)
      let districts = District.read_all_from_stream ic ndistricts in
      {
        tokens= tokens;
        borders= borders;
        points= points;
        districts= districts;
      }
 ;;