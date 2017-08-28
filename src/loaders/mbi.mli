(** Mission mesh *)
open Bitstring;;

module Version : sig
  type t = C1 | C2 | C3 | UNKNOWN

  val of_byte : int -> t
  val to_string : t -> string
end

module Point : sig
  type t = float * float * float

  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int -> t list
end

module District : sig
  type t = {
    attribute : int;
    texture_id: int;
    point_uvs : (int*int*int) list
  }

  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int -> t list
end


module Obj : sig
  type t = {
    name                : string;
    start_district      : int32;
    next_start_district : int32;
  }

  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int -> t list
end


module Texture : sig
  type t = {
    width: int;
    height: int;
    palette: int32 list;
    rectangle: int list list;
  }
  
  val from_stream : in_channel -> t  
  val read_all_from_stream : in_channel -> int -> t list
end


type t = {
  version   : Version.t;
  points    : Point.t list;
  districts : District.t list;
  objects   : Obj.t list;
  textures  : Texture.t list;
}

val load : string -> t
(** Load an MBI from file *)

val to_obj : t -> string -> bool
(** Save t in an .obj file string path *)