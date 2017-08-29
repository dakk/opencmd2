(** SEC *)

module Terrain : sig
  type major = TIERRA | NIEVE | AGUA | ORILLA | SUBMARINO
  
  type minor = ASFALTO | HIERBA | TIERRA | ADOQUINES | AZULEJOS
    | MADERA | ARENA | NIEVE | HIELO | ROCAS | ARBUSTOS | METAL
    | METAL_ENCHARCADO | ORILLA | AGUA_PROFUNDA | GRAVILLA

  type t = major * minor
end
(** Terrain *)


module Point : sig
  type t = float * float

  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int32 -> t list
end
(** Point 2D *)


module Border : sig
  type t = {
    start_point : int32;
    end_point : int32;
    parent_district : int32;
    neighbor_district : int32;
    unknown : int32;
  }

  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int32 -> t list
end
(** Border *)


module District : sig
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
  
  val from_stream : in_channel -> t
  val read_all_from_stream : in_channel -> int32 -> t list
end
(** District *)


type t = {
  tokens : string list;
  points : Point.t list;
  borders: Border.t list; 
  districts : District.t list;
}

val load : string -> t
(** Load a SEC from file *)