(** SEC *)

module Terrain : sig
  type major = TIERRA | NIEVE | AGUA | ORILLA | SUBMARINO
  
  type minor = ASFALTO | HIERBA | TIERRA | ADOQUINES | AZULEJOS
    | MADERA | ARENA | NIEVE | HIELO | ROCAS | ARBUSTOS | METAL
    | METAL_ENCHARCADO | ORILLA | AGUA_PROFUNDA | GRAVILLA

  type t = major * minor
end
(** Terrain *)

type t = {
  tokens : string list;
}

val load : string -> t
(** Load a SEC from file *)