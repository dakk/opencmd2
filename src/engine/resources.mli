open OgamlCore
open OgamlGraphics

type t

val init : string -> t

val get_audio : t -> string -> unit
val get_cursor : t -> string -> Image.t

(*
  val get_menu_image : t -> string -> unit
  val get_hud_image : t -> string -> unit
  val get_mission_data : t -> string -> Mission.t
*)

(* 
  Resources loading utilities 

  Puo' essere utilizzato coi loaders per i file originali commandos, oppure un altra implementazione 
  di questa interfaccia permette di creare un gioco completamente nuovo;
  le funzioni del modulo restituiscono solo tipi definiti da ogaml
*)