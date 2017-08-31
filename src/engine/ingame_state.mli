open OgamlCore
open OgamlGraphics
open State

val update : State.t -> State.t
val draw : State.t -> State.t
val handle_event : State.t -> Event.t -> State.t


(* Game save and load functions *)
(*
val save : State.t -> string -> State.t
val save_quick : State.t -> State.t
val load : State.t -> string -> State.t 
val load_quick : State.t -> State.t
*)