open OgamlCore
open OgamlGraphics
open State

val update : State.t -> State.t
val draw : State.t -> State.t
val handle_event : State.t -> Event.t -> State.t