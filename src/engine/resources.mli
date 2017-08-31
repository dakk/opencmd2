open OgamlCore
open OgamlGraphics

type t

val init : string -> t

val get_cursor : t -> string -> Image.t