open OgamlGraphics;;

module Palette : sig
  type t = {
    name : string;
    offset : int32;
    data : (int * int * int) list;
  }
  
  val from_stream : in_channel -> t  
  val read_all_from_stream : in_channel -> int32 -> t list
end

module Image : sig
  type t = {
    name : string;
    offset : int32;
    length : int32;
    palette_index: int32;
    compression : int32;
    width : int32;
    height : int32;
    centerx : int32;
    centery : int32;
    data : int list;
  }

  val to_image : t -> (int * int * int) list -> Image.t
  val from_stream : in_channel -> t  
  val read_all_from_stream : in_channel -> int32 -> t list
end





type t = {
  images : Image.t list;
  palettes : Palette.t list;
}
  
val load : string -> t