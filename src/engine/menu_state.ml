open OgamlCore;;
open OgamlGraphics;;
open OgamlMath;;
open State;;

type t = {
 sprites         : (string * Sprite.t) list;
 audio           : (string * unit) list
};;

let _state = ref None;;

let update g = match !_state with 
| None -> (* Load resources *)
  let sprite_of_image img = Sprite.create ~texture:(Texture.Texture2D.create (module Window) g.window (`Image img)) () in
  _state := Some ({
    sprites= [
      ("mouse_cursor", sprite_of_image @@ Resources.get_cursor g.resources "default")
    ];
    audio= [

    ];
  }); 
  g
| Some (state) ->
  g
;;

let draw g = match !_state with | None -> g | Some (state) ->
  (* Draw cursor *)
  let mouse_pos = Mouse.relative_position g.window in
  let mouse_sprite = List.assoc "mouse_cursor" state.sprites in
  Sprite.set_position mouse_sprite @@ Vector2f.from_int mouse_pos;
  Sprite.draw (module Window) ~target:g.window ~sprite:mouse_sprite ();

  g
;;

let handle_event g ev = match ev with
| Event.ButtonPressed (be) -> (match be.button with
  | Left -> Printf.printf "leftclick!!!\n%!"; g
  | _ -> g
)
| Event.KeyPressed (ke) -> (match ke.key with
  | Escape -> Printf.printf "escape!!!!\n%!"; { g with state=`IngameState }
  | _ -> g)
| _ -> g
;;