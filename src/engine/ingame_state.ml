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
  _state := Some ({
    sprites= [
      ("mouse_cursor", Utils.sprite_of_image g.window @@ Resources.get_cursor g.resources "default")
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
  Utils.draw_cursor g.window @@ List.assoc "mouse_cursor" state.sprites;
  

  g
;;

let handle_event g ev = match ev with
| Event.ButtonPressed (be) -> (match be.button with
  | Left -> Printf.printf "leftclick!!!\n%!"; g
  | _ -> g
)
| Event.KeyPressed (ke) -> (match ke.key with
  | Escape -> Printf.printf "escape!!!!\n%!"; { g with state=`PauseState }
  | _ -> g)
| _ -> g
;;