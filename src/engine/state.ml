open OgamlCore;;
open OgamlGraphics;;

type t = {
  settings    : ContextSettings.t;
  window      : Window.t;

  data_path   : string;
  resources   : Resources.t;

  state : [`MenuState | `IngameState | `PauseState];
};;

let name_of_state s = match s.state with
| `MenuState -> "menu"
| `IngameState -> "ingame"
| `PauseState -> "pause"
;;