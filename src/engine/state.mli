open OgamlCore
open OgamlGraphics

type t = {
  settings    : ContextSettings.t;
  window      : Window.t;

  data_path   : string;
  resources   : Resources.t;
  
  state  : [`MenuState | `IngameState];
}

