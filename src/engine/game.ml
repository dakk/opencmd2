open OgamlCore;;
open OgamlGraphics;;
open State;;

let init data_path = 
  let settings = ContextSettings.create ~msaa:8 ~resizable:true ~fullscreen:false () in
  let window = Window.create ~width:1366 ~height:768 ~settings ~title:"OpenCmd2" () in  
  {
    settings= settings;
    window= window;

    data_path= data_path;
    resources= Resources.init data_path;

    state= `MenuState;
  }
;;


let rec loop g = match Window.is_open g.window with
| false -> ()
| true ->
  Window.clear ~color:(Some (`RGB Color.RGB.black)) g.window;
    
  (* Draw *)
  let g' = match g.state with 
  | `MenuState -> Printf.printf "drawing menu\n%!"; Menu_state.draw g
  | _ -> Printf.printf "drawing game\n%!"; g
  in

  let _ = Window.display g.window in

  (* Event handling *)
  let g'' = match Window.poll_event g.window with
  | Some Event.Closed -> Window.close g.window; g'
  | None -> g' 
  | Some ev -> (match g'.state with
    | `MenuState -> Menu_state.handle_event g' ev
    | _ -> g'
  ) in

  (* Update *)
  let g''' = match g.state with 
  | `MenuState -> Menu_state.update g''
  | _ -> g''
  in

  loop g'''
;;