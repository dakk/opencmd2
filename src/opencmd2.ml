open Printf;;
open OgamlCore;;
open OgamlGraphics;;


let main () =
    printf "opencmd2 0.0.1\n";
    let settings = OgamlCore.ContextSettings.create ~msaa:8 ~resizable:true ~fullscreen:false () in
    let window = Window.create ~width:1366 ~height:768 ~settings ~title:"OpenCmd2" () in

    while (Window.is_open window) do (
        Window.clear ~color:(Some (`RGB Color.RGB.black)) window;
        
        (* Draw *)

        Window.display window;
        
        (* Event handling *)
        match Window.poll_event window with
        | Some Event.Closed -> Window.close window
        | _ -> ();
    ) done;

    printf "exiting...\n";
;;


main ();;