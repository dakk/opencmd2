open OgamlCore;;
open OgamlGraphics;;

type t = {
  path : string;
};;


let init path = {
  path= path;
};;

let get_audio r aname = 
  ()
;;

let get_cursor r cname = 
  Image.create @@ `File "data/cursor.png"
;;