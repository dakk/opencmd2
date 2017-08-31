open OgamlCore;;
open OgamlGraphics;;
open OgamlMath;;

let draw_cursor w s = 
  let mouse_pos = Mouse.relative_position w in
  Sprite.set_position s @@ Vector2f.from_int mouse_pos;
  Sprite.draw (module Window) ~target:w ~sprite:s ();
;;

let sprite_of_image w img = 
  Sprite.create ~texture:(Texture.Texture2D.create (module Window) w (`Image img)) ()
;;