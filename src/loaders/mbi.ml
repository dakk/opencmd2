type commandos_version = C2 | C3;;

type district = {
  attribute : int;
  n         : int;
  texture   : int;
  point     : int;
  u         : int;
  v         : int;
};;

type obj = {
  name            : string;
  start_district  : int;
  next_district   : int;
};;

type t = {
  version   : commandos_version;
  points    : (float * float * float) list;
  districts : district list;
  objects   : obj list;
  textures  : int list;
};;

let load path = 
  0
;;