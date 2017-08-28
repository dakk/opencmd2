open Scanf;;

type t = (int * string) list;;

let load path =
  let rec read_strings ic = 
    try 
      let line = input_line ic in
      let paridx = String.index line ')' + 1 in
      let l = sscanf line "%d %d)" (fun i i' -> (i, String.sub line paridx @@ (String.length line) - paridx))
      in l :: read_strings ic
    with 
    | _ -> []
  in

  let ic = open_in path in
  let strs = read_strings ic in
  close_in ic;
  strs
;;