open OUnit2;;
open Hex;;
open Loaders;;

let mbi_load () octx = 
	let mbi = Mbi.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/1.mbi" in
	Mbi.to_obj mbi "/home/dakk/test/";
	assert_equal false false
;;

let str_load () octx = 
	let str = Str.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/003.str" in
	assert_equal false false
;;

let suite = "opencmd2" >::: [
	"mbi.load" >:: mbi_load ();
	"str.load" >:: str_load ();
];;

let () = run_test_tt_main suite;;