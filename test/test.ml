open OUnit2;;
open Hex;;
open Loaders;;

let loaders_mbi_load () octx = 
	let mbi = Mbi.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/2.mbi" in
	Mbi.to_obj mbi "/home/dakk/test/";
	assert_equal false false
;;

let loaders_sec_load () octx = 
	let sec = Sec.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/4.sec" in
	assert_equal false false
;;

let loaders_str_load () octx = 
	let _ = Str.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/003.str" in
	assert_equal false false
;;

let suite = "opencmd2" >::: [
	"loaders.mbi.load" >:: loaders_mbi_load ();
	"loaders.str.load" >:: loaders_str_load ();
	"loaders.sec.load" >:: loaders_sec_load ();
];;

let () = run_test_tt_main suite;;