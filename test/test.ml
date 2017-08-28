open OUnit2;;
open Hex;;
open Loaders;;

let mbi_load () octx = 
  let _ = Mbi.load "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/DATA/MISIONES/001/1.mbi" in
	assert_equal false false
;;

let suite = "opencmd2" >::: [
	"mbi.load" >:: mbi_load ();
];;

let () = run_test_tt_main suite;;