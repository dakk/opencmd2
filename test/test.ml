open OUnit2;;
open Hex;;
open Loaders;;

let loaders_mbi_load () octx = 
	let mbi = Mbi.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MISIONES/001/3.mbi" in
	Mbi.to_obj mbi "/home/dakk/test/";
	assert_equal false false
;;

let loaders_sec_load () octx = 
	let sec = Sec.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MISIONES/001/4.sec" in
	assert_equal false false
;;

let loaders_str_load () octx = 
	let _ = Str.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MISIONES/001/003.str" in
	assert_equal false false
;;

let loaders_grl_load () octx =
	let grl = Grl.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MENUS/MISSIONS.GRL" in
	Grl.save_images grl "/home/dakk/test/";
	(*let grl = Grl.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/INTERFAZ/OFFICERS.GRL" in
	Grl.save_images grl "/home/dakk/test/";
	let grl = Grl.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/ANIMS/GRL/ASPAS_BCD08.GRL" in
	Grl.save_images grl "/home/dakk/test/";
	let grl = Grl.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MISIONES/001/5GLIB.GRL" in
	Grl.save_images grl "/home/dakk/test/";
	let grl = Grl.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA/MISIONES/XL1/XLLIB.GRL" in
	Grl.save_images grl "/home/dakk/test/";*)
	assert_equal false false
;;


let loaders_pck_load () octx = 
	let l = Pck.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA.PCK" in
	let _ = Pck.print_tree l in
	let l = Pck.load "/mnt/data/Commandos 2+3 [GOG]__uXfVb7/Commandos 2/DATA2.PCK" in
	let _ = Pck.print_tree l in
	assert_equal false false
;;

let suite = "opencmd2" >::: [
	(*"loaders.mbi.load" >:: loaders_mbi_load ();
	"loaders.str.load" >:: loaders_str_load ();*)
	(*"loaders.grl.load" >:: loaders_grl_load ();*)
	(*"loaders.sec.load" >:: loaders_sec_load ();*)
	"loaders.pck.load" >:: loaders_pck_load ();
];;

let () = run_test_tt_main suite;;