open Printf;;
open Engine;;

let main () =
    printf "opencmd2 0.0.1\n";

    let gs = Game.init "/home/dakk/.wine/drive_c/GOG Games/Commandos 2/" in
    Game.loop gs; 

    printf "exiting...\n";
;;


main ();;