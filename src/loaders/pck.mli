type t

val print_tree : t -> unit

val load : string -> t
(** Open a PCK from file *)

val close : t -> unit
(** Close an opened PCK *)

val read_dir : t -> string -> string list
(** Return the list of files in a directory *)

(*val open_file : t -> string -> in_channel*)
(** Open a file and return the in_channel pointer *)

(*val read_file : t -> string -> bytes*)
(** Read a file and return the bytes data *)