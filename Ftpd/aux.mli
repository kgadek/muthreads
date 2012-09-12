val print : string -> unit
val debug : string -> unit
val log : string -> unit
val of_ascii : string -> int -> string * int
val to_ascii : string -> int -> string * int
val split : string -> string list
val unique : string -> string
val ( >>= ) : ('a -> 'b) -> 'a -> 'b
val do_login : string -> string -> (string * int -> unit) -> unit
val set_user : int -> unit
val my_ip : Unix.file_descr -> Unix.inet_addr
val new_socket : unit -> Unix.file_descr
val bind_any : Unix.file_descr -> int -> unit
val make_resp : int -> string -> string
val send : Unix.file_descr -> int -> string -> (unit -> unit) -> unit
val print_dir_line : string -> string
