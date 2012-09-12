val parse_type_arg : string -> Data.type_code
val parse_struct_arg : string -> Data.struct_code
val parse_mode_arg : string -> Data.mode_code
val parse_port_arg : string -> Unix.inet_addr * int
val parse_allo_arg : string -> int * int option
val parse_cmd : string -> Data.cmd
val make_pasv_text : Unix.inet_addr -> int -> string
