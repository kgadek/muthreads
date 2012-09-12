
(*s Data type definitions *)

type type_code = 
  | T_A | T_AT | T_AC | T_E | T_ET | T_EC | T_I | T_L of int
  | T_bad

type struct_code = S_F | S_R | S_P | S_bad

type mode_code = M_S | M_B | M_C | M_bad

type cmd =
  | USER of string
  | PASS of string
  | ACCT of string
  | CWD  of string
  | CDUP
  | SMNT of string
  | QUIT
  | REIN
  | PORT of Unix.inet_addr * int
  | PASV
  | TYPE of type_code
  | STRU of struct_code
  | MODE of mode_code
  | RETR of string
  | STOR of string
  | STOU of string
  | APPE of string
  | ALLO of int * int option
  | REST of int
  | RNFR of string
  | RNTO of string
  | ABOR
  | DELE of string
  | RMD  of string
  | MKD  of string
  | PWD
  | LIST of string
  | NLST of string
  | SITE of string
  | SYST
  | STAT of string option
  | HELP of string option
  | NOOP
  | BAD_arg  (* pseudo command for bad argument *)
  | BAD      (* ... for bad command *)
  | END      (* ... for unexpected close of control connection *)
