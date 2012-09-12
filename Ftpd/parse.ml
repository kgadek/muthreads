(*s Parsing functions *)

open Data

(* These parsing functions may fail on malformed arguments.
Exceptions will be catched in the [parse_cmd] function which will
then return a [BAD_arg] value. *)

let parse_type_arg s =
  if s.[0] = 'L' then 
    Scanf.sscanf s "L %i" (fun i -> T_L i)
  else
    match s with
    | "A" | "A N" -> T_A   (* default *)
    | "A T"       -> T_AT
    | "A C"       -> T_AC
    | "E" | "E N" -> T_E
    | "E T"       -> T_ET
    | "E C"       -> T_EC
    | "I"         -> T_I
    |  _          -> T_bad

let parse_struct_arg s =
  match s.[2] with
  | 'F' -> S_F     (* default (and only supported!) *)
  | 'R' -> S_R
  | 'P' -> S_P
  |  _  -> S_bad

let parse_mode_arg s =
  match s.[2] with
  | 'S' -> M_S     (* default (and only supported!) *)
  | 'B' -> M_B
  | 'C' -> M_C
  |  _  -> M_bad

let parse_port_arg s =
  Scanf.sscanf s "%s@,%s@,%s@,%s@,%i,%i" 
    (fun i1 i2 i3 i4 p1 p2 ->
      Unix.inet_addr_of_string (i1 ^ "." ^ i2 ^ "." ^ i3 ^ "." ^ i4),
      p1*256 + p2)

let parse_allo_arg s =
  try Scanf.sscanf s "%s@ R%c%i" 
      (fun a ' ' b -> int_of_string a, Some b)
  with End_of_file ->
    Scanf.sscanf s "%i" (fun a -> a, None) (* trailing garbage accepted ZZZ *)

(* RFC959 says all commands must end with CRLF (``[_\r\n]'') but as most
implementations, we'll also accept a single '[_\n]'.  Commands are case
insensitive.  We're rather loose on command syntax here.

*)

let parse_cmd s =
  try
    let l = String.length s in
    let e = if s.[l-2] = '\r' && s.[l-1] = '\n' then 2
	else if s.[l-1] = '\n' then 1
	else failwith "bad end of line"
    in
    let arg s i = String.sub s i (l-i-e)
    in
    match String.uppercase (String.sub s 0 4) with
    | "USER" -> USER (arg s 5)
    | "PASS" -> PASS (arg s 5)
    | "ACCT" -> ACCT (arg s 5)
    | "CWD " -> CWD  (arg s 4)
    | "CDUP" -> CDUP
    | "SMNT" -> SMNT (arg s 5)
    | "QUIT" -> QUIT
    | "REIN" -> REIN
    | "PORT" -> let a, p = parse_port_arg (arg s 5) in PORT (a,p)
    | "PASV" -> PASV
    | "TYPE" -> TYPE (parse_type_arg (arg s 5))
    | "STRU" -> STRU (parse_struct_arg (arg s 5))
    | "MODE" -> MODE (parse_mode_arg (arg s 5))
    | "RETR" -> RETR (arg s 5)
    | "STOR" -> STOR (arg s 5)
    | "STOU" -> STOU (arg s 5)
    | "APPE" -> APPE (arg s 5)
    | "ALLO" -> let a,b = parse_allo_arg (arg s 5) in ALLO (a,b)
    | "REST" -> REST (int_of_string (arg s 5))
    | "RNFR" -> RNFR (arg s 5)
    | "RNTO" -> RNTO (arg s 5)
    | "ABOR" -> ABOR
    | "DELE" -> DELE (arg s 5)
    | "RMD " -> RMD  (arg s 4)
    | "MKD " -> MKD  (arg s 4)
    | "PWD\r"-> PWD
    | "PWD\n"-> PWD
    | "LIST" -> LIST (if String.length s > 6 then arg s 5 else "") (*ZZZ*)
    | "NLST" -> NLST (if String.length s > 6 then arg s 5 else "")
    | "SITE" -> SITE (arg s 5)
    | "SYST" -> SYST
    | "STAT" -> STAT (if String.length s > 6 then Some(arg s 5) else None)
    | "HELP" -> HELP (if String.length s > 6 then Some(arg s 5) else None)
    | "NOOP" -> NOOP
    | _ -> BAD
  with _ -> BAD_arg


let make_pasv_text ip p =
  let s = Unix.string_of_inet_addr ip in
  Scanf.sscanf s "%s@.%s@.%s@.%s" 
    (fun i1 i2 i3 i4 ->
      Printf.sprintf "Entering passive mode (%s,%s,%s,%s,%i,%i)"
	i1 i2 i3 i4 (p/256) (p mod 256))


(*i from rfc959

active DTP from port 20 ?

RFC959
            USER <SP> <username> <CRLF>
            PASS <SP> <password> <CRLF>
            ACCT <SP> <account-information> <CRLF>
            CWD  <SP> <pathname> <CRLF>
            CDUP <CRLF>
            SMNT <SP> <pathname> <CRLF>
            QUIT <CRLF>
            REIN <CRLF>
            PORT <SP> <host-port> <CRLF>
            PASV <CRLF>
            TYPE <SP> <type-code> <CRLF>
            STRU <SP> <structure-code> <CRLF>
            MODE <SP> <mode-code> <CRLF>
            RETR <SP> <pathname> <CRLF>
            STOR <SP> <pathname> <CRLF>
            STOU <CRLF>
            APPE <SP> <pathname> <CRLF>
    .        ALLO <SP> <decimal-integer>
                [<SP> R <SP> <decimal-integer>] <CRLF>
            REST <SP> <marker> <CRLF>   -- modifie par RFC3569
            RNFR <SP> <pathname> <CRLF>
            RNTO <SP> <pathname> <CRLF>
    .        ABOR <CRLF>                    -- not for LIST and NLST transfers
            DELE <SP> <pathname> <CRLF>
            RMD  <SP> <pathname> <CRLF>
            MKD  <SP> <pathname> <CRLF>
            PWD  <CRLF>
    .        LIST [<SP> <pathname>] <CRLF>  -- manque date
            NLST [<SP> <pathname>] <CRLF>
            SITE <SP> <string> <CRLF>
            SYST <CRLF>
    .        STAT [<SP> <pathname>] <CRLF>  -- compléter sans arg
            HELP [<SP> <string>] <CRLF>
            NOOP <CRLF>

2389 FEAT


i*)
