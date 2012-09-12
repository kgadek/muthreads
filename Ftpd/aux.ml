open Parse
open Muthr

let print s = print_string s; print_newline (); flush stdout
let debug s = print s
let log   s = print s

let of_ascii s l =            (* change crlf to newline *)
  let c = ref 0 in
  for i = 1 to l-1 do
    if s.[i-1]='\r' && s.[i]='\n' then
      incr c;
    s.[i - !c] <- s.[i]
  done;
  s, l - !c

let to_ascii s l =            (* change newline to crlf *)
  let s'= String.make (l*2) ' '
  in
  let c = ref 0 in
  for i = 0 to l-1 do
    if s.[i]='\n' then begin
      s'.[i + !c] <- '\r';
      incr c
    end;
    s'.[i + !c] <- s.[i]
  done;
  s', l + !c


(* split a multi-line string into a list of strings, with bs n removed *)
let split st =
  let rec split st acc =
    try
      let p = String.rindex st '\n' in
      split (String.sub st 0 p)
	(String.sub st (p+1) ((String.length st - p - 1))::acc)
    with Not_found -> st::acc
  in
  (* we remove the trailing bs n put by [get_list] ZZZ *)
  split (String.sub st 0 (String.length st - 1)) []

(* find a unique name for full pathname name *)
let unique name =
  let rec loop name i =
    let ni = name ^ (string_of_int i) in
    if Sys.file_exists ni then loop name (i+1)
    else ni
  in
  if Sys.file_exists name then loop name 1
  else name


(* The authentication is performed through PAM.  We ask PAM not to add
any delay on failure, because it would block the whole process and so
all our threads.  We instead perform the delay ourselves, by letting
the thread sleep.

Note that the pam module itsef can request some delay.  This has to be
taken care of in the PAM configuration file.  Here we use
\texttt{/etc/pam.d/muftpd}.  For example, the \verb+pam_unix+ module,
using the system password file, must be given the \texttt{nodelay}
option.

TODO: see pam\_ftp for anonymous access

 *)

let (>>=) = Muthr.(>>=)

open Pam

let do_login name pass k =
  let conv a b = match a with
  | Pam_Prompt_Echo_On -> name
  | Pam_Prompt_Echo_Off -> pass
  in
  let h = pam_start "muftpd" conv in
  pam_set_item h pam_item_fail_delay;
  let dir, uid =
    (try
      pam_authenticate h [] ~silent:false;
      let e = Unix.getpwnam name in
      e.Unix.pw_dir, e.Unix.pw_uid
    with _ -> "/tmp", -1)
  in
  pam_end h;
  if uid = -1 then Muthr.sleep 3. >>= fun () -> k (dir, uid)
  else k (dir, uid)

(*
let do_login name pass k =
  k ("/tmp/", 1000)
*)

let set_user id = 
  Euid.seteuid 0;
  Euid.setegid 0;
  Euid.setegid id;
  Euid.seteuid id




(*s Some utility functions *)

let my_ip s =
  let Unix.ADDR_INET(ip,p) = Unix.getsockname s in ip;;

let new_socket () =
  socket Unix.PF_INET Unix.SOCK_STREAM 0

let bind_any s port =
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_any, port))

(* Make a response with given code [c] and text [t].  Multi-line
responses are triggered by adding 1000 to code. *)

let make_resp c t =
  if c < 1000 then
    (string_of_int c) ^ (if t="" then " .." else " " ^ t) ^ ".\r\n"
  else
    let c = c - 1000 in
    let st = List.fold_left
	(fun st e -> st ^ (Printf.sprintf "%i-%s\r\n" c e))
	""
	(split t)
    in
    st ^ (Printf.sprintf "%i End.\r\n" c)

let send o c t k =
  let s = make_resp c t in
  debug ("< " ^ s);
  really_write o s 0 (String.length s) >>= k


(*i let protected_read i s off len k =
  trywith
    (fun () -> read i s off len k)
    (
i*)


open Unix

let print_dir_line name =

  let char_of_file_kind k =
    match k with
    | S_REG  -> '-'
    | S_DIR  -> 'd'
    | S_CHR  -> 'c'
    | S_BLK  -> 'b'
    | S_LNK  -> 'l'
    | S_FIFO -> 'p'
    | S_SOCK -> 's'
  in

  let string_of_file_perm p =
    let one p =
      (if p land 4 = 4 then "r" else "-") ^
      (if p land 2 = 2 then "w" else "-") ^
      (if p land 1 = 1 then "x" else "-")
    in
    one (p lsr 6) ^ one ((p land 0o77) lsr 3) ^ one (p land 0o7)
  in

  let st = Unix.lstat name
  in
  let user  = (getpwuid st.st_uid).pw_name in
  let group = (getgrgid st.st_gid).gr_name
  in
  Printf.sprintf "%c%s %3i %-8s %-8s %6i %s %s"
    (char_of_file_kind st.st_kind)
    (string_of_file_perm st.st_perm)
    st.st_nlink user group st.st_size "date" name

