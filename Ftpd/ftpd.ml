(*p

\usepackage{a4wide}
*)

(*s FTP server with user-level muthreads for OCaml -- Christophe Deleuze *)

open Muthr
open Data
open Parse
open Aux

let date = "Jan, 2010"
let version = "0.6"
let msg_greeting = 
  "Welcome to muthreads ftp server (v " ^ version ^ ", " ^ date ^ ")"

let msg_help = "Local server help\n...\n...\nThat's all\n"

let msg_quit nb tot =
  Printf.sprintf "You have transfered %i bytes in %i transactions\n%s\n" 
    tot nb "Come back later!"

let buf_size = 1500

(* How long we wait for a passive data connection to be set up *)
let passive_wait = 5.  

(* Control connection idle for more than that will be closed (unless
   transfer is ongoing) *)
let control_idle = 900.

let cport = ref 21      (* PI port *)
let dport = ref 2999    (* DTP port -- use ephemeral if fail *)

let umask = 0o644

let initial_dir = "/tmp"

(* Wait for a command on the socket [i] (which should be the one for
the control connection) and pass the parsed command to the
continuation.  If none arrives during [control_idle] seconds, close
the connection (and the thread terminates). *)

let recv i k =
  let s = String.create 512 in
  timeoutk control_idle
    (fun k () -> read i s 0 512 k)
    (fun () ->
      send i 421 "Timeout: closing control connection" >>= fun () ->
      close i)
    (fun l ->
      if l=0 then k END else
      let s = String.sub s 0 l in
      let m = parse_cmd s in
      debug ("> " ^ s); k m)

let take_or_recv mv i kr kmv =
  let s = String.create 512 in
  read_or_take mv i s 0 512 
    (fun l -> if l=0 then kr END else
    let s = String.sub s 0 l in
    let m = parse_cmd s in
    debug ("> " ^ s); kr m)
    kmv


(*s DTP thread: data *)

type dtr_kind =             (* kind of data transfer performed by DTP *)
  | Nlst of string option
  | List of string option
  | Retr of int * string  (* byte count for restart *)
  | Stor of int * string
  | Stou of string
  | Appe of string
  | Cancel

let show_dtr dtr =
  match dtr with
  | Nlst None -> "Listing (NLST)", ""
  | Nlst (Some d) -> "Listing (NSLT)", d
  | List None -> "Listing (LIST)", ""
  | List (Some d) -> "Listing (LIST)", d
  | Stor (_, f) -> "Uploading (STOR)", f
  | Stou f      -> "Uploadind (STOU)", f
  | Appe f      -> "Uploading (APPE)", f
  | Retr (_, f) -> "Downloading (RETR)", f


(*s DTP thread: a few auxiliary functions *)

(* Upload (ie store locally) from [skt] to file descriptor [f]
   receiving with [do_read] (which takes care of TYPE conversion).
   Count bytes with [cnt].  *)

let do_upload do_read skt f cnt k =
  let s = String.create buf_size
  in
  let rec loop k =
    do_read s 0 buf_size
      (* performing transfer *)
      (fun l ->
	if l>0 then begin
	  output f s 0 l;
	  cnt := !cnt + l;
	  loop k
	end else begin
	  close_out f; close skt; k 226
	end)
      (* aborting if value received on abort mvar *)
      (fun () -> close_out f; close skt; k 426)
  in
  loop >>= k

(* Call [do_upload] with the appropriate [do_read] function (taking
   care of TYPE conversion and of abort signals) and the file
   descriptor obtained from [do_open]. *)

let gupload typ do_open skt cnt abt res =
  let do_read = 
    if typ = T_A then
      fun s off len kread kabt -> read_or_take abt skt s off len 
	  (fun l -> let _, l = of_ascii s l in kread l) 
	  kabt
    else
      fun s off len kread kabt -> read_or_take abt skt s off len kread kabt
  in
  try
    let f = do_open () in
    do_upload do_read skt f cnt >>= fun r ->
    put_mvar res r >>= nothing
  with _ -> close skt; put_mvar res 550 >>= nothing

(* Perform download from file descriptor [f] to socket [skt] using
   [typ] TYPE, counting in [cnt], aborting if value in
   [abt].  [do_write] convert to ASCII type if needed. *)

let do_download typ skt f cnt abt k = (* TODO: first read in whole file ?*)
  let s = String.create buf_size
  in
  let do_write =
    if typ=T_A then
      fun s l kw kt -> let s,l = to_ascii s l in
      write_or_take abt skt s 0 l kw kt
    else
      fun s l kw kt -> write_or_take abt skt s 0 l kw kt
  in
  let rec loop k =
    let l = Unix.read f s 0 buf_size in
    if l>0 then
      do_write s l (* if partial write, will fail ZZZ *)
	(fun l -> cnt := !cnt + l; loop k)
	(fun () -> Unix.close f; close skt; k 426)
    else begin
      Unix.close f; close skt; k 226
    end
  in
  loop >>= k


let bad_do_download typ skt f cnt abt k = (* TODO: first read in whole file ?*)
  let s = String.create buf_size
  in
  let do_write =
    if typ=T_A then
      fun s p l kw kt -> let s,l = to_ascii s l in
      write_or_take abt skt s p l kw kt
    else
      fun s p l kw kt -> write_or_take abt skt s p l kw kt
  in
  let rec loop_buf p n kcont kfail =
    if p < n then
      do_write s p (n-p)
	(fun l -> cnt := !cnt + l; loop_buf (p+l) n kcont kfail)
	(fun () -> Unix.close f; close skt; kfail 426)
    else skip >>= kcont
  in
  let rec loop_file k =
    let l = Unix.read f s 0 buf_size in
    if l>0 then
      loop_buf 0 l (fun () -> loop_file k) k
    else begin
      Unix.close f; close skt; k 226
    end
  in
  loop_file >>= k

(* ZZZ trampoline empêche Array.iter write ? *)

let close_and_put skt mv code () = (* rewrite with reallywriteortake *)
  put_mvar mv code >>= fun () ->
  close skt

(* Build a string for the LIST command. *)

let get_list typ arg =
  let file_list = match arg with
  | None   -> Sys.readdir "."
  | Some d -> 
      if Sys.is_directory d then begin
	Unix.chdir d; Sys.readdir "."  (* change dir for stat *)
      end else [| d |]
  in
  let sep = if typ=T_A then "\r\n" else "\n" in
  Array.fold_left 
    (fun st f -> 
      st ^ print_dir_line f ^  sep)
    "" 
    file_list

(*s DTP thread: the transfer functions themselves. *)

(* [upload], [append], [download], [list], and [nlst]. The
differences between [upload] and [append] are in the way the file is
open, plus we don't accept REST for append and so don't perform any
[seek_out] on the file (RFC3659). *)

let upload typ name c skt cnt abt res =
  let do_open = fun () -> 
    let f = open_out_gen [ Open_wronly; Open_creat ] umask name
    in seek_out f c; f
  in 
  gupload typ do_open skt cnt abt res

let append typ name skt cnt abt res =
  let do_open = 
    fun () -> open_out_gen [ Open_append; Open_creat ] umask name
  in
  gupload typ do_open skt cnt abt res

let download typ name c skt cnt abt res =
  try
    let f = Unix.openfile name [ Unix.O_RDONLY ] 0
    in
    Unix.lseek f c Unix.SEEK_SET;
    do_download typ skt f cnt abt >>= fun r ->
    put_mvar res r >>= nothing
  with 
    Unix.Unix_error(Unix.ENOENT, "open", _) (* ZZZ ou autre ? *)
   -> close skt; put_mvar res 550 >>= nothing

let nlst typ s abt res =
  let sep = if typ=T_A then "\r\n" else "\n" in
  let st = Array.fold_left 
      (fun st f -> st ^ f ^ sep) 
      "" 
      (Sys.readdir ".")
  in
  really_write_or_take abt s st 0 (String.length st)
    (close_and_put s res 226)
    (close_and_put s res 426)
    
let list typ skt arg abt res =
  let st = get_list typ arg
  in
  really_write_or_take abt skt st 0 (String.length st)
    (close_and_put skt res 226)
    (close_and_put skt res 426)

(*s DTP thread: main function *)

(* Once the connection is set up and the kind of transfer known, we
   dispatch to the appropriate function according to [dtr].  We first
   set our effective uid to that of the user so that the file opening
   is made under the correct user id. Once the file is open, the euid
   is of no importance, so we don't need to call that continuously
   during a file transfer.  The same applies for the current
   directory.  *)

let dtp_switch (id,dir) typ dtr s abt res cnt =
  set_user id;
  Unix.chdir dir;
  match dtr with
  | Retr(c,name) -> download typ name c s cnt abt res
  | Stor(c,name) -> upload   typ name c s cnt abt res
  | Stou name    -> upload   typ (unique name) 0 s cnt abt res
  | Appe name    -> append   typ name   s cnt abt res
  | List arg ->
      (try list typ s arg abt res with _ -> 
	put_mvar res 550 >>= fun () -> close s)

  | Nlst None     -> nlst typ s abt res
  | Nlst (Some d) -> 
      try Unix.chdir d; nlst typ s abt res with _ ->
      put_mvar res 550 >>= fun () -> close s


(* DTP thread for passive mode. Start data transfer type [dtr] given
   in [go] (it can also ask for cancellation).  When done, put
   response code in [res].  Note that the thread can also be cancelled
   while blocked in accept, through [abt]. [con] is used to inform the
   PI that the data connection has been setup. *)

let dtp_passive user typ s abt go res con cnt () =
  accept_or_take abt s
    (fun (s', _) ->
      close s;
      put_mvar con () >>= fun () ->
      take_mvar go >>= fun dtr ->
      if dtr <> Cancel then
	trywith
	  (fun () -> dtp_switch user typ dtr s' abt res cnt)
	  (fun _  -> put_mvar res 425 >>= nothing)
	  nothing
      else (close s'; debug "cancel!"))
    (fun () -> close s; debug "cancel accept")

(* DTP thread for active mode.  Create connection and start [dtr]
   transfer.  When done, put response code in [res]. We try to bind
   on [!dport], or use an ephemeral port as a fallback. *)

let dtp_active user typ dtr ip p abt res cnt () =
  let s = new_socket () in
  (try
    bind_any s !dport
  with Unix.Unix_error _ -> 
    bind_any s 0);
  trywith
    (fun () -> 
      connect s (Unix.ADDR_INET(ip, p)) >>= fun () ->
      dtp_switch user typ dtr s abt res cnt)
    (fun (Unix.Unix_error _) -> put_mvar res 425 >>= nothing)
    nothing


(*s PI thread *)

(* The PI (protocol interpreter) manages the control connection with
the client, receives commands and sends replies.  It spawns and
controls a DTP when a data transfer is required. *)

type port_info =  (* parameter of PI preparing transfer phase *)
  | Active  of Unix.inet_addr * int
  | Passive of unit mvar * dtr_kind mvar * unit mvar * int mvar

(* The control connection can be closed unexpectedly at any moment,
   we'll call this function when this happens.  *)

let do_end skt = log "Control connection closed by peer"; close skt

let pi skt () =

(* The state of the protocol interpreter.  This state is private to
each PI thread, but the process current directory and the effective
user id are not.  Thus, we have to be careful with directories and
user ids, since the current directory and euid may change between
cooperation points because of other PI threads.  To get around that
we'll have to make a [Unix.chdir !cdir] and a [set_user] sometimes.
That is, each time we're about to access the filesystem (and in
particular at the beginning of a DTP).
*)

  let cdir = ref initial_dir  (* current directory *)
  and typ  = ref T_A          (* transfer TYPE *)
  and uid  = ref (-1)         (* user ID *)
  in

  let cnt   = ref 0           (* data transfer stats *)
  and nb_tr = ref 0
  and bytes = ref 0
  in

  let rep c k = send skt c "" k
  in

  (*s PI phases 1 and 2 -- Greeting, login *)

  let rec greeting () =
    cdir := initial_dir;
    typ  := T_A;
    Unix.chdir !cdir;
    send skt 220 msg_greeting >>=
    login

  and login () =
    recv skt >>= fun m -> match m with
    | USER user -> 
	rep 331  >>= fun () ->
        recv skt >>= fun m -> (match m with
	| PASS pass -> 
	    do_login user pass >>= fun (dir, id) ->
	    if id <> -1 then begin
	      uid := id;
	      cdir := dir;
	      rep 230 >>= inter
	    end else
	      rep 530 >>= login

	| END -> do_end skt

	| _   -> rep 530 >>= login)

    | QUIT -> rep 221 >>= fun () -> close skt

    | END -> do_end skt

    | _ -> rep 503 >>= login

  (*s PI phase 3 -- Interaction loop *)

  and inter () =

    recv skt >>= fun m -> 
    set_user !uid;
    Unix.chdir !cdir;
    match m with

(* We don't accept a data transfer command here but only just after a
   successful PORT or PASV command... *)

    | LIST _ | NLST _ | RETR _
    | STOR _ | STOU _ | APPE _ -> rep 500 >>= inter

(* ... that will bring us to the [preparing_transfer] function. *)

    | PORT (ip, p) -> rep 200 >>= preparing_transfer (Active(ip,p)) 0
    
    | PASV -> 
	let go  = make_mvar () in
	let res = make_mvar () in
	let abt = make_mvar () in
	let con = make_mvar () in

(* Handling passive data connections is made in two steps.  When
   receiving the PASV command, we create a socket, spawn a
   [dtp_passive] thread listening on it, then reply a 227 with the
   socket information.  *)

	let s = new_socket () in
	bind_any s 0;
	let Unix.ADDR_INET(_,p) = Unix.getsockname s in
	let ip = my_ip skt in
	Unix.listen s 1024;
	spawn (dtp_passive (!uid,!cdir) !typ s abt go res con cnt);
	send skt 227 (make_pasv_text ip p) >>=
	preparing_transfer (Passive(abt,go,con,res)) 0
	  
    | RNFR oldname ->
	if not (Sys.file_exists oldname) then
	  rep 550 >>= inter
	else
	  rep 350  >>= fun () ->
	  recv skt >>= fun m -> let r = match m with
	  | RNTO newname -> 
	      (try 
		set_user !uid;
		Unix.chdir !cdir; Sys.rename oldname newname; 250
	      with _ -> 550)
	  | _ -> 503
	  in rep r >>= inter
	      
    | RNTO name -> rep 503 >>= inter

    | REIN -> greeting ()

    | QUIT -> 
	send skt 1221 (msg_quit !nb_tr !bytes) >>= fun () ->
	close skt

    | END -> do_end skt

    | _ -> let c, t = match m with

(* Now all the simple commands that can be processed atomically. *)
	  
      | NOOP   -> 200, "NOOP completed successfully!"
      | ALLO _ -> 200, "We don't need to do that here"
      | ACCT _ -> 202, "There's no account here"
      | SITE _ -> 202, ""
      | HELP _ -> 1214, msg_help

      | SMNT _ -> 202, "We don't support that"
      | SYST   -> 215, "Unix / OCaml with muthreads"
      | PWD    -> 257, Printf.sprintf "\"%s\" is current directory" !cdir
      | CWD dir -> 
	  (try Unix.chdir dir; cdir := Unix.getcwd (); 250,"" 
	  with Unix.Unix_error _ -> 550,"")
	    
      | MKD dir -> 
	  (try Unix.mkdir dir 0o770; 
	    257, Printf.sprintf "\"%s/%s\" directory created" !cdir dir
	  with Unix.Unix_error _ -> 521,"")
	    
      | RMD dir ->
	  (try Unix.rmdir dir;  250,"" with Unix.Unix_error _ -> 550,"")
	    
      | CDUP    -> 
	  (try Unix.chdir ".."; cdir := Unix.getcwd (); 250,"" 
 	  with Unix.Unix_error _ -> 550,"")
	    
      | DELE n  -> (try Sys.remove n; 250,"" with Sys_error _ -> 550,"")
	    
      | STAT None  -> 1211, Printf.sprintf 
              "muftpd server v %s - %s\nTYPE %s STRUCT F MODE S\n%s\n"
  version date (if !typ=T_A then "A" else "I") "No data connection"

      | STAT (Some f) ->
	  (try 1213, "Status of " ^ f ^ "\r\n" ^ (get_list T_I (Some f))
	  with Sys_error _ -> 550, "Unknown file")

      | TYPE T_A   -> typ := T_A; 200, "Type set to Ascii"
      | TYPE T_I
      | TYPE (T_L 8) -> typ := T_I; 200, "Type set to Image"
      | TYPE T_bad -> 501, "Unknown type"
      | TYPE _     -> 504, "Unsupported type"

      | STRU S_F   -> 200, "Struct set to File"
      | STRU S_bad -> 501, "Unknown struct"
      | STRU _     -> 504, "Unsupported struct"
	    
      | MODE M_S   -> 200, "Mode set to Stream"
      | MODE M_bad -> 501, "Unknown mode"
      | MODE _     -> 504, "Unsupported mode"
	    
      | ABOR -> 226, "Pa ni pwoblem, nothing to abort"
      | BAD  -> 500, "Bad command"
	      
      | _    -> 400, "Not applicable now"
    in
    send skt c t >>= inter

  (*s PI phase 4 -- Preparing data transfer  *)

(* We wait for a data transfer command.  If so, we pass information
   to the DTP and go to [controlling_transfer].  Otherwise, we [reject]
   the command (and we have to cancel the passive DTP thread if any).
   [c] is the bytecount given by a REST command (it is 0 when we enter
   here).  We accept REST only for RETR and STOR.  *)

  and preparing_transfer port c () =

    let reject k =
      match port with 
      | Passive(abt, go, _, _) ->
	  put_mvar abt ()    >>= fun () ->
	  put_mvar go Cancel >>= fun () ->
	  rep 400 >>= k

      | _ -> rep 400 >>= k
    in
	
    recv skt >>= fun m -> 
    set_user !uid;
    Unix.chdir !cdir;
    match m with

    | REST c -> rep 350 >>= preparing_transfer port c
   
    | LIST name | NLST name
    | STOU name
    | APPE name when c<>0 -> reject >>= inter

    | LIST name | NLST name | RETR name
    | STOU name | STOR name | APPE name ->
        begin
	let dtr = match m with
	| LIST _ -> List (if name="" then None else Some (name))
	| NLST _ -> Nlst (if name="" then None else Some (name))
	| RETR _ -> Retr (c, name)
	| STOR _ -> Stor (c, name)
	| STOU _ -> Stou (name)
	| APPE _ -> Appe (name)
	in
	match port with

(* Handling active mode data connections is easy.  We have been passed
the IP address and port on the [port] parameter. Now that the transfer
command is received, we spawn a [dtp_active] thread to which we pass
the data transfer type, the IP address and port plus an [abt] mvar
used as an abort signal and a [res] mvar for the thread to provide its
result.  [cnt] is an [int ref] in which the DTP will constantly write
the number of bytes processed so far.  We immediately reply a 150
(``About to open data connection'').  *)

	| Active(ip,p) -> 
	    let res = make_mvar () in
	    let abt = make_mvar ()
	    in 
	    spawn (dtp_active (!uid,!cdir) !typ dtr ip p abt res cnt); 
	    rep 150 >>= 
            controlling_transfer dtr abt res

(* For passive mode, the thread has already been spawn.  It puts [()]
to the [con] mvar after connection establishment, then waits for the
kind of transfer to perform.  Now is the time to tell it (put [dtr] in
its [go] mvar), and reply to the command with a 125.  If the
connection is not established during the specified delay, we cancel
the transfer.  [abt] gets the DTP out of the accept.  We also put
[Cancel] in [go] to avoid race conditions (in case the DTP thread has
been scheduled to perform the accept but is now waiting for the
scheduler to run it).\footnote{Oh my, even with cooperative threads we
can have race conditions!} *)

	| Passive(abt,go,con,res) -> 
	    timeout passive_wait
              (fun () -> 
		take_mvar con      >>= fun () -> 
		rep 125            >>= fun () ->
		put_mvar go dtr    >>= nothing)
	      (fun () ->
		rep 425            >>= fun () ->
		put_mvar abt ()    >>= fun () ->
		put_mvar go Cancel >>=
		inter)
	      (controlling_transfer dtr abt res)
    end

(* Reject commands other than data transfer. *)

    | END -> do_end skt

    | _ -> reject >>= inter

(*s PI phase 5 -- Controlling data transfer  *)

  and controlling_transfer dtr abt res () =

(* When a transfer is being performed, the PI should accept only a
   few commands: STAT inquires for the current status of the transfer,
   ABOR asks for aborting it, QUIT is to be deferred until the end of
   the transfer.

   As we saw, the DTP can be aborted through an [abt] mvar.  RFC959
   says that when a ABOR is performed, a first reply (426) should be
   sent for indicating the connection closing, followed by a 226
   acknowledging the success of the abort operation... 

   ``An unexpected close on the control connection will cause the
   server to take the effective action of an abort (ABOR) and a logout
   (QUIT)''.  *)

      let rec loop () =
	take_or_recv res skt
	  (fun m ->                    (* cmd received by PI *)
	    match m with
	    | STAT _ -> 
		let kind, f = show_dtr dtr in
		let txt = 
                  Printf.sprintf "%s of %s in %s mode (%i bytes)\n"
                    kind f (if !typ=T_A then "ASCII" else "image") !cnt 
		in
		send skt 1211 txt >>= 
                loop

	    | ABOR   -> 
		put_mvar abt () >>= fun () -> 
		take_mvar res   >>= fun r ->
		rep r           >>= fun () -> (* from DTP *)
		rep 226         >>=           (* from PI: successful abort *)
		update_stats dtr

            | END    -> 
		put_mvar abt () >>= fun () ->
		do_end skt

	    | _ -> rep 500 >>= loop)
	  
	  (fun r -> rep r >>= update_stats dtr)       (* DTP is done *)
      in
      loop ()

(*s PI phase 6 -- Data transfer done, update stats and go back to [inter] *)

  and update_stats dtr () =
    begin
      match dtr with
      | Retr _
      | Stor _
      | Stou _
      | Appe _ -> incr nb_tr
      | _ -> ()
    end;
    bytes := !bytes + !cnt;
    cnt := 0;
    inter ()

  in
  greeting ()
    
(*s Main thread *)

let rec main s () =
  accept s >>= fun (inp, _) ->
  spawn (pi inp);
  main s ()

let init () =
  let socket = new_socket () in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  bind_any socket !cport;
  Unix.listen socket 1024;
  spawn (main socket);
  start ()
;;

init ()





(*i + RFC1123


    let create_active_dtp what ip p k =
      let mv = make_mvar () in
      let ab = make_mvar ()
      in 
      let rec loop () =
	take_or_recv mv skt
	  (fun m ->   
	    match m with
	    | STAT _ -> send skt 211 "Ongoing transfer" >>= loop
	    | ABOR   -> 
		put_mvar abt () >>= fun () -> 
		take_mvar mv   >>= fun v ->
		rep v          >>= fun () -> 
		rep 226        >>= k

	    | _ -> rep 500 >>= loop)
	  (fun v -> rep v >>= k)    
      in
      spawn (dtp_active !typ what ip p abt mv); 
      rep 150 >>=
      loop
    in

i*)

(*s TODO *)

(*

ASCII TYPE conversion rather inefficient

PAM fait planter si usage intensif (client.ml)

ls - manque juste la date

parse plus exigeant des commandes (espace) + vérifier pas d'arg

really write or take, comment faire ? -> A VOIR !!!

gestion des double quote dans les noms de répertoire ?

parsing plus strict de ALLO OK ?

NLST sur pattern - RFC indique de faire sur le client

timeout sur la connexion de contrôle
sur recv ?

stat login name, combien connexions de contrôle et de données
+ stats sur les transferts effectués

compter le trafic des LIST et NLST

+ ne répond pas aux infos de controle (DTP ne laisse jamais la main ?)
-> changé read dans muthr... ZZZ
si pas en local ne se produira pas ?

cnt ne tient pas compte du mode ASCII...

LIST ne donne pas les répertoires "." et ".."

STOU : rep 250 contient nom généré (125 et 226 ?)
DTP renvoie code *et* texte dans mvar res ?
ou, déplacer les ouvertures de fichiers dans le PI ?

pendant transfert ABOR et STAT peuvent être précédés de "Telnet IP" et
"Telnet Synch" pour "attirer l'attention du serveur"...  Si ce n'est
pas nécessaire, cela ne doit pas avoir "unusual effect".

rfc854 (telnet) IP = 244 synch = TCP Urgent + Telnet Data Mark Data
mark = 242 (netkit envoie ff f4 ff f2 , ff = IAC qui précède toute
commande) 

QUIT peut être envoyé pendant transfert (rfc959 p 34)
réponse et fermeture à fin de transfert (p 27)

définir recv dans le PI (pour éliminer l'arg socket) ?

HELP SITE

SITE

----

OK timeout sur la control connection (sauf pendant transfert)

OK dans printdirline remplacé Unix.stat par Unix.lstat
   lien symbolique cassé fait échouer LIST (mais pas NLST)

OK delai de l'authentification PAM
   -> annuler delai par pamsetitem
   -> annuler delai de pamunix (fichier /etc/pam.d/muftpd)

OK END pendant login + preparing transfer...

OK changé read or take dans muthr.ml -- gros soucis dans controling
  transfer : on ne veut pas rater le retour à cause d'une commande
  reçue à ce moment là !

OK RESTart - accepté uniquement sur RETR et STOR

OK STOU

OK STAT sur fichier -> idem LIST mais sur connexion de contrôle

OK PWD, CWD, MKD -> donne dir absolu (appendix II) + 521 ?

OK - ASCII mode pour download (mais besoin optimiser ?)

OK - utiliser really\_write

OK - passive mode, wait some time for connection

OK - cancel passive DTP when blocked on accept

*)
