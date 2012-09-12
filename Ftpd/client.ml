(*#load "unix.cma";;
#load "muthr.cmo";;*)

open Muthr


let wait_forever () =
  let mv = make_mvar ()
  in
  take_mvar mv >>= nothing

    
let timeout_at t op exp nor =
  let t1 = Unix.gettimeofday ()
  in
  timeout (t -. t1)
    op exp nor

(* erreur subtile avec fun () -> c (); wait_forever ()) pour le test2 !
   voir système de type-monades pour détecter ce type d'erreur ?
*)

(* cette version compense le retard pris par le thread en attente
d'éxécution, en utilisant une date absolue -> garantit la fréquence
(sauf si saturation... *)

(* TODO: check if context grows at each iteration... *)

let doevery d c =
  let rec loop t c =
    timeout_at t
      (fun () -> c >>= wait_forever)
      (fun () -> loop (t+.d) c)
      nothing
  in
  loop (Unix.gettimeofday () +. d) c

let test d f =
  let t () = doevery d f
  in
  spawn t;
  start ()


let test1 () = test 2. (fun k -> print_string "toto\n"; k ())


let test2 () =
  test 2.
    (fun k ->
      sleep 1. >>= fun () ->
      print_string "tata\n"; k ())

let test3 () =
  let t () =
    doevery 2. (fun k -> print_string "titi\n"; k ())
  in
  (* this thread creates delay in the system *)
  let rec p () = for i=1 to 10000000 do () done; sleep 1. >>= p
  in
  spawn t;
  spawn p;
  start ()

(* ---- *)

let dbg s = print_string s; print_newline (); flush stdout

let buf = String.create 255

let get_answer s k =
  read s buf 0 255 >>= fun l ->
  k (String.sub buf 0 l)

let answ s k =
  get_answer s >>= fun r ->
  print_string r;
  k ()


let send s cmd k =
  write s cmd 0 (String.length cmd) >>= fun l ->
  Printf.printf "Sent %s (%i chars).\n" cmd l;
  k ()


let sr s cmd k =
  send s cmd >>= fun () -> answ s k


let new_socket () =
  socket Unix.PF_INET Unix.SOCK_STREAM 0

let login host port user pass k ()  =
  let s = new_socket ()
  in
  let ip = (Unix.gethostbyname host).Unix.h_addr_list.(0)
  in
  connect s (Unix.ADDR_INET(ip, port)) >>= fun () ->
  answ s >>= fun () ->
  sr s ("USER " ^ user ^ "\n") >>= fun () ->
  sr s ("PASS " ^ pass ^ "\n") >>=
  k s
;;

let parse_pasv s =
  if String.sub s 0 3 = "227" then
    let arg =
      let beg = String.index s '(' + 1
      in let fin = String.index s ')' - 1
      in String.sub s beg (fin-beg+1)
    in
    Scanf.sscanf arg "%s@,%s@,%s@,%s@,%i,%i" 
      (fun i1 i2 i3 i4 p1 p2 ->
	Unix.ADDR_INET(
	Unix.inet_addr_of_string (i1 ^ "." ^ i2 ^ "." ^ i3 ^ "." ^ i4),
	p1*256 + p2))
  else
    failwith "Not a 227"

let rec read_all sock action k =
  let buf = String.create 1500 in
  dbg "read_all";
  read sock buf 0 1500 >>= fun l ->
  if l = 0 then k ()
  else begin (* ZZZ emacs indents as block even if no begin end !!! *)
    action (String.sub buf 0 l);
    read_all sock action k
  end


let list s k =
  send s "PASV\n" >>= fun () ->
  get_answer s >>= fun r ->
  send s "LIST\n" >>= fun () ->
  let sock = new_socket () in
  connect sock (parse_pasv r) >>= fun () ->
  answ s >>= fun () ->
  read_all sock print_string >>= fun () ->
      (* le close fait planter muthr select avec EBADF ??? 
	 = le fd est fermé mais présent dans rfd
	     pourtant close le retire

         si pas close, on a lost rfd ... après un grand nombre de read_all 
	 supplémentaires
         = le fd est présent dans rfd mais readers à NoRead
	     mis à NoRead quand la lecture est faite.

	 cause: read_all s'appelle après le else
	 si close, on a un read fait sur un fd fermé -> EBADF
	 si pas close, ???
	 read_all boucle sur un fd qui est en EOF
 *)
  close sock;
  answ s >>= k
;;


for i = 1 to 1 do
  spawn (login "localhost" 3000 "guest" "invité+" 
	   (fun s -> fun () -> 
	     sleep 1.5 >>= fun () ->
	     list s >>= fun () ->
	     close s))

(*	   (fun s -> fun () -> doevery 1. (send s "PWD\n")));*)
done;

start ()
