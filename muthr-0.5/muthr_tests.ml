open Muthr
open OUnit


let (@) f x = f x (* Haskell's $ equivalent *)
let (=>) f x = fun () -> f x (* create thunk *)
let (=>=) f x = fun () -> f x; () (* create thunk returning unit *)
let (>>=) x f = f x (* Haskell's monadic operator for not-monadic operations
                       WARNING: its behaviour is different in muthr library *)
let (>>) x f = x (); f (* another Haskell's monadic operator *)

let range a = (* Python's range *)
  let rec rng acc y = if y > a then rng (y-1::acc) (y-1) else acc in
  rng []

let fib_std = (* regular, ordinary, one-threaded Fibonacci *)
  let rec fib p pp i = if i > 0 then fib pp (p+pp) (i-1) else p in
  fib 0 1

let _check_fibf (rng_a : int) (rng_z : int) (fn : (int ref -> int -> unit)) =
  List.map
    (fun n ->
      let rf = ref 0 in
      fn rf => n >>= spawn;
      start ();
      assert_equal !rf @ fib_std n)
    (range rng_a rng_z)
let check_fibf = _check_fibf 1 20
    
let tests = "Tests w/ Fibonacci" >::: [
  ("Fibonacci: spawn & yield" >::
   let rec fib (xr : int ref) (n : int) =
     if n<2
     then (xr := !xr + n)
     else begin
       fib xr => (n-1) >>= spawn;
       fib xr => (n-2) >>= yield
     end in
   check_fibf =>= fib);
  ("Fibonacci: spawn & calculate" >::
   let rec fib xr n =
     if n<2
     then (xr := !xr + n)
     else begin
       xr := !xr + (fib_std @ n-2);
       fib xr => (n-1) >>= yield
     end in
   check_fibf =>= fib);
]

let _ = run_test_tt ~verbose:true tests
