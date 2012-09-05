(* vim: set shiftwidth=2 tabstop=2 expandtab: *)
open Muthr
open OUnit


let (@) f x   =  f x               (* Haskell's $ equivalent *)
let (=>) f x  =  fun _ -> f x      (* create thunk *)
let (=>=) f x =  fun _ -> f x; ()  (* create thunk returning unit *)
let (>>=) x f =  f x               (* Haskell's monadic operator for non-monadic operations.
                                      WARNING: in muthr.ml (>>=) is different *)
let (>>) x f  =  x (); f ()        (* another Haskell's monadic operator *)
let range a   =                    (* Python's range *)
  let rec rng acc y = if y > a then rng (y-1::acc) (y-1) else acc in
  rng []
let inc x     =  x := !x + 1

let fib_std = (* regular, ordinary, one-threaded Fibonacci *)
  let rec fib p pp i = if i > 0 then fib pp (p+pp) (i-1) else p in
  fib 0 1

let fibN = 15

let _check_fibf (rng_a : int) (rng_z : int) (fn : (int ref -> int -> unit)) =
  List.map
    (fun n ->
      let rf = ref 0 in
      fn rf => n >>= spawn;
      start ();
      assert_equal !rf @ fib_std n)
    (range rng_a rng_z)
let check_fibf = _check_fibf 1 fibN
    
exception TestException1

let tests = "Test suite" >::: [
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
  ("Fibonacci: spawn & raise exceptions (not catched)" >:: 
    let rec fib (xr : int ref) (n : int) =
      if n<2
      then raise TestException1
      else begin
        fib xr => (n-1) >>= spawn;
        fib xr => (n-2) >>= yield
      end in
    fib (ref 0) => fibN >>= spawn;
    start => ());
  ("Fibonacci: spawn & yield. Catch all exceptions (exc when <2)" >:: fun() -> 
    let excps = ref 0
    and finls = ref 0 in
    let rec fib (xr : int ref) (n : int) =
      if n<2
      then raise TestException1
      else begin
        trywith (fib xr => (n-1))
                (inc => excps)
                => (inc =>= finls)
          >>= spawn;
        fib xr => (n-2) >>= yield
      end in
    trywith (fib (ref 0) => fibN)
            (inc => excps)
            (inc =>= finls);
    start ();
    fibN + 1 >>= fib_std >>= assert_equal !finls;
    fibN + 1 >>= fib_std >>= assert_equal !excps
  );
  ("Fibonacci: spawn & yield. Catch all exceptions (exc when 0)" >:: fun() -> 
    let excps = ref 0
    and finls = ref 0 in
    let rec fib (xr : int ref) (n : int) =
      if n<2
      then if n=0 then raise TestException1 else (xr := !xr + n)
      else begin
        trywith (fib xr => (n-1))
                (inc => excps)
                => (inc =>= finls)
          >>= spawn;
        fib xr => (n-2) >>= yield
      end in
    trywith (fib (ref 0) => fibN)
            (inc => excps)
            (inc =>= finls);
    start ();
    fibN + 1 >>= fib_std >>= assert_equal !finls;
    fibN - 1 >>= fib_std >>= assert_equal !excps
  );
]

let _ = run_test_tt ~verbose:true tests
