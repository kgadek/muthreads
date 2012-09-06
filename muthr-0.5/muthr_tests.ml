(* vim: set shiftwidth=2 tabstop=2 expandtab: *)
open Muthr
open OUnit


let (@) f x    = f x              (* Haskell's $ equivalent *)
let (=>) f x   = fun _ -> f x     (* create thunk *)
let (=>=) f x  = fun _ -> f x; () (* create thunk returning unit *)
let (>>=) x f  = f x              (* Haskell's monadic operator for non-monadic operations.  WARNING: in muthr.ml (>>=) is different *)
let (>>) x f   = x (); f ()       (* another Haskell's monadic operator *)
let addrf v x  = x := !x + v
let inc        = addrf 1
let (=..) a    =                  (* Haskell's .. equivalent *)
  let rec rng acc y = if y >= a then rng (y::acc) (y-1) else acc in
  rng []
let compare_results (rng_a : int) (rng_z : int) (fnA : int -> int) (fnB : int ref -> int -> unit) =
  List.map (fun n ->
             let rf = ref 0 in
             spawn (fnB rf => n);
             start();
             assert_equal !rf @ fnA n)
           (rng_a =.. rng_z)

(* A000045 - Fibonacci sequence *)
let fibN = 15
let fib_std = (* regular, ordinary, one-threaded Fibonacci *)
  let rec fib p pp i = if i > 0 then fib pp (p+pp) (i-1) else p in
  fib 0 1
let check_fibf = compare_results 1 fibN fib_std

(* A001057 - sum positive integers w/ alternating signs *)
let altN = 25
let alt_std n =
  let sgnng v = if (v mod 2) = 1 then 1 else -1 in
  ((n+1) / 2 * (sgnng n))
let check_altf = compare_results 1 altN alt_std
    
exception TestException1

let tests = "Test suite" >::: [
  ("Fibonacci: spawn & yield" >:: fun () ->
   let rec fib (xr : int ref) (n : int) =
     if n<2
     then addrf n xr
     else begin
       spawn (fib xr => (n-1));
       yield (fib xr => (n-2))
     end in
   check_fibf fib; ());
  ("Fibonacci: spawn & calculate" >:: fun() ->
   let rec fib xr n =
     if n<2
     then addrf n xr
     else begin
       xr := !xr + (fib_std @ n-2);
       yield (fib xr => (n-1))
     end in
   check_fibf fib; ());
  ("Fibonacci: spawn & raise exceptions (not catched)" >:: fun() ->
    let rec errs = ref 0
    and fib (xr : int ref) (n : int) =
      if n<2
      then begin
        inc errs;
        raise TestException1
      end else begin
        spawn (fib xr => (n-1));
        yield (fib xr => (n-2))
      end in
    spawn (fib (ref 0) => fibN);
    start ();
    assert_equal !errs (fib_std @ fibN+1));
  ("Fibonacci: spawn & yield. Catch all exceptions (exc when <2)" >:: fun() -> 
    let rec excps = ref 0
    and finls = ref 0
    and fib (xr : int ref) (n : int) =
      if n<2
      then raise TestException1
      else begin
        trywith (fib xr  =>  (n-1))
                (inc     =>  excps)
                => (inc =>= finls)
          >>= spawn;
        yield (fib xr => (n-2))
      end in
    trywith (fib (ref 0) => fibN)
            (inc => excps)
            (inc =>= finls);
    start ();
    assert_equal !finls (fib_std @ fibN + 1);
    assert_equal !excps (fib_std @ fibN + 1));
  ("Fibonacci: spawn & yield. Catch all exceptions (exc when 0)" >:: fun() -> 
    let excps = ref 0
    and finls = ref 0 in
    let rec fib (xr : int ref) (n : int) =
      if n<2
      then if n=0 then raise TestException1 else (xr := !xr + n)
      else begin
        trywith (fib xr  =>  (n-1))
                (inc     =>  excps)
                => (inc =>= finls)
          >>= spawn;
        yield (fib xr => (n-2))
      end in
    trywith (fib (ref 0) => fibN)
            (inc => excps)
            (inc =>= finls);
    start ();
    assert_equal !finls (fib_std @ fibN + 1);
    assert_equal !excps (fib_std @ fibN - 1));
  ("A001057: calculate the naive way" >:: fun() ->
    let rec sgnng v = if (v mod 2) = 1 then 1 else -1
    and f res n =
      if n < 1
      then ()
      else begin
        addrf (sgnng n *n) res;
        yield (f res => (n-1))
      end in
    check_altf f; ());
]

let _ = run_test_tt ~verbose:true tests
