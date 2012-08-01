module Tracer = struct
  (*
    This module introduces some modal operators from temporal logic:
       * p >|> q   --  "until"; q holds at the current or a future position, and p has to hold until that position.
                       At that position p does not have to hold any more.
       * p >-> q   --  "release"; p releases q if q is true until the first position in which p is true (or forever
                       is such a position does not exist) 
       * p >/> q   --  "weak until"; equivalent to (p >|> q) || (>=> p) 
       * >*> p     --  "next"; p has to hold at the next state
       * >^> p     --  "future"; p eventually has to hold
       * >=> p     --  "globally"; p has to hold on the entire subsequent path


    To use, one creates a set of statements. Then a (multi-threaded) program runs, changes statements' values and
    leaves a trace. After this stage, we can check the trace with modal operators.


    Implementation note: in OCaml, as there's no concurrency, we're simply obtaining a "global clock";
    this suggests limiting maths to "linear temporal logic", but as this module could be used in distributed
    computing, we're not going that way for now.
  *)


  type trace             = snapshot list (* This is a reversed list of states. Each state is a snapshot of all statements' values. These *)
  and snapshot           = bool array    (* arrays should be of the same size, as each statement is actually pointing to one *)
  and temporal_statement = (snapshot -> bool, snapshot -> unit, snapshot -> unit)  (*  boolean in each of these arrays. *)
                                         (* Each statement is a triple of functions: reader, setter and unsetter. Note that one could
                                            write additional logic and pack those un-/setters into his own functions. *)

end