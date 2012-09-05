module Tracer = struct

  type word        = letter array
  and letter       = bool
  and pointed_word = word * int
  and formula      = (pointed_word -> bool)


  (* helpers: quantificators and bool operators =================================================*)
  let rec forall f a b = (* true iff for all i=[a,b) holds (f i) *)
    if a>=b then true
    else (if (f a) then forall f (a+1) b else false)

  let rec exists f a b = (* true iff there exists i=[a,b) for which (f i) *)
    if a>=b then false
    else (if (f a) then true else exists f (a+1) b)

  let (=>) a b =
    (not a) or b



  (***********************************************************************************************
   ***********************************************************************************************
   ***********************************************************************************************)


  let (|=) ((pi,i) : pointed_word) (phi : formula) =
    phi (pi,i) (*bug if i>len pi; should ret. false then*)


  (* base =======================================================================================*)
  let (?=) ((pi, i) : pointed_word)    = pi.(i)
  let f_true ((pi, i) : pointed_word)  = true
  let f_false ((pi, i) : pointed_word) = false

  (* future and past modalities =================================================================*)
  let f_not (p : formula) =
    ((fun (pi,i) -> not (p (pi,i))
     ) : formula )

  let next (p : formula) =
    ((fun (pi,i) -> (pi, i+1) |= p
     ) : formula )

  let weak_next p =
    f_not (next (f_not p))

  let previously p =
    ((fun (pi,i) -> 0 < i && (pi,i-1) |= p
     ) : formula )

  let weak_previously p =
    f_not (previously (f_not p))

  let initially p =
    ((fun (pi,i) -> (pi,0) |= p
     ) : formula )

  let now p =
  ((fun (pi,i) -> let pi_i = Array.sub pi 0 i in
                    (pi_i, 0) |= p
     ) : formula )

  let f_and (p : formula) (q : formula) =
    ((fun (pi,i) -> if p (pi,i) then q (pi,i) else false
     ) : formula )

  let f_or (p : formula) (q : formula) =
    ((fun (pi,i) -> if p (pi,i) then true else q (pi,i)
     ) : formula )

  let until (p : formula) (q : formula) =
    ((fun (pi,i) -> exists (fun k -> (pi,k) |= q
                                       &&
                                     forall (fun j -> (pi,j) |= p) i k
                           ) i (Array.length pi)
     ) : formula)

  let strict_until p q =
    next (until p q)

  let weak_until p q =
    f_or (until p q) (f_not (until f_true (f_not p)))

  let release p q =
    f_not (until (f_not p) (f_not q))

  let aslongas p q =
    until q (f_not p)

  let before p q =
    release p (f_not q)

  let strict_before = strict_until

  let first_time p q =
    f_or (strict_until (f_not q) (f_and p q)) (f_not (strict_until f_true q))

  let since (p : formula) (q : formula) =
    ((fun (pi,i) -> exists (fun k -> (pi,k) |= q
                                       &&
                                     forall (fun j -> (pi,j) |= p
                                     ) (k+1) (i+1)
                           ) 0 i
     ) : formula )

  let strict_since p q =
    previously (since p q)

  let weak_since p q =
    f_or (since p q) (f_not (since f_true (f_not p)))

  let chop p q =
    ((fun (pi,i) -> exists (fun k -> let pi_ok = Array.sub pi 0 k
                                     and pi_k = Array.sub pi k (Array.length pi - k) in
                                       (pi_k,0) |= q
                                         &&
                                       (i<k => ((pi_ok,i) |= p))
                           ) i (Array.length pi)
     ) : formula )

  let sometimeinpast = since f_true

  let alwaysinpast p =
    f_not (sometimeinpast (f_not p))

  let strict_sometimeinpast = strict_since f_true

  let strict_alwaysinpast p =
    f_not (strict_sometimeinpast (f_not p))

  let eventually = until f_true

  let always p =
    f_not (eventually (f_not p))

  let strict_eventually = strict_until f_true

  let strict_always p =
    f_not (strict_eventually (f_not p))

end
