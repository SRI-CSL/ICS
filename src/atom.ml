
(*i*)
open Term
open Hashcons
(*i*)

let is_atom a =
  match a.node with
    | App({node=Pred _},_) -> true
    | _ -> false

let rec equal (a,b) =
  if a === b then
    mk_tt()
  else if is_const a && is_const b then
    mk_ff()
  else if is_equal a && is_tt b then
    equal (d_equal a)
  else if is_tt a && is_equal b then
    equal (d_equal b)
  else 
    Term.mk_equal (if a <<< b then (a,b) else (b,a))

let rec cnstrnt c a =
  if Cnstrnt.is_singleton c then
    equal (a, mk_num(Cnstrnt.d_singleton c))
  else
    match Cnstrnt.mem a c with
      | Cnstrnt.Yes -> mk_tt()
      | Cnstrnt.No -> mk_ff()
      | Cnstrnt.X -> mk_cnstrnt (Cnstrnt.inter (Cnstrnt.of_interp a) c) a

let rec diseq (a,b) =
  if a === b then
    mk_ff()
  else if is_const a && is_const b then
    mk_tt()
  else if is_equal a && is_tt b then
    diseq (d_equal a)
  else if is_tt a && is_equal b then
    diseq (d_equal b)
  else if is_num a then
    cnstrnt (Cnstrnt.diseq (d_num a)) b
  else if is_num b then
    cnstrnt (Cnstrnt.diseq (d_num b)) a
  else 
    Term.mk_diseq (if a <<< b then (a,b) else (b,a))

(*s Inconsistency check. Rather incomplete... *)

let inconsistent a b =         (*s Tests if equality [a = b] between atoms is inconsistent. *)
     (is_tt a && is_ff b)      (*s Only used for 'quick' detection of inconsistencies. *)
  || (is_ff a && is_tt b)
  || (is_equal a && is_equal b &&
      let (a1,a2) = d_equal a in
      let (b1,b2) = d_equal b in
      a1 === b1 && not(a2 === b2) && is_const b1 && is_const b2)
