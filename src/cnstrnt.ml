
(*i*)
open Term
open Hashcons
open Mpa
(*i*)

type t = Interval.t
  
    (*s Computing the best constraint, given a context of constraint declarations. *)

let cnstrnt ctxt a =
  let rec cnstrnt_of_term a =
    try
      ctxt a
    with
	Not_found ->
	  (match a.node with
	     | Arith x ->
		 (match x with
		   | Num q -> cnstrnt_of_num q
		   | Multq(q,x) -> Interval.multq q (cnstrnt_of_term x)
		   | Mult l -> cnstrnt_of_mult l
		   | Add l -> cnstrnt_of_add l
		   | Div(x,y) -> cnstrnt_of_div x y)
	     | _ ->
		 Interval.top)

  and cnstrnt_of_num q =
    Interval.singleton q

  and cnstrnt_of_mult l =
    match l with
      | [] -> Interval.singleton Q.one
      | [x] -> cnstrnt_of_term x
      | x :: l -> Interval.mult (cnstrnt_of_term x) (cnstrnt_of_mult l)

  and cnstrnt_of_add l =
    match l with
      | [] -> Interval.singleton Q.zero
      | [x] -> cnstrnt_of_term x
      | x :: l -> Interval.add (cnstrnt_of_term x) (cnstrnt_of_add l)

  and cnstrnt_of_div a b =
    Interval.real
  in
  cnstrnt_of_term a


  (*s [mem a c] holds iff term [a] is known to be a member of constraint [c]. *)

let num_of a =
  match a.node with
    | Arith(Num q) -> Some(q)
    | _ -> None
     
   (*s Applying a constraint to a term. *)

type status = Yes | No | X

let mem a c =
  if Interval.is_bot c then
    No
  else if Interval.is_top c then
    Yes
  else
    match num_of a with
      | Some(q) ->
	  if Interval.mem q c then Yes else No
      | None ->
	  if is_ground a then No else X

	    
(*s Apply a constraint. *)
  
let rec app c a =
  match mem a c with
    | Yes ->
	tt()
    | No ->
	ff()
    | X ->
	match a.node with
	  | Bool(Ite(x,y,z)) ->
	      let a = app c x in
	      (match a.node with
		 | Bool(True) -> app c y
		 | Bool(False) -> app c z
		 | _ ->
		     let b1 = app c y in
		     let b2 = app c z in
		     if b1 === b2 then b1 else
		       hc(Bool(Ite(a,b1,b2))))
	  | _ ->
	      hc(App(hc(Set(Cnstrnt(c))),[a]))
