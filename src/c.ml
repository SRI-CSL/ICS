(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
 *)

open Term
open Sym
open Mpa

type t = {
  c: (Cnstrnt.t * Fact.justification option) Term.Map.t;
  use : Use.t;
}

let empty = {
  c = Term.Map.empty;
  use = Use.empty
}

let eq s t = (s.c == t.c)

let cnstrnts s = s.c

let to_list s =
  Term.Map.fold 
    (fun x (i, _) acc -> (x, i) :: acc) 
    s.c []

let pp fmt s =
  let l = to_list s in
    if l <> [] then
      begin
	Format.fprintf fmt "\nc:";
	Pretty.map Term.pp Cnstrnt.pp fmt l
      end

let changed = ref Set.empty

let apply s a = Term.Map.find a s.c

let use s = Use.find s.use

let cnstrnt s x =
  let (i, prf) = apply s x in
    Fact.mk_cnstrnt x i prf

let mem a s = Term.Map.mem a s.c


(** {6 Abstract interpretation} *)

let of_term s =
 let lookup s x = fst(apply s x) in
   Cnstrnt.of_term (lookup s)

let of_addl s =
  let lookup s x = fst(apply s x) in
    Cnstrnt.of_addl (lookup s)


(** {6 Predicates} *)

let is_less s =
  let rec less (a, alpha) =
    Trace.msg "foo" "holds?" (a, alpha) (Pretty.pair Term.pp Pretty.bool);
    (Ineq.is_less (a, alpha, Arith.mk_zero)) 
    ||
    (try
       let c = of_term s a in
	 Cnstrnt.exists_upper (fun (beta, u) ->less (u, alpha && beta)) c
     with
	 Not_found -> false)
  in
    less


(** {6 Updating of datastructures} *)

let varfold f =
  let rec termfold acc = function
    | ((Var _) as x) -> f x acc
    | App(_, al) -> List.fold_left termfold acc al
  in 
  let boundfold acc = function
    | Cnstrnt.Neginf -> acc
    | Cnstrnt.Posinf -> acc
    | Cnstrnt.Bound(_, a) -> termfold acc a
  in
    Cnstrnt.fold
      (fun (l, u) acc -> 
	 boundfold (boundfold acc l) u)
       

(** [update x c s] updates the constraint map with the constraint [x in c]
   and modifies the use lists accordingly. In addition, all implied 
   inequalities are added. As a side-effect, {!C.changed} is updated. *)

let eqs = ref Fact.Equalset.empty

let update a c prf s =
  Trace.msg "c" "Update" (a, c) (Pretty.infix Term.pp " in " Cnstrnt.pp);
  let (bs, c) = Cnstrnt.d_equalities c in
    if Cnstrnt.is_empty c then
      raise Exc.Inconsistent
    else 
      begin
	changed := Term.Set.add a !changed;
	Term.Set.iter
	  (fun b ->
	     let e = Fact.mk_equal a b None in
	       eqs := Fact.Equalset.add e !eqs)
	  bs;
	let use' =
	  try
	    let (d,_) = apply s a in
	      varfold (Use.remove a) d s.use
	  with
	      Not_found -> s.use
	in
	  {s with 
	     c = Term.Map.add a (c, prf) s.c;
	     use = varfold (fun b -> Use.add a b) c use'}
      end 

(** Restrict the map. *)
let restrict a s =
  try
    let (i, _) = apply s a in
      Trace.msg "c" "Restrict" a Term.pp;
      changed := Term.Set.remove a !changed;
      {s with 
	 c = Term.Map.remove a s.c;
	 use = varfold (fun b -> Use.remove a b) i s.use}
  with
      Not_found -> s

(** Asserting an inequality *)
let rec add rho s =
  eqs := Fact.Equalset.empty;
  let (a, alpha, b, prf) = Fact.d_less rho in
  let s' = addl [Ineq.mk_less (a, alpha, b)] s in
    (!eqs, s')

and ineqs_of (a, c) = 
  Cnstrnt.fold
    (fun (l, u) acc ->
       let acc' = match l with
	 | Cnstrnt.Bound(beta, b) -> Ineq.mk_greater (a, beta, b) :: acc
	 | _ -> acc
       in
       let acc'' = match u with
	 | Cnstrnt.Bound(beta, b) -> Ineq.mk_less (a, beta, b) :: acc'
	 | _ -> acc'
       in
	 acc'')
    c []

and addl ineqs s =
  match ineqs with
    | [] -> s
    | ineq :: ineqs' ->
	let s' = add1 ineq s in
	  addl ineqs' s'

and add1 ineq s =
  match Ineq.solve ineq with
    | Ineq.True -> 
	s
    | Ineq.False -> 
	raise Exc.Inconsistent
    | Ineq.Less(x, beta, b) ->
	(* if is_less s (x, beta, b) then s else *)
	  (try
	     let (c, _) = apply s x in
	     let c' = Cnstrnt.add_upper (beta, b) c in
	     let s' = update x c' None s in
	     let ineqs' =      (* derived *)
	       Cnstrnt.fold_lower 
		 (fun (gamma, l) acc ->
		    let ineq = (Arith.mk_sub l b, gamma && beta) in
		      if is_less s ineq then
			acc 
		      else
			let ineq = Ineq.mk_less (Arith.mk_sub l b, gamma && beta, Arith.mk_zero) in 
			  ineq :: acc)
		 c' []
	     in
	       addl ineqs' s'
	   with
	       Not_found ->
		 let c = Cnstrnt.mk_less Dom.Real (b, beta) in
		   update x c None s)
    | Ineq.Greater(x, alpha, a) -> 
	failwith "to do"


(** Propagating an equality. *)
let rec merge e s =
  eqs := Fact.Equalset.empty;
  let s' = merge1 e s in
    (!eqs, s')

and merge1 e s =
  let (x, a, _) = Fact.d_equal e in
    try
      let (c, _) = apply s x in
	(try
	   let (d, _) = apply s a in
	   let cd = Cnstrnt.inter c d in
	     update x cd  None (restrict a s)
	 with
	     Not_found -> 
	       let ineqs = 
		 Cnstrnt.fold_lower
		   (fun (alpha, l) acc ->
		      Ineq.mk_less (l, alpha, a) :: acc)
		   c []
	       in
	       let ineqs' = 
		 Cnstrnt.fold_upper
		   (fun (beta, u) acc ->
		      Ineq.mk_less (a, beta, u) :: acc)
		   c ineqs
	       in
	       let s' = instantiate s x a in
		 addl ineqs' s')
    with
	Not_found ->
	  instantiate s x a

and instantiate s x a =
  Set.fold
    (fun x s ->
       try
	 let (c, _) = apply s x in
	 let c' = Cnstrnt.replace x a c in
	   update x c' None (restrict x s)
       with
	   Not_found -> s)
    (use s x) 
    s	 


(** Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let diseq d s =
  Trace.msg "c1" "Diseq" d Fact.pp_diseq;
  s


(** Finite ranges *)

let split s = 
  failwith "split: to do"
