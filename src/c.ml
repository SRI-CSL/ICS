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

let of_term s a =
  try
    let lookup s x = fst(apply s x) in
      Cnstrnt.of_term (lookup s) a
  with
      Not_found -> Cnstrnt.mk_real

let of_term s =
  Trace.func "foo" "of_term" Term.pp Cnstrnt.pp (of_term s)

let of_addl s a =
  try
    let lookup s x = fst(apply s x) in
      Cnstrnt.of_addl (lookup s) a
  with
      Not_found -> Cnstrnt.mk_real


(** {6 Constructors} *)

let mk_less s (a, beta, b) =
  Fact.mk_less (Arith.mk_sub a b, beta) None

let mk_greater s (a, beta, b) =
  Fact.mk_less (Arith.mk_sub b a, beta) None


(** {6 Predicates} *)

let rec is_less s (a, alpha) = 
  Trace.msg "foo" "is_less" a Term.pp;
  match a with
    | App(Arith(Num(q)), []) ->
	if alpha then Q.le q Q.zero else Q.lt q Q.zero
    | _ ->
	(Cnstrnt.exists_upper 
	   (fun (beta, u) -> 
	      is_less s (u, alpha && beta)) 
	   (of_term s a))

let rec is_greater s (a, alpha) =  
  Trace.msg "foo" "is_greater" a Term.pp;
  match a with
    | App(Arith(Num(q)), []) ->
	if alpha then Q.ge q Q.zero else Q.gt q Q.zero
    | _ ->
	(Cnstrnt.exists_lower
	   (fun (beta, l) -> 
	      is_greater s (l, alpha && beta)) 
	   (of_term s a))

let holds s (a, alpha) =
  if is_less s (a, alpha) then
    Three.Yes
  else if is_greater s (a, not alpha) then
    Three.No
  else 
    Three.X

let holds s =
  Trace.func "foo2" "Holds" (Pretty.pair Term.pp Pretty.bool) Three.pp (holds s)


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
let rec add l s =
  eqs := Fact.Equalset.empty;
  let s' = addl [l] s in
    (!eqs, s')


and addl ineqs s =
  match ineqs with
    | [] -> s
    | ineq :: ineqs' ->
	let s' = add1 ineq s in
	  addl ineqs' s'

and add1 ineq s =
  Trace.msg "c'" "Add" ineq Fact.pp_less;
  match Ineq.solve ineq with
    | Ineq.True -> 
	s
    | Ineq.Less(x, beta, b) ->
	(try
	   let (c, _) = apply s x in
	   let c' = Cnstrnt.add_upper (beta, b) c in
	     if c == c' then s else 
	       let s' = update x c' None s in
	       let ineqs' =      (* derived *)
		 Cnstrnt.fold_lower 
		   (fun (gamma, l) acc ->
		      let ineq = mk_less s (l, gamma && beta, b) in 
			ineq :: acc)
		   c' []
	       in
		 addl ineqs' s'
	 with
	     Not_found ->
	       let c = Cnstrnt.mk_less Dom.Real (b, beta) in
		 update x c None s)
    | Ineq.Greater(x, alpha, a) -> 
	(try
	   let (c, _) = apply s x in
	   let c' = Cnstrnt.add_lower (alpha, a) c in
	     if c == c' then s else 
	       let s' = update x c' None s in
	       let ineqs' =      (* derived *)
		 Cnstrnt.fold_upper 
		   (fun (gamma, u) acc ->
		      let ineq = mk_less s (a, gamma && alpha, u) in 
			ineq :: acc)
		   c' []
	       in
		 addl ineqs' s'
	 with
	     Not_found ->
	       let c = Cnstrnt.mk_greater Dom.Real (alpha, a) in
		 update x c None s)


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
	       let ineqs' = 
		 Cnstrnt.fold_lower
		   (fun (alpha, l) acc ->
		      mk_less s (l, alpha, a) :: acc)
		   c []
	       in
	       let ineqs'' = 
		 Cnstrnt.fold_upper
		   (fun (beta, u) acc ->
		      mk_less s (a, beta, u) :: acc)
		   c ineqs'
	       in
	       let s' = instantiate s x a in
		 addl ineqs'' s')
    with
	Not_found ->
	  instantiate s x a

and instantiate s x a =
  Set.fold
    (fun y s ->
       try
	 let (c, _) = apply s y in
	 let c' = Cnstrnt.replace x a c in
	   update y c' None (restrict y s)
       with
	   Not_found -> s)
    (use s x) 
    s	 

(** Adding a domain constraint. *)

let dom c s =
  let (x, d, _) = Fact.d_dom c in
    try
      let (c, _) = apply s x in
      let c' = Cnstrnt.add_dom d c in
	if c == c' then 
	  (Fact.Equalset.empty, s)
	else
	  begin
	    eqs := Fact.Equalset.empty;
	    let s' = update x c' None s in
	      (!eqs, s')
	  end
    with
	Not_found ->
	  let s' = update x (Cnstrnt.mk_dom d) None s in
	    (Fact.Equalset.empty, s')
	  

(** Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let diseq d s =
  Trace.msg "c1" "Diseq" d Fact.pp_diseq;
  let (x, a, _) = Fact.d_diseq d in
    try
      let (c, _) = apply s x in
      let c' = Cnstrnt.add_diseq a c in
	if c == c' then
	  (Fact.Equalset.empty, s)
	else
	  begin
	    eqs := Fact.Equalset.empty;
	    let s' = update x c' None s in
	      (!eqs, s')
	  end 
    with
	Not_found -> 
	  (Fact.Equalset.empty, s)


(** Finite ranges *)

let split s = 
  failwith "split: to do"
