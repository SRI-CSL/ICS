
(*i
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
 i*)

(*i*)
open Mpa
open Term
open Sym
open Context
(*i*)

(*s Fold over all partitions of a list [l]. *)

let partitions_fold l f e =
  let rec part = function
    | [] -> 
	[([], [])]
    | a :: al ->
	(List.fold_left
           (fun acc (l1, l2) ->
	      (a :: l1, l2) :: (l1, a :: l2) :: acc)
           []
	   (part al))
  in
    List.fold_left f e (part l)

(*s Deduce new constraints from an equality. *)

let rec deduce e s =
  Trace.msg "deduce" "Deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    assert(is_var x);
    match b with
      | Var _ -> of_var x b s
      | App(Pp(pp), yl) -> of_pprod x (pp, yl) s
      | App(Arith(op), yl) -> of_linarith x (op, yl) s
      | App(Bvarith(op), [y]) -> of_bvarith x (op, y) s
      | _ -> s

and infer x i s =
  let c' = Fact.mk_cnstrnt (v s x) i None in
  let p' = Partition.add c' (p_of s) in
    update p' s


(*s Deduce new constraints from an equality of the form [x = b],
 where [x] is a variable. *)

and of_var x y s =
  try
    let i = c s x and j = c s y in
      infer x j (infer y i s)
  with
      Not_found -> s

and of_bvarith x (op, y) s =
  match op with
    | Unsigned ->               (* [x = unsigned(y)] *)
	infer x Cnstrnt.mk_nat s

	

and of_pprod x (op, yl) s =
  Trace.msg "deduce" "Deduce(u)" x Term.pp;
  let j = Pp.tau (c s) op yl in
  try
    let i = c s x in
      match Cnstrnt.cmp i j with
	| Binrel.Same ->
	    s
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| Binrel.Sub ->
	    s
	| Binrel.Super ->
	    infer x i s
	| Binrel.Singleton(q) ->
	    infer x (Cnstrnt.mk_singleton q) s
	| Binrel.Overlap(ij) ->
	    infer x ij s
      with
	  Not_found ->
	    infer x j s

and of_linarith x (op, yl) s =
  Trace.msg "deduce" "Deduce(a)" (x, Arith.sigma op yl) Term.pp_equal;
  match op, yl with
    | Num(q), [] -> 
	of_num x q s
    | Multq(q), [y] -> 
	of_multq x (q, y) s
    | Add, ml ->
	of_add x ml s
    | _ ->
	assert false

and of_num x q s =                                (* case [x = q]. *)
  let j' = Cnstrnt.mk_singleton q in
    infer x j' s

and of_multq x (q, y) s =                             (* case [x = q * y]. *)
  assert(not(Mpa.Q.is_zero q));
  try
    let i' = Cnstrnt.multq q (c s y) in                 (* 1. [x in q ** C(y)] *)
    let j' = Cnstrnt.multq (Mpa.Q.inv q) (c s x) in      (* 2. [y in 1/q ** C(x)] *)
      infer x i' (infer y j' s)
  with
      Not_found -> s

and is_unbounded s = function
  | App(Arith(Num _), []) -> 
      false
  | App(Arith(Multq(_)), [x]) ->
      (try Cnstrnt.is_unbounded (c s x) with Not_found -> true)
  | x ->
      (try Cnstrnt.is_unbounded (c s x) with Not_found -> true)

and cnstrnt_of_monomial s = 
  Trace.func "deduce" "Cnstrnt_of_monomial" Term.pp Cnstrnt.pp
    (function
       | App(Arith(Num(q)), []) ->
	   Cnstrnt.mk_singleton q
       | App(Arith(Multq(q)), [x]) -> 
	   (try Cnstrnt.multq q (c s x) with Not_found -> Cnstrnt.mk_real)
       | x -> 
	   (try c s x with Not_found -> Cnstrnt.mk_real))

and cnstrnt_of_monomials s = function
  | [] -> 
      Cnstrnt.mk_zero
  | [m] -> 
      cnstrnt_of_monomial s m
  | [m1; m2] -> 
      let i1 = cnstrnt_of_monomial s m1 in
      let i2 = cnstrnt_of_monomial s m2 in
      Cnstrnt.add i1 i2
  | m :: ml -> 
      let i = cnstrnt_of_monomial s m in
      Cnstrnt.add i (cnstrnt_of_monomials s ml)


and of_add x ml s =                               (* case [x = q1*y1 + ... + qn * yn]. *)
  of_add_bounds x ml 
    (of_add_int x ml s)

and of_add_bounds x ml s =
  let ml' = Arith.mk_neg x :: ml in            (* [ml'] not necessarily orderd. *)
    Trace.msg "deduce" "Add_bounds" ml' (Pretty.infixl Term.pp "++");
    match partition_unbounded s ml' with
      | yl, [] ->                              (* subcase: all monomials unbound. *)
	  let i = cnstrnt_of_monomials s yl in
	    infer x i s
      | [], zl ->                              (* subcase: all monomials bound. *)
	  propagate_zero zl s 
      | [(Var _ as y)], zl ->                  (* subcase: only one variable is unbound. *)
	  let i = Cnstrnt.multq Mpa.Q.negone (cnstrnt_of_monomials s zl) in
	    infer y i s
      | [App(Arith(Multq(q)), [y])], zl ->     (* subcase: [q*y + zl = 0] with [y] bound, [zl] unbound *)
	  let i = cnstrnt_of_monomials s zl in (* thus: [y in -1/q ** C(zl)] *)
	  let i' = Cnstrnt.multq (Mpa.Q.minus (Mpa.Q.inv q)) i in
	    infer y i' s
      | yl, zl ->   
          extend (yl, zl) s

and partition_unbounded s =
  Trace.func "deduce" "Partition"
    (Pretty.list Term.pp)
    (Pretty.pair (Pretty.list Term.pp) (Pretty.list Term.pp))
    (List.partition (is_unbounded s))

and extend (yl, zl) s =                      (* [yl + zl = 0]. *)
  let a = Can.term s (Arith.mk_addl yl) in
    if Arith.is_num a then 
      s 
    else 
      let i' = Cnstrnt.multq Mpa.Q.negone (cnstrnt_of_monomials s zl) in
	if is_var a then
	  infer a i' s
	else
	  let x = Var(Var.mk_slack None) in
	  let e =  Fact.mk_equal x a None in
	    Trace.msg "deduce" "Extend" e Fact.pp_equal;
	    compose Th.la e (infer x i' s)


(*s Propagate constraints for [ml = 0] for each variable [x] in [b].
  Suppose [b] is of the form [pre + q * x + post'], then
  [x in -1/q * (j + k)] is derived, where [pre in j] and
  [post' in k]. Following should be optimized. *)

                                                          (* this needs to be done for every subterm. *)
and propagate_zero ml =
  Trace.msg "deduce" "Propagate" ml (Pretty.list Term.pp);
  let rec loop j post s = 
    match post with
      | [] -> s
      | m :: post' ->
	  let (q, x) = Arith.mono_of m in
	  let qinv = Mpa.Q.inv q in
	  let k =  cnstrnt_of_monomials s post' in
          let j' = 
	    try Cnstrnt.add (Cnstrnt.multq qinv (c s x)) j 
	    with Not_found -> Cnstrnt.mk_real in
          let i' = Cnstrnt.multq (Mpa.Q.minus qinv) (Cnstrnt.add j k) in
	  let s' = infer x i' s in
	    loop j' post' s'
  in
    loop Cnstrnt.mk_zero ml

and propagate_zero_all ml =                           (* [ml = 0]. Solve for every "subterm" in [ml].  *)
  Trace.msg "deduce" "Propagate" ml (Pretty.list Term.pp);
  let install ml1 ml2 s =        (* add constraint for [ml1 in -C(ml2)]. *)
    match ml1 with
      | [] -> s
      | [App(Arith(Num(_)), [])] -> s
      | [App(Arith(Multq(q)), [x])]
	  when not(Q.is_zero q) ->
	  let i = Cnstrnt.multq (Q.minus (Q.inv q)) (cnstrnt_of_monomials s ml2) in
	    infer x i s
      | [x] ->
	  let i = Cnstrnt.multq Q.negone (cnstrnt_of_monomials s ml2) in
	    infer x i s  
      | _ ->
	  let a = Can.term s (Arith.mk_addl ml1) in
	    if Arith.is_num a then s else 
	      let i = Cnstrnt.multq Mpa.Q.negone (cnstrnt_of_monomials s ml2) in
		if is_var a then
		  infer a i s
		else
		  let x = Var(Var.mk_slack None) in
		  let e =  Fact.mk_equal x a None in
		    Trace.msg "deduce" "Extend" e Fact.pp_equal;
		    compose Th.la e (infer x i s) 
  in
  partitions_fold ml
    (fun acc (ml1, ml2) -> (install ml1 ml2 acc))

(*s And now also propagate the integer information. *)

and of_add_int x ml s = 
  let ml' = Arith.mk_neg x :: ml in                 (* [ml'] not necessarily orderd. *)
  Trace.msg "deduce" "Int" ml' (Pretty.list Term.pp);
  match List.filter (fun m -> not (is_int s m)) ml' with
    | [] -> 
	s
    | [Var _ as x] ->
	infer x Cnstrnt.mk_int s
    | [App(Arith(Num(q)), [])] -> 
	if Mpa.Q.is_integer q then s else raise Exc.Inconsistent
    | [App(Arith(Multq(q)), [x])] 
	when Mpa.Q.is_integer q ->
	infer x Cnstrnt.mk_int s
    | ql -> 
	let a = Can.term s (Arith.mk_addl ql) in
	  match Arith.d_num a with
	    | Some(q) ->
		if Mpa.Q.is_integer q then s else raise Exc.Inconsistent
	    | _ ->
		if is_var a then
		  infer a Cnstrnt.mk_int s
		else 
		  s   (* do not introduce new names for now. *)

	   
and is_int s m = 
  let is_int_var x = 
    try 
      Cnstrnt.dom_of (c s x) = Dom.Int 
    with 
	Not_found -> false
  in
    match m with
      | App(Arith(Num(q)), []) ->
	  Mpa.Q.is_integer q
      | App(Arith(Multq(q)), [x]) ->
	  Mpa.Q.is_integer q &&
	  is_int_var x
      | _ ->
	  is_int_var m
  



