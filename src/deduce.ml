
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 20013 2004.  All rights reserved.
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


(*s Add a constraint [x in i] to the the logical context [s]. *)

let infer x i s =
  if Cnstrnt.eq i Cnstrnt.mk_real then s else 
    let c' = Fact.mk_cnstrnt (Context.v s x) i None in
    let p' = Partition.add c' (Context.p_of s) in
      Context.update p' s



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
  assert(not(Q.is_zero q));
  try
    let i' = Cnstrnt.multq q (c s y) in                 (* 1. [x in q ** C(y)] *)
    let j' = Cnstrnt.multq (Q.inv q) (c s x) in         (* 2. [y in 1/q ** C(x)] *)
      infer x i' 
        (infer y j' s)
  with
      Not_found -> s

and cnstrnt_of_monomials s = 
  Arith.cnstrnt_of_monomials (c s)

and of_add x ml s =                               (* case [x = q1*y1 + ... + qn * yn]. *)
  let ml' = Arith.mk_neg x :: ml in               (* [ml'] not necessarily orderd. *)
    of_add_bounds x ml         
      (of_add_int ml' s)

and of_add_bounds x ml s = 
 let ml' = Arith.mk_neg x :: ml in
 of_add_bounds1 ml' s
 
(*
  Trace.msg "foo3" "Add_bounds" (x, ml) (Pretty.pair Term.pp (Pretty.list Term.pp));
  let isolate x pre q post =      (* [x = pre + q * ? + post] --> [? = 1/q * (x - (pre + post))] *)
    assert(not(Q.is_zero q));
    Arith.monomials (Arith.mk_multq (Q.inv q)
		      (Arith.mk_sub x (Arith.mk_addl (pre @ post))))
  in
  let ml' =  Arith.mk_neg x :: ml in
  let s' = of_add_bounds1 ml' s in                (* [x = q1*y1 + ... + qn * yn]. *)
  let processed = ref (Set.singleton x) in 
  let rec loop s pre = function
    | [] -> s
    | a :: post ->                                                  (* [x = pre + q * k + post] *)
	let (q, k) = Arith.mono_of a in
	  if Term.is_slack_var k then
	    let al' = isolate x pre q post in
	      Set.fold
		(fun y s ->
		   if Set.mem y !processed then 
		     s
		   else 
		     try 
		       processed := Set.add y !processed;
		       let b = Context.apply Th.la s y in
		       let (pre', p, post') = Arith.decompose k b in (* [y = pre' + p * k + post'] *)
		       let bl' = isolate y pre'(Q.minus p) post' in
			 of_add_bounds1 (al' @ bl') s
		     with
			 Not_found -> s)
		(use Th.la s k)
		s
	  else 
	    loop s (a :: pre) post                 (* Invariant: [ml' = pre + m + post]. *) 
  in
    loop s' [] ml'
*)

and of_add_bounds1 ml s =
  Trace.msg "foo3" "Add_bounds" ml (Pretty.list Term.pp);
  match List.partition (Arith.is_unbounded (c s)) ml with
    | yl, [] ->                               (* subcase: all monomials unbound. *)
	s
    | [], zl ->                               (* subcase: all monomials bound. *)
	propagate zl s 
    | [(Var _ as y)], zl ->                   (* subcase: only one variable is unbound. *)
	let i = Cnstrnt.multq Q.negone (cnstrnt_of_monomials s zl) in
	  infer y i s
    | [App(Arith(Multq(q)), [y])], zl ->      (* subcase: [q*y + zl = 0] with [y] bound, [zl] unbound *)
	let i = cnstrnt_of_monomials s zl in  (* thus: [y in -1/q ** C(zl)] *)
	let i' = Cnstrnt.multq (Q.minus (Q.inv q)) i in
	  infer y i' s
    | yl, zl ->   
        extend (yl, zl) s

and extend (yl, zl) s =                      (* [yl + zl = 0]. *)
  let a = Can.term s (Arith.mk_addl yl) in   (* generate: [k = yl] and [k in -C(zl)] *)
    if Arith.is_num a || Arith.is_multq a then 
      s 
    else
      let i' = Cnstrnt.multq Q.negone (cnstrnt_of_monomials s zl) in
	if is_var a then
	  infer a i' s
	else
	  let x = Var(Var.mk_slack None) in
	  let e =  Fact.mk_equal x a None in
	    Trace.msg "foo1" "Extend" e Fact.pp_equal;
	    compose Th.la e (infer x i' s)

and is_slack_combination a = 
  match a with
    | Var _ -> Term.is_slack_var a
    | App(_, xl) -> List.for_all is_slack_combination xl


(*s Propagate constraints for [ml = 0] for each variable [x] in [b].
  Suppose [b] is of the form [pre + q * x + post'], then
  [x in -1/q * (j + k)] is derived, where [pre in j] and
  [post' in k]. Following should be optimized. *)

                                            (* this needs to be done for every subterm. *)
and propagate ml = 
  Trace.msg "deduce" "Propagate" ml (Pretty.list Term.pp);
  let rec loop j post s = 
    match post with
      | [] -> s
      | m :: post' ->
	  let (q, x) = Arith.mono_of m in
	  let qinv = Q.inv q in
	  let k =  cnstrnt_of_monomials s post' in
          let j' = 
	    try Cnstrnt.add (Cnstrnt.multq qinv (c s x)) j 
	    with Not_found -> Cnstrnt.mk_real in
          let i' = Cnstrnt.multq (Q.minus qinv) (Cnstrnt.add j k) in
	  let s' = infer x i' s in
	    loop j' post' s'
  in
    loop Cnstrnt.mk_zero ml


(*s And now also propagate the integer information. *)

and of_add_int ml s = 
  let not_is_int m =
    not (Arith.is_int (c s) m)
  in
  match List.filter not_is_int ml with
    | [] -> 
	s
    | [Var _ as x] ->
	infer x Cnstrnt.mk_int s
    | [App(Arith(Num(q)), [])] -> 
	if Q.is_integer q then 
	  s 
	else 
	  raise Exc.Inconsistent
    | [App(Arith(Multq(q)), [x])] 
	when Q.is_integer q ->
	infer x Cnstrnt.mk_int s
    | ql -> 
	let a = Can.term s (Arith.mk_addl ql) in
	  match Arith.d_num a with
	    | Some(q) ->
		if Q.is_integer q then s else raise Exc.Inconsistent
	    | _ ->
		if is_var a then
		  infer a Cnstrnt.mk_int s
		else 
		  s   (* do not introduce new names for now. *)


(* Following not needed. *)

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
	      let i = Cnstrnt.multq Q.negone (cnstrnt_of_monomials s ml2) in
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


(*s Fold over all partitions of a list [l]. *)

and partitions_fold l f e =
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
