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
 *)

(** Theory-specific canonization. *)
let sigma f al =
  match Sym.get f with
    | Sym.Arith(op) ->  Arith.sigma op al
    | Sym.Product(op) -> Product.sigma op al
    | Sym.Bv(op) ->  Bitvector.sigma op al
    | Sym.Coproduct(op) -> Coproduct.sigma op al
    | Sym.Propset(op) -> Propset.sigma op al
    | Sym.Cl(op) -> Apply.sigma op al
    | Sym.Pp(op) -> Pprod.sigma op al
    | Sym.Uninterp _ -> Term.App.mk_app f al
    | Sym.Arrays(op) -> Funarr.sigma Term.is_equal op al


(** Individual solvers *)
let solve th e =
  match th with
    | Th.Shostak(i) ->
        (match i with
           | Th.LA -> Arith.solve e
           | Th.BV -> Bitvector.solve e
           | Th.P -> Product.solve e
           | Th.COP -> Coproduct.solve e
           | Th.APP -> Apply.solve e
           | Th.SET -> Propset.solve e)
    | _ ->
        raise Exc.Incomplete
	

(** ICS inference system as the cross product
       [u * a * p * cop * cl * arr * set * bv]
  of the theory-specific inference systems. *)
module Is = struct

  (** Individual inference systems. *)
  module U = U.Infsys
  module A = A.Infsys
  module P = P.Infsys
  module Cop = Cop.Infsys
  module Cl = L.Infsys
  module Arr = Arr.Infsys
  module Set = Pset.Infsys
  module Bv = Bv.Infsys
    
  module Cross = Infsys.Cross
    
  (** Combined inference system. *)
  module Combined =
    (Cross(U)
       (Cross(A)
	  (Cross(P)
	     (Cross(Cop)
		(Cross(Cl)
		   (Cross(Arr)
		      (Cross(Set)(Bv))))))))
end 

module E = struct

  type t = Is.Combined.e

  let u_of (e: t) = fst e 
  let a_of e = fst (snd e)
  let la_of e = fst (a_of e)
  let nl_of e = snd (a_of e)
  let p_of e = fst (snd (snd e))
  let cop_of (e: t) = fst (snd (snd (snd e)))
  let cl_of e = fst (snd (snd (snd (snd e))))
  let arr_of e = fst (snd (snd (snd (snd (snd e)))))
  let set_of e = fst (snd (snd (snd (snd (snd (snd e))))))
  let bv_of e = snd (snd (snd (snd (snd (snd (snd e))))))

  let empty: t = 
    (U.S.empty, 
     ((La.S.empty, Nl.E.S.empty),
      (P.E.S.empty, 
       (Cop.E.S.empty, 
	(L.E.S.empty, 
	 (Arr.E.S.empty, 
	  (Pset.E.S.empty, 
	   Bv.E.S.empty)))))))
    
  let is_empty ((u, ((la, nl), (p, (cop, (cl, (arr, (set, bv))))))): t) =
    U.S.is_empty u &&
    La.S.is_empty la &&
    Nl.E.S.is_empty nl &&
    P.E.S.is_empty p &&
    Cop.E.S.is_empty cop &&
    L.E.S.is_empty cl &&
    Arr.E.S.is_empty arr &&
    Pset.E.S.is_empty set &&
    Bv.E.S.is_empty bv


  let eq ((u1, ((la1, nl1), (p1, (cop1, (cl1, (arr1, (set1, bv1))))))): t)
         ((u2, ((la2, nl2), (p2, (cop2, (cl2, (arr2, (set2, bv2))))))): t) 
    =
   U.S.eq u1 u2 &&
   La.S.eq la1 la2 &&
   Nl.E.S.eq nl1 nl2 &&
   P.E.S.eq p1 p2 &&
   Cop.E.S.eq cop1 cop2 &&
   L.E.S.eq cl1 cl2 &&
   Arr.E.S.eq arr1 arr2 &&
   Pset.E.S.eq set1 set2 &&
   Bv.E.S.eq bv1 bv2
    
  let pp fmt ((u, ((la, nl), (p, (cop, (cl, (arr, (set, bv))))))): t) =
    let name i =
      Format.fprintf fmt "\n%s:" (Th.to_string i)
    in
      if not(U.S.is_empty u) then
	(name Th.u; U.S.pp fmt u);
      if not(La.S.is_empty la) then
	(let name = Th.to_string Th.la in
	   Format.fprintf fmt "\n%s(r):" name;
           La.pp La.R fmt la;
	   Format.fprintf fmt "\n%s(t):" name;
	   La.pp La.T fmt la);
      if not(Nl.E.S.is_empty nl) then
	(name Th.nl; Nl.E.S.pp fmt nl);
      if not(P.E.S.is_empty p) then
	(name Th.p; P.E.S.pp fmt p);
      if not(Cop.E.S.is_empty cop) then
	(name Th.cop; Cop.E.S.pp fmt cop);
      if not(L.E.S.is_empty cl) then
	(name Th.app; L.E.S.pp fmt cl);
      if not(Arr.E.S.is_empty arr) then
	(name Th.arr; Arr.E.S.pp fmt arr);
      if not(Pset.E.S.is_empty set) then
	(name Th.set; Pset.E.S.pp fmt set);
      if not(Bv.E.S.is_empty bv) then
	(name Th.bv; Bv.E.S.pp fmt bv)

  let pp_i i fmt e =
    match i with
      | Th.Uninterpreted ->  U.S.pp fmt (u_of e)
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> 
		 La.pp La.R fmt (la_of e); 
		 Format.fprintf fmt "\n";
		 La.pp La.T fmt (la_of e)
             | Th.BV -> Bv.E.S.pp fmt (bv_of e)
             | Th.P -> P.E.S.pp fmt (p_of e)
             | Th.COP -> Cop.E.S.pp fmt (cop_of e)
             | Th.APP -> L.E.S.pp fmt (cl_of e)
             | Th.SET -> Pset.E.S.pp fmt (set_of e))
      | Th.Can(i) ->
	  (match i with
	     | Th.NL -> Nl.E.S.pp fmt (nl_of e)
	     | Th.ARR -> Arr.E.S.pp fmt (arr_of e))

  let find (e, p) i =
    match i with
      | Th.Uninterpreted -> 
	  Jst.Eqtrans.id  (* no find for uninterpreted theory. *)
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> La.S.find (la_of e)
             | Th.BV -> Bv.E.find (p, bv_of e)
             | Th.P -> P.E.find (p, p_of e)
             | Th.COP -> Cop.E.find (p, cop_of e)
             | Th.APP -> L.E.find (p, cl_of e) 
             | Th.SET -> Pset.E.find (p, set_of e))
      | Th.Can(i) ->
	  (match i with
	     | Th.NL -> Nl.E.find (p, nl_of e)
	     | Th.ARR -> Arr.E.find (p, arr_of e))

  let inv (e, p) a = 
    try
      let i = Sym.theory_of (Term.App.sym_of a) in
	(match i with
	  | Th.Uninterpreted -> 
	      Jst.Eqtrans.compose (Partition.find p) (U.S.inv (u_of e)) a
	  | Th.Shostak(i) -> 
	      (match i with
		 | Th.LA -> La.S.inv (la_of e) a
		 | Th.BV -> Bv.E.inv (p, bv_of e) a
		 | Th.P -> P.E.inv (p, p_of e) a
		 | Th.COP -> Cop.E.inv (p, cop_of e) a
		 | Th.APP -> L.E.inv (p, cl_of e) a 
		 | Th.SET -> Pset.E.inv (p, set_of e) a)
	  | Th.Can(i) ->
	      (match i with
		 | Th.NL -> Nl.E.inv (p, nl_of e) a
		 | Th.ARR -> Arr.E.inv (p, arr_of e) a))
    with
	Not_found -> Partition.find p a

  let dep i e =
    match i with
      | Th.Uninterpreted -> 
	  U.S.dep (u_of e)
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> La.S.dep (la_of e)
             | Th.BV -> Bv.E.S.dep (bv_of e)
             | Th.P -> P.E.S.dep (p_of e)
             | Th.COP -> Cop.E.S.dep (cop_of e)
             | Th.APP -> L.E.S.dep (cl_of e)
             | Th.SET -> Pset.E.S.dep (set_of e))
      | Th.Can(i) ->
	  (match i with
	     | Th.NL -> Nl.E.S.dep (nl_of e)
	     | Th.ARR -> Arr.E.S.dep (arr_of e))

  let diff ((u1, ((la1, nl1), (p1, (cop1, (cl1, (arr1, (set1, bv1))))))): t)
           ((u2, ((la2, nl2), (p2, (cop2, (cl2, (arr2, (set2, bv2))))))): t) 
    =
    let u = U.S.diff u1 u2
    and la = La.S.diff la1 la2
    and bv = Bv.E.diff bv1 bv2
    and p = P.E.diff p1 p2
    and cop = Cop.E.diff cop1 cop2
    and cl = L.E.diff cl1 cl2
    and set = Pset.E.diff set1 set2
    and nl = Nl.E.S.diff nl1 nl2
    and arr = Arr.E.S.diff arr1 arr2 in
      (u, ((la, nl), (p, (cop, (cl, (arr, (set, bv)))))))

  let copy s =
    if not(!Tools.destructive) then s else
      let (u, ((la, nl), (p, (cop, (cl, (arr, (set, bv))))))) = s in
      let u = U.S.copy u
      and la = La.S.copy la
      and bv = Bv.E.S.copy bv
      and p = P.E.S.copy p
      and cop = Cop.E.S.copy cop
      and cl = L.E.S.copy cl
      and set = Pset.E.S.copy set
      and nl = Nl.E.S.copy nl
      and arr = Arr.E.S.copy arr in
	(u, ((la, nl), (p, (cop, (cl, (arr, (set, bv)))))))
	
end 

module G = Fact.Input
module P = Partition

module Tr: Infsys.LEVEL with type t = E.t = 
struct
  type t = E.t
  let level = "rule"
  let eq = E.eq
  let diff = E.diff
  let pp = E.pp
end 

(** Tracing rule applications. *)
module S: (Infsys.IS with type e = E.t) = 
  Infsys.Trace(Is.Combined)(Tr)

type e = E.t
type t = e Infsys.config
    (** A configuration consists of a triple [(g, e, p)] with
      - [g] the input facts,
      - [e] the equality sets for the individual theories
      - [p] the shared variable equalities and disequalities. *)
    
type processed = E.t * P.t

let eq (g1, e1, p1) (g2, e2, p2) =
  G.eq g1 g2 &&
  E.eq e1 e2 &&
  P.eq p1 p2

let empty = 
  (G.empty, E.empty, P.empty)

let make (g, e, p) = (g, e, p)

let is_empty (g, e, p) = 
  G.is_empty g &&
  E.is_empty e &&
  P.is_empty p

let pp fmt ((g, e, p): t) = 
  G.pp fmt g;
  E.pp fmt e;
  P.pp fmt p

let copy ((g, e, p) as s) = 
  if !Tools.destructive then 
    (G.copy g, E.copy e, P.copy p)
  else 
    s

let protect f s = f (copy s)

let do_destructive = ref false

let with_destructive f s =
  if !do_destructive then
    try
      Tools.destructive := true;
      let s' = copy s in
      let s'' = f s' in
	Tools.destructive := false;
	s''
    with
	exc -> 
	  Tools.destructive := false;
	  raise exc
  else 
    f s

(** Canonization of mixed terms. *)
let rec can ((e, p) as s) a =
  let sigma f al = (sigma f al, Jst.dep0) in
  let interp_can f = 
    let i = Sym.theory_of f in
      Jst.Eqtrans.compose_partial1
	(E.find s i) 
	(can s)
  in 
    if Term.is_var a then
      P.find p a
    else 
      try
	Jst.Eqtrans.compose
	  (E.inv s)                       
	  (Jst.Eqtrans.mapargs sigma interp_can)
	  a 
      with 
	  Not_found ->
	    P.find p a

let can s = Jst.Eqtrans.trace "canon" "Can" (can s)

let is_canonical s a =
  let (b, rho) = can s a in
    if Term.eq a b then Some(rho) else None

let is_equal s a b =
  let (a', rho') = can s a
  and (b', tau') = can s b in
    if Term.eq a' b' then Some(Jst.dep2 rho' tau') else None

(** Explicitly generated disequalities. *)
let is_diseq ((e, p) as s) a b =
  let (a', rho') = can s a
  and (b', tau') = can s b in
    match Partition.is_diseq p a' b' with
      | Some(sigma') -> Some(Jst.dep3 rho' tau' sigma')
      | None -> None

let is_equal_or_diseq s =
  Jst.Rel2.yes_or_no (is_equal s) (is_diseq s)

let cheap = ref false

(** Incomplete test. *)
let is_nonneg ((e, p) as s) a =
  if Term.is_pure Th.la a then
    let la = E.la_of e in
    let (b, rho) = La.can (p, la) a in
      match Arith.is_nonneg b with
	| Three.Yes -> Jst.Three.Yes(rho)
	| Three.No -> Jst.Three.No(rho)
	| Three.X -> Jst.Three.X
  else 
    Jst.Three.X

(** Incomplete test. *)
let is_pos ((e, p) as s) a =
  if Term.is_pure Th.la a then
    let la = E.la_of e in
    let (b, rho) = La.can (p, la) a in
      match Arith.is_pos b with
	| Three.Yes -> Jst.Three.Yes(rho)
	| Three.No -> Jst.Three.No(rho)
	| Three.X -> Jst.Three.X
  else 
    Jst.Three.X
	

(** Simplification of an atom. *)  
let simplify ((_, p) as s) atm =
  let id atm = (atm, Jst.dep0) in
  match Atom.atom_of atm with
    | (Atom.TT | Atom.FF) -> 
	id atm
    | Atom.Equal(a, b) -> 
	let (a', rho') = can s a 
	and (b', tau') = can s b in
	  if Term.eq a' b' then
	    (Atom.mk_true, Jst.dep2 rho' tau')
	  else              (* only check for explicit disequalities. *)
	    (match Partition.is_diseq p a' b' with 
	       | Some(sigma') -> 
		   (Atom.mk_false, Jst.dep3 rho' tau' sigma')
	       | None -> 
		   if a == a' && b == b' then
		     id atm
		   else 
		     (Atom.mk_equal (a', b'), Jst.dep2 rho' tau'))   
    | Atom.Diseq(a, b) ->
	let (a', rho') = can s a 
	and (b', tau') = can s b in
	  if Term.eq a' b' then
	    (Atom.mk_false, Jst.dep2 rho' tau')
	  else              (* only check for explicit disequalities. *)
	    (match Partition.is_diseq p a' b' with
	       | Some(sigma') -> 
		   (Atom.mk_true, Jst.dep3 rho' tau' sigma')
	       | None -> 
		   if a == a' && b == b' then
		     id atm
		   else 
		     (Atom.mk_diseq (a', b'), Jst.dep2 rho' tau'))
    | Atom.Nonneg(a) -> 
	let (a', rho') = can s a in
	  (match is_nonneg s a' with
	    | Jst.Three.Yes(tau') -> 
		(Atom.mk_true, Jst.dep2 rho' tau')
	    | Jst.Three.No(tau') ->
		(Atom.mk_false, Jst.dep2 rho' tau')
	    | Jst.Three.X ->
		if a == a' then id atm else 
		  (Atom.mk_nonneg a', rho'))
    | Atom.Pos(a) -> 
	let (a', rho') = can s a in
	  (match is_pos s a' with
	     | Jst.Three.Yes(tau') -> 
		 (Atom.mk_true, Jst.dep2 rho' tau')
	     | Jst.Three.No(tau') -> 
		 (Atom.mk_false, Jst.dep2 rho' tau')
	     | Jst.Three.X ->
		 if a == a' then id atm else 
		   (Atom.mk_pos a', rho'))

let simplify s =
  Trace.func "rule" "Simplify" Atom.pp (Pretty.pair Atom.pp Jst.pp)
    (simplify s)


(** Domain Interpretation. *)
let dom s a =
  let (b, rho) = can s a in
  let d = Arith.dom_of b in
    (d, rho)


(** {6 Processing} *)	


(** Apply applicable inference rules until input [G] is empty. *)   
let rec process s =
  let eval s =
    let ((g, _, _) as s) = 
      process_star s 
    in
      assert(G.is_empty g);
      S.normalize s
  in
    with_destructive
      eval s

and process_star ((g, _, _) as s) =
  Trace.msg "process" "Process_star" g G.pp;
  if G.is_empty g then s else
    let s = process1 s in
      process_star s

and process1 s =
  let s1 = process_equal s in
  let s2 = process_diseq s1 in
    s2
    
and process_equal (((g, e, p) as s) : t) =
  try
    let (e', g') = G.Equal.choose g in
      Trace.msg "rule" "Input" e' Fact.Equal.pp;
      let e' = Fact.Equal.map (can (e, p)) e' in
      let (a', b', rho') = Fact.Equal.destruct e' in
	(match is_equal (e, p) a' b' with
	  | Some _ -> 
	      (g', e, p)    (* drop. *)
	  | None -> 
	      let s' = 
		match Fact.Equal.status e' with
		  | Term.Variable ->
		      assert(Fact.Equal.is_var e');
		      (g', e, Partition.merge p e')
		  | Term.Pure(i) -> 
		      assert(Fact.Equal.is_pure i e');
		      S.merge i e' (g', e, p)
		  | Term.Mixed(i, a) -> 
		      S.abstract i a s
	      in
		propagate s')
  with
      Not_found -> s

(** Propagate all fresh variable equalities and 
  disequalities in partitioning [p] to individual 
  procedures. *)
and propagate s =
  try
    let s' = propagate_equal s in
      propagate s'
  with
      Not_found -> 
	(try
	   let s' = propagate_diseq s in
	     propagate s' 
	 with
	     Not_found -> s)

and propagate_equal ((g, e, p) as s) =
  let (eq, p) = P.fresh_equal p in
    S.propagate eq (g, e, p)

and propagate_diseq ((g, e, p) as s) = 
  let (d, p) = P.fresh_diseq p in
    S.propagate_diseq d (g, e, p)

and process_diseq ((g, e, p) as s) = 
  try
    let (d, g') = G.Diseq.choose g in 
      Trace.msg "rule" "Input" d Fact.Diseq.pp;
      let d = Fact.Diseq.map (can (e, p)) d in
      let (a, b, rho) = Fact.Diseq.destruct d in
	(match is_diseq (e, p) a b with
	   | Some _ -> 
	       (g', e, p)     (* drop. *)
	   | None -> 
	       let s' = 
		 match Fact.Diseq.status d with
		   | Term.Variable ->
		       assert(Fact.Diseq.is_var d);
		       let p' = Partition.dismerge p d in
			 S.propagate_diseq d (g', e, p')
		   | Term.Pure(i) -> 
		       assert(Fact.Diseq.is_pure i d);
		       S.dismerge i d (g', e, p)
		   | Term.Mixed(i, a) ->
		       S.abstract i a s
	       in
		 propagate s')
  with
      Not_found -> s

(** Two terms [a], [b] are disequal in [(e, p)] if asserting
  [a = b] yields a contradiction. This also takes into 
  account all implicit disequalies. *)
and is_diseq_complete (e, p) a b =
  let d = Fact.Diseq.make (a, b, Jst.dep0) in
  let s = (G.Diseq.add G.empty d, e, p) in
    try
      let _ = protect process s in
	None
    with
	Jst.Inconsistent(rho) -> Some(rho)

(** Search for satisfiable branch. *)
let rec is_sat s =
  match S.branch s with
    | [] ->
	invalid_arg "Combine.check: empty branching"
    | [s] -> 
	(try
	   let s' = protect process s in
	     Some(s')
	 with
	     Jst.Inconsistent _ -> None)
    | sl ->
	is_sat_orelse sl

and is_sat_orelse sl =
  match sl with
    | [] -> 
	None
    | s :: sl -> 
	(try
	   let s' = protect process s in
	     is_sat s'
	 with
	     Jst.Inconsistent _ -> is_sat_orelse sl)

let gc s = s

let model (e, p) =
  La.model (E.la_of e)

let minimize (e, p) a =
  La.lower (p, E.la_of e) a

let maximize (e, p) a =
  La.upper (p, E.la_of e) a
