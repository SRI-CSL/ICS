(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
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


(** Combined equality sets. *)
module E = struct

  module S = Solution.Set

  type t = {
    u: U.S.t;
    la: La.S.t;
    nl: S.t;
    p: S.t;
    cop: S.t;
    cl : S.t;
    arr : S.t;
    set : S.t;
    bv : S.t
  }

   (** Projections to individual equality sets. *)
  let u_of s = s.u
  let la_of s = s.la
  let nl_of s = s.nl
  let p_of s = s.p
  let cop_of s = s.cop
  let cl_of s = s.cl
  let arr_of s = s.arr
  let set_of s = s.set


  let empty = {
    u = U.S.empty;
    la = La.S.empty;
    nl = S.empty;
    p = S.empty;
    cop = S.empty;
    cl = S.empty;
    arr = S.empty;
    set = S.empty;
    bv = S.empty
  }

  let copy s = {
    u = U.S.copy s.u;
    la = La.S.copy s.la;
    nl = S.copy s.nl;
    p = S.copy s.p;
    cop = S.copy s.cop;
    cl = S.copy s.cl;
    arr = S.copy s.arr;
    set = S.copy s.set;
    bv = S.copy s.bv
  }

  let is_empty s =
    U.S.is_empty s.u &&
    La.S.is_empty s.la &&
    S.is_empty s.nl &&
    S.is_empty s.p &&
    S.is_empty s.cop &&
    S.is_empty s.cl &&
    S.is_empty s.arr &&
    S.is_empty s.set &&
    S.is_empty s.bv
    
  let eq s1 s2 =
    U.S.eq s1.u s2.u &&
    La.S.eq s1.la s2.la &&
    S.eq s1.nl s2.nl &&
    S.eq s1.p s2.p &&
    S.eq s1.cop s2.cop &&
    S.eq s1.cl s2.cl &&
    S.eq s1.arr s2.arr &&
    S.eq s1.set s2.set &&
    S.eq s1.bv s2.bv

   
  let pp fmt s =
    let name i = Format.fprintf fmt "\n%s:" (Th.to_string i) in
      if not(U.S.is_empty s.u) then
	(name Th.u; U.S.pp fmt s.u);
      if not(La.S.is_empty s.la) then
	(let name = Th.to_string Th.la in
	   Format.fprintf fmt "\n%s(r):" name; La.pp La.R fmt s.la;
	   Format.fprintf fmt "\n%s(t):" name; La.pp La.T fmt s.la);
      if not(S.is_empty s.nl) then
	(name Th.nl; S.pp fmt s.nl);
      if not(S.is_empty s.p) then
	(name Th.p; S.pp fmt s.p);
      if not(S.is_empty s.cop) then
	(name Th.cop; S.pp fmt s.cop);
      if not(S.is_empty s.cl) then
	(name Th.app; S.pp fmt s.cl);
      if not(S.is_empty s.arr) then
	(name Th.arr; S.pp fmt s.arr);
      if not(S.is_empty s.set) then
	(name Th.set; S.pp fmt s.set);
      if not(S.is_empty s.bv) then
	(name Th.bv; S.pp fmt s.bv)

  let pp_i i fmt s =
    match i with
      | Th.Uninterpreted ->  U.S.pp fmt s.u
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> 
		 La.pp La.R fmt s.la; 
		 Format.fprintf fmt "\n";
		 La.pp La.T fmt s.la
             | Th.BV -> S.pp fmt s.bv
             | Th.P -> S.pp fmt s.p
             | Th.COP -> S.pp fmt s.cop
             | Th.APP -> S.pp fmt s.cl
             | Th.SET -> S.pp fmt s.set)
      | Th.Can(i) ->
	  (match i with
	     | Th.NL -> S.pp fmt s.nl
	     | Th.ARR -> S.pp fmt s.arr)

  let find (s, p) = function
    | Th.Uninterpreted -> 
	Jst.Eqtrans.id  (* no find for uninterpreted theory. *)
    | Th.Shostak(i) -> 
	(match i with
           | Th.LA -> La.S.find s.la
           | Th.BV -> S.find s.bv
           | Th.P -> S.find s.p
           | Th.COP -> S.find s.cop
           | Th.APP -> S.find s.cl
           | Th.SET -> S.find s.set)
    | Th.Can(i) ->
	(match i with
	   | Th.NL -> Nl.Ops.find (p, s.nl)
	   | Th.ARR -> (Arr.Ops.find (p, s.arr)))
	 
  let inv (s, p) a = 
    try
      let i = Sym.theory_of (Term.App.sym_of a) in
	(match i with
	  | Th.Uninterpreted -> 
	      Jst.Eqtrans.compose (Partition.find p) (U.S.inv s.u) a
	  | Th.Shostak(i) -> 
	      (match i with
		 | Th.LA -> La.S.inv s.la a
		 | Th.BV -> S.inv s.bv a
		 | Th.P -> S.inv s.p a
		 | Th.COP -> S.inv s.cop a
		 | Th.APP -> S.inv s.cl a 
		 | Th.SET -> S.inv s.set a)
	  | Th.Can(i) ->
	      (match i with
		 | Th.NL -> Nl.Ops.inv (p, s.nl) a
		 | Th.ARR -> Arr.Ops.inv (p,s .arr) a))
    with
	Not_found -> Partition.find p a

  let dep i s =
    match i with
      | Th.Uninterpreted -> 
	  U.S.dep s.u
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> La.S.dep s.la
             | Th.BV -> S.dep s.bv
             | Th.P -> S.dep s.p
             | Th.COP -> S.dep s.cop
             | Th.APP -> S.dep s.cl
             | Th.SET -> S.dep s.set)
      | Th.Can(i) ->
	  (match i with
	     | Th.NL -> S.dep s.nl
	     | Th.ARR -> S.dep s.arr)

  let diff s1 s2 =
    {empty with
       u = U.S.diff s1.u s2.u;
       la = La.S.diff s1.la s2.la;
       bv = S.diff s1.bv s2.bv;
       p = S.diff s1.p s2.p;
       cop = S.diff s2.cop s2.cop;
       cl = S.diff s1.cl s2.cl;
       set = S.diff s1.set s2.set;
       nl = S.diff s1.nl s2.nl;
       arr = S.diff s1.arr s2.arr}
	
end 


type e = E.t

type t = e Infsys.Config.t
    (** A configuration consists of a triple [(g, e, p)] with
      - [g] the input facts,
      - [e] the equality sets for the individual theories
      - [p] the shared variable equalities and disequalities. *)
    
type processed = E.t * Partition.t

let pp fmt ((g, e, p): t) = 
  G.pp fmt g; E.pp fmt e; Partition.pp fmt p
    
let copy ((g, e, p) as s) = 
  (G.copy g, E.copy e, Partition.copy p)
  
let protect f s = 
  f (copy s)

	

(** ICS inference system as the cross product
       [u * a * p * cop * cl * arr * set * bv]
  of the theory-specific inference systems. *)
module Is = struct


  (** Load configuration into global variables. *)
  let rec initialize fct s p =
    let g0 = G.copy G.empty in
      G.put fct g0;
      Infsys.g := g0;
      initialize0 s p

  and initialize0 s p =
    Infsys.p := Partition.copy p;
    U.Infsys.initialize(s.E.u);
    La.Infsys.initialize(s.E.la);
    Nl.Infsys.initialize(s.E.nl);
    P.Infsys.initialize(s.E.p);
    Cop.Infsys.initialize(s.E.cop);
    L.Infsys.initialize(s.E.cl);
    Arr.Infsys.initialize(s.E.arr);
    Pset.Infsys.initialize(s.E.set);
    Bv.Infsys.initialize(s.E.bv)
      
  (** Save global configuration into a pair [(s, p)]. *)
  let finalize () = 
    assert(G.is_empty !Infsys.g);
    let p = !Infsys.p in
    let u = U.Infsys.finalize() in
    let la = La.Infsys.finalize() in
    let nl = Nl.Infsys.finalize() in
    let pro = P.Infsys.finalize() in
    let cop = Cop.Infsys.finalize() in
    let cl = L.Infsys.finalize() in
    let arr = Arr.Infsys.finalize() in
    let set = Pset.Infsys.finalize() in
    let bv = Bv.Infsys.finalize() in
    let  s = {
      E.u = u;
      E.la = la;
      E.nl = nl;
      E.p = pro;
      E.cop = cop;
      E.cl = cl;
      E.arr = arr;
      E.set = set;
      E.bv = bv
    } 
    in
      (s, p)

  
  let abstract i a =
    assert(Term.is_pure i a);
    match i with
      | Th.Uninterpreted -> 
	  U.Infsys.abstract a
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> La.Infsys.abstract a 
             | Th.BV -> Bv.Infsys.abstract a
             | Th.P ->  P.Infsys.abstract a
             | Th.COP -> Cop.Infsys.abstract a 
             | Th.APP -> L.Infsys.abstract a
             | Th.SET -> Pset.Infsys.abstract a) 
      | Th.Can(i) ->
	  (match i with
	     | Th.NL ->  Nl.Infsys.abstract a
	     | Th.ARR -> Arr.Infsys.abstract a)

  let merge i e =
    assert(Fact.Equal.is_pure i e);
    match i with
      | Th.Uninterpreted -> 
	  U.Infsys.merge e
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> 
		 La.Infsys.merge e;
		 if not(Solution.Set.is_empty (Nl.Infsys.current())) then 
		   Nl.Infsys.merge e         (* propagate linear equalities in [nl]. *)
             | Th.BV -> Bv.Infsys.merge e
             | Th.P ->  P.Infsys.merge e
             | Th.COP -> Cop.Infsys.merge e
             | Th.APP -> L.Infsys.merge e
             | Th.SET -> Pset.Infsys.merge e)
      | Th.Can(i) ->
	  (match i with
	     | Th.NL ->  Nl.Infsys.merge e
	     | Th.ARR -> Arr.Infsys.merge e)

  let propagate e =
    assert(Fact.Equal.is_var e);
    U.Infsys.propagate e;
    La.Infsys.propagate e;
    Bv.Infsys.propagate e;
    P.Infsys.propagate e;
    Cop.Infsys.propagate e;
    L.Infsys.propagate e;
    Pset.Infsys.propagate e;
    Nl.Infsys.propagate e;
    Arr.Infsys.propagate e

  let dismerge i d =
    assert(Fact.Diseq.is_pure i d);
    match i with
      | Th.Uninterpreted -> 
	  U.Infsys.dismerge d 
      | Th.Shostak(i) -> 
	  (match i with
             | Th.LA -> La.Infsys.dismerge d
             | Th.BV -> Bv.Infsys.dismerge d
             | Th.P ->  P.Infsys.dismerge d
             | Th.COP -> Cop.Infsys.dismerge d
             | Th.APP -> L.Infsys.dismerge d
             | Th.SET -> Pset.Infsys.dismerge d)
      | Th.Can(i) ->
	  (match i with
	     | Th.NL ->  Nl.Infsys.dismerge d
	     | Th.ARR -> Arr.Infsys.dismerge d)


  let propagate_diseq d =
    assert(Fact.Diseq.is_var d);  
      U.Infsys.propagate_diseq d;
      La.Infsys.propagate_diseq d;
      Bv.Infsys.propagate_diseq d;
      P.Infsys.propagate_diseq d;
      Cop.Infsys.propagate_diseq d;
      L.Infsys.propagate_diseq d; 
      Pset.Infsys.propagate_diseq d;  
      Nl.Infsys.propagate_diseq d;
      Arr.Infsys.propagate_diseq d

  let branch () = ()

  let normalize () =
    La.Infsys.normalize ()

  let nonneg nn =
    assert(Fact.Nonneg.is_pure Th.la nn);
    La.Infsys.nonneg nn

  let pos pp =
    assert(Fact.Pos.is_pure Th.la pp);
    La.Infsys.pos pp

end 
   

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
      Partition.find p a
    else 
      try
	Jst.Eqtrans.compose
	  (E.inv s)                       
	  (Jst.Eqtrans.mapargs sigma interp_can)
	  a 
      with 
	  Not_found ->
	    Partition.find p a


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
    let (b, rho) = La.can (p, e.E.la) a in
      match Arith.is_nonneg b with
	| Three.Yes -> Jst.Three.Yes(rho)
	| Three.No -> Jst.Three.No(rho)
	| Three.X -> Jst.Three.X
  else 
    Jst.Three.X


(** Incomplete test. *)
let is_pos ((e, p) as s) a =
  if Term.is_pure Th.la a then
    let (b, rho) = La.can (p, e.E.la) a in
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


(** Domain Interpretation. *)
let dom s a =
  let (b, rho) = can s a in
  let d = Arith.dom_of b in
    (d, rho)


(** {6 Processing} *)	

(** Apply applicable inference rules until input [G] is empty. *)   
let rec process fct (s, p) =
  Is.initialize fct s p;        (* 1. store initial configuration into global variables. *)
  eval ();                      (* 2. run rule interpreter. *)  
  assert(G.is_empty !Infsys.g); (* 3. restore final configuration from global variables. *)
  Is.finalize () 

and eval () =
  if G.is_empty !Infsys.g then
    Is.normalize ()
  else
    begin
      eval1 ();
      eval ()
    end

and eval1 () =
  try
    let ((atm, rho) as fct) = G.get !Infsys.g in
      (match Atom.atom_of atm with
	 | Atom.TT ->
	     ()
	 | Atom.FF ->
	     raise(Jst.Inconsistent(rho))
	 | Atom.Equal(a, b) ->
	     let e = Fact.Equal.make a b rho in
	       (match Fact.Equal.status e with
		  | Term.Variable -> merge_v e
		  | Term.Pure(i) ->  merge_i i e 
		  | Term.Mixed(i, a) -> abstract fct i a)
	 | Atom.Diseq(a, b) ->
	     let d = Fact.Diseq.make a b rho in
	       (match Fact.Diseq.status d with
		  | Term.Variable -> dismerge_v d
		  | Term.Pure(i) -> dismerge_i i d
		  | Term.Mixed(i, a) -> abstract fct i a)
	 | Atom.Nonneg(a) ->
	     let nn = Fact.Nonneg.make a rho in
	       (match Fact.Nonneg.status nn with
		 | Term.Variable -> nonneg_la nn
		 | Term.Pure(i) when i = Th.la -> nonneg_la nn
		 | Term.Pure(i)  -> abstract fct i (Fact.Nonneg.term_of nn)
		 | Term.Mixed(i, a) -> abstract fct i a)
	 | Atom.Pos(a) ->
	     let pp = Fact.Pos.make a rho in
	       (match Fact.Pos.status pp with
		 | Term.Variable -> pos_la pp
		 | Term.Pure(i) when i = Th.la -> pos_la pp
		 | Term.Pure(i) -> abstract fct i (Fact.Pos.term_of pp)
		 | Term.Mixed(i, a) -> abstract fct i a))
  with
      Not_found -> ()

	    
and merge_v e =
  assert(Fact.Equal.is_var e);
  Partition.merge !Infsys.p e;
  propagate ()

and merge_i i e = 
  assert(Fact.Equal.is_pure i e);
  Is.merge i e;
  propagate ()
    
and abstract fct i a =
  G.put fct !Infsys.g; 
  Is.abstract i a;
  propagate ()
    
	
(** Propagate all fresh variable equalities and 
  disequalities in partitioning [p] to individual 
  procedures. *)
and propagate () =
  try
    let e = Partition.fresh_equal !Infsys.p in
      Is.propagate e;
      propagate ()
  with
      Not_found -> 
	(try
	   let d = Partition.fresh_diseq !Infsys.p in
	     Is.propagate_diseq d;
	     propagate ()
	 with
	     Not_found -> ())
	
and dismerge_v d =
  assert(Fact.Diseq.is_var d);
  Partition.dismerge !Infsys.p d;
  propagate ()

and dismerge_i i d = 
  assert(Fact.Diseq.is_pure i d);
  Is.dismerge i d;
  propagate ()

and nonneg_la nn = 
  assert(Fact.Nonneg.is_pure Th.la nn);
  Is.nonneg nn;
  propagate ()

and pos_la pp = 
  assert(Fact.Pos.is_pure Th.la pp);
  Is.pos pp;
  propagate ()


(** Two terms [a], [b] are disequal in [(e, p)] if asserting
  [a = b] yields a contradiction. This also takes into 
  account all implicit disequalites. *)
and is_diseq_complete ((s, p) as cfg) a b =
  let d = (Atom.mk_diseq (a, b), Jst.dep0) in
    try
      let _ = process d cfg in
	None
    with
	Jst.Inconsistent(rho) -> Some(rho)


(** Search for satisfiable branch. *)
let rec is_sat (s, p) =
  try
    Is.initialize0 s p;
    eval_and_branch ();
    let (s', p') = Is.finalize () in
      Some(s', p')
  with 
      Jst.Inconsistent _ -> None

and eval_and_branch () =
  eval ()

let gc c = ()

let model (s, p) =
  La.model s.E.la

let minimize (s, p) a =
  La.lower (p, s.E.la) a

let maximize (s, p) a =
  La.upper (p, s.E.la) a
