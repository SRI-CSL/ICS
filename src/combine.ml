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

open Th

(** {6 Array of Equality Sets} *)

type t = {
  u : U.t;
  a : La.t;
  p : P.t;
  bv : Bv.t;
  cop : Cop.t;
  nl : Nl.t;
  app : L.t;
  arr : Arr.t;
  pset : Pset.t;
}

let eq s1 s2 =  
  U.eq s1.u s2.u &&
  La.eq s1.a s2.a &&
  P.eq s1.p s2.p &&
  Bv.eq s1.bv s2.bv &&
  Cop.eq s1.cop s2.cop &&
  Nl.eq s1.nl s2.nl &&
  L.eq s1.app s2.app &&
  Arr.eq s1.arr s2.arr &&
  Pset.eq s1.pset s2.pset
  

let empty = {
  u = U.empty;
  a = La.empty;
  p = P.empty;
  bv = Bv.empty;
  cop = Cop.empty;
  nl = Nl.empty;
  app = L.empty;
  arr = Arr.empty;
  pset = Pset.empty
}

let copy s = {
  u = U.copy s.u;
  a = La.copy s.a;
  p = P.copy s.p;
  bv = Bv.copy s.bv;
  cop = Cop.copy s.cop;
  nl = Nl.copy s.nl;
  app = L.copy s.app;
  arr = Arr.copy s.arr;
  pset = Pset.copy s.pset
}

let pp i fmt s =
  match i with
    | Uninterpreted -> 
	U.pp fmt s.u
    | Shostak(i) ->
	(match i with
	   | LA -> La.pp fmt s.a
	   | P -> P.pp fmt s.p
	   | BV -> Bv.pp fmt s.bv
	   | COP -> Cop.pp fmt s.cop
	   | SET -> Pset.pp fmt s.pset)
    | Can(i) ->
	(match i with
	   | NL -> Nl.pp fmt s.nl
	   | APP -> L.pp fmt s.app
	   | ARR -> Arr.pp fmt s.arr)

let is_empty s = function 
  | Uninterpreted -> U.is_empty s.u
  | Shostak(i) ->
      (match i with
	 | LA ->  La.is_empty s.a
	 | P -> P.is_empty s.p
	 | BV -> Bv.is_empty s.bv
	 | COP -> Cop.is_empty s.cop
	 | SET -> Pset.is_empty s.pset)
  | Can(i) ->
      (match i with
	 | NL -> Nl.is_empty s.nl
	 | APP -> L.is_empty s.app
	 | ARR -> Arr.is_empty s.arr)

let is_empty_but i s =
  Th.for_all_but i (is_empty s)
       
let is_dependent s = function
  | Uninterpreted -> 
      U.is_dependent s.u
  | Shostak(i)->
      (match i with
	 | LA -> La.is_dependent s.a
	 | P-> P.is_dependent s.p
	 | BV -> Bv.is_dependent s.bv
	 | COP -> Cop.is_dependent s.cop
	 | SET -> Pset.is_dependent s.pset)
  | Can(i) ->
      (match i with
	 | NL -> Nl.is_dependent s.nl
	 | APP -> L.is_dependent s.app
	 | ARR -> Arr.is_dependent s.arr)

let is_independent s = function
  | Uninterpreted -> U.is_independent s.u
  | Shostak(LA)-> La.is_independent s.a 
  | Shostak(P)-> P.is_independent s.p
  | Shostak(BV) -> Bv.is_independent s.bv
  | Shostak(COP) -> Cop.is_independent s.cop
  | Shostak(SET) -> Pset.is_independent s.pset
  | Can(NL) -> Nl.is_independent s.nl
  | Can(APP) -> L.is_independent s.app
  | Can(ARR) -> Arr.is_independent s.arr
   
let occurs s i x =
  is_dependent s i x ||
  is_independent s i x

let apply s =  function
  | Uninterpreted -> U.apply s.u
  | Shostak(LA)-> La.apply s.a
  | Shostak(P)-> P.apply s.p
  | Shostak(BV) -> Bv.apply s.bv
  | Shostak(COP) -> Cop.apply s.cop
  | Shostak(SET) -> Pset.apply s.pset
  | Can(NL) -> Nl.apply s.nl
  | Can(APP) -> L.apply s.app
  | Can(ARR) -> Arr.apply s.arr

let find s = function
  | Uninterpreted -> U.find s.u
  | Shostak(LA)-> La.find s.a
  | Shostak(P)-> P.find s.p
  | Shostak(BV) -> Bv.find s.bv
  | Shostak(COP) -> Cop.find s.cop
  | Shostak(SET) -> Pset.find s.pset
  | Can(NL) -> Nl.find s.nl
  | Can(APP) -> L.find s.app
  | Can(ARR) -> Arr.find s.arr

let inv (p, s) a =
  match Term.App.theory_of a with
    | Uninterpreted -> U.inv s.u a
    | Shostak(LA)-> La.inv s.a a
    | Shostak(P)-> P.inv s.p a
    | Shostak(BV) -> Bv.inv s.bv a
    | Shostak(COP) -> Cop.inv s.cop a
    | Shostak(SET) -> Pset.inv s.pset a
    | Can(NL) -> Nl.inv s.nl a
    | Can(APP) -> L.inv s.app a
    | Can(ARR) -> Arr.inv s.arr a

let dep s = function
  | Uninterpreted -> U.dep s.u
  | Shostak(LA)-> La.dep s.a
  | Shostak(P)-> P.dep s.p
  | Shostak(BV) -> Bv.dep s.bv
  | Shostak(COP) -> Cop.dep s.cop
  | Shostak(SET) -> Pset.dep s.pset
  | Can(NL) -> Nl.dep s.nl
  | Can(APP) -> L.dep s.app
  | Can(ARR) -> Arr.dep s.arr


(** No interpretations for uninterpreted theory! *)
let interp (p, s) i a =
  match i with
    | Uninterpreted -> Jst.Eqtrans.id a 
    | Shostak(LA) -> La.apply s.a a
    | Shostak(P) -> apply s Th.p a 
    | Shostak(BV) -> Bv.apply s.bv a  
    | Shostak(COP) -> Cop.apply s.cop a 
    | Shostak(SET) -> Pset.apply s.pset a 
    | Can(NL) -> Nl.interp (p, s.nl) a
    | _ -> Partition.choose p (apply s i) a


(** Totalized inverse map. *)
let uninterp ((p, _) as cfg) a =
  try
    Jst.Eqtrans.compose (Partition.find p) (inv cfg) a
  with
      Not_found -> Partition.find p a


(** Return a variable for a term, possibly updating the state. *)
let name (p, s) i a = 
  match i with
    | Uninterpreted -> U.name (p, s.u) a
    | Shostak(LA)-> La.name (p, s.a) a
    | Shostak(P)-> P.name  (p, s.p) a
    | Shostak(BV) -> Bv.name  (p, s.bv) a
    | Shostak(COP) -> Cop.name (p, s.cop) a
    | Shostak(SET) -> Pset.name (p, s.pset) a
    | Can(NL) -> Nl.name (p, s.nl) a
    | Can(APP) -> L.name (p, s.app) a
    | Can(ARR) -> Arr.name (p, s.arr) a


(** {6 Predicates} *)

let cheap = ref false

(** Test if terms [a] and [b] are equal or disequal in [s]. *)
let rec is_equal ((p, s) as cfg)  =
  Jst.Rel2.orelse
    (Partition.is_equal_or_diseq p)
    (is_equal_interp cfg)

and is_equal_interp ((p, s) as cfg) a b =
  let pure_of (a, b) =
    match a, b with
      | Term.App(f, _, _), _ ->
	  let i = Sym.theory_of f in
	    if Term.is_pure i a && Term.is_pure i b then i else raise Not_found
      | _, Term.App(f, _, _) ->
	  let i = Sym.theory_of f in
	    if Term.is_pure i a && Term.is_pure i b then i else raise Not_found
      | _ -> 
	  raise Not_found
  in
  try
    (match pure_of (a, b) with
       | Shostak(LA) -> Jst.Rel2.yes (La.is_diseq (p, s.a)) a b
       | Shostak(COP) -> Jst.Rel2.yes (Cop.is_diseq s.cop) a b
       | Shostak(BV) -> Jst.Rel2.yes (Bv.is_diseq s.bv) a b
       | _ -> Jst.Three.X)
  with
      Not_found -> Jst.Three.X

let rec is_pos (p, s) a = 
  let is_pos0 = Jst.Three.of_three Atom.Pos.holds in
    if !cheap || not(Term.is_pure Th.la a) then is_pos0 a else 
      Jst.Rel1.orelse
	is_pos0
	(Jst.Rel1.yes_or_no 
	   (La.is_pos (p, s.a)) 
	   (La.is_nonpos (p, s.a)))
	a

	
let rec is_nonneg (p, s) a =
  let is_nonneg0  = Jst.Three.of_three Atom.Nonneg.holds in
    if !cheap || not(Term.is_pure Th.la a) then is_nonneg0 a else 
      Jst.Rel1.orelse
	is_nonneg0
	(Jst.Rel1.yes_or_no
	   (La.is_nonneg (p, s.a))
	   (La.is_neg (p, s.a)))
	a
  
let is_neg cfg a = is_pos cfg (Arith.mk_neg a)

let is_nonpos cfg a = is_nonneg cfg (Arith.mk_neg a)

let is_nonneg cfg = Jst.Rel1.trace "bar" "is_nonneg" (is_nonneg cfg)


(** {6 Canonization} *)

(** Sigma normal forms for individual theories. These are largely independent
 of the current state, except for sigma-normal forms for arrays, which use
 variable equalities and disequalities. *)
let rec sigma cfg f al =
  let inj sigma op l = (sigma op l, Jst.dep0) in
    match Sym.get f with
      | Sym.Arith(op) ->  inj Arith.sigma op al
      | Sym.Product(op) -> inj  Product.sigma op al
      | Sym.Bv(op) ->  inj Bitvector.sigma op al
      | Sym.Coproduct(op) -> inj Coproduct.sigma op al
      | Sym.Propset(op) -> inj Propset.sigma op al
      | Sym.Fun(op) -> 
	  let rhos = ref [] in
	  let sigma' g bl =
	    let (b, rho) = sigma cfg g bl in
	      rhos := rho :: !rhos; b
	  in
	  let a = Apply.sigma sigma' op al in
	    (a, Jst.dep !rhos)
      | Sym.Pp(op) -> inj Pprod.sigma op al
      | Sym.Uninterp _ -> inj Term.App.mk_app f al
      | Sym.Arrays(op) -> 
	  let rhos = ref [] in
	  let is_equal' = Jst.Three.to_three rhos (is_equal cfg) in
	  let b = Funarr.sigma is_equal' op al in
	    (b, Jst.dep !rhos)

let rec can ((p, s) as cfg) a =
  let interp_can op =            (* Apply interpretation on canonized term. *)
    Jst.Eqtrans.compose_partial1 (* If no interpretation, return canonized result. *)
      (interp cfg (Sym.theory_of op))
      (can cfg)
  in 
    if Term.is_var a then
      Partition.find p a
    else 
      try
	Jst.Eqtrans.compose
	  (uninterp cfg)                         
	  (Jst.Eqtrans.mapargs (sigma cfg) interp_can)
	  a 
      with 
	  Not_found ->
	    Partition.find p a
	    

(** Solver *)
let solve th e =
  match th with
    | Shostak(i) -> 
	(match i with
	   | LA -> Arith.solve e
	   | BV -> Bitvector.solve e
	   | P -> Product.solve e
	   | COP -> Coproduct.solve e
	   | SET -> Propset.solve e)
    | _ -> 
	raise Exc.Incomplete


(** Abstract domain interpretation *)
let dom (p, _) a =
  let hyps = ref [] in
  let rec of_term a =
    match a with
      | Term.Var _ -> 
	  of_var a
      | Term.App(f, al, _) ->
	  (try
	     let op = Sym.Arith.get f in
	       Arith.dom of_term op al
	   with
	       Not_found ->
		 let op = Sym.Pprod.get f in
		   Pprod.dom of_term op al)
  and of_var x =
     let (y, rho) = Partition.find p x in
     let d = Term.Var.dom_of y in
       if not(x == y) then hyps := rho :: !hyps;
       d
  in
  let d = of_term a in
  let rho = Jst.dep !hyps in
    (d, rho)


(** {6 Atom transformers} *)

let map cfg = 
  Fact.map (is_equal cfg, is_nonneg cfg, is_pos cfg)

let simplify cfg = map cfg (can cfg)

(** Return a purified atom. *)
let rec abstract cfg atm =
  let (atm', rho) = map cfg (abstract_term cfg) atm in
  let (atm'', tau) = abstract_atom cfg atm' in
    (atm'', Jst.dep2 rho tau)
    
and abstract_atom cfg atm =
  match Atom.atom_of atm with
    | Atom.Equal(a, b) -> 
	(try
	   let i = Term.App.theory_of a and j = Term.App.theory_of b in
	     if i = j then Fact.mk_holds atm else 
	       let (a', rho') = name cfg i a in
		 (Atom.mk_equal (a', b), rho')
	 with
	     Not_found -> Fact.mk_holds atm)
    | Atom.Diseq(a, b) -> 
	(try
	   let i = Term.App.theory_of a and j = Term.App.theory_of b in
	     if i = j then Fact.mk_holds atm else 
	       let (a', rho') = name cfg i a in
		 (Atom.mk_diseq (a', b), rho')
	 with
	     Not_found -> Fact.mk_holds atm)
    | _ -> 
	Fact.mk_holds atm

and abstract_term ((p, s) as cfg) a =   
  let rec of_args j al =
    let rho = ref Jst.dep0 in
    let trans a =
      let (b, tau) = of_term j a in
	rho := Jst.dep2 tau !rho; b
    in
    let bl = Term.mapl trans al in
      (bl, !rho)
  and of_term i a =
    match a with
      | Term.Var _ ->  
	  Partition.find p a
      | Term.App(f, _, _) when Sym.Fun.is_abs f -> (* abstract lambda terms. *)
	  name cfg Th.app a
      | Term.App(f, al, _) ->
	  let j = Sym.theory_of f in
	  let (bl, rhos) = of_args j al in    (* roughly, [rhok |- bk = ak] *) 
	  let (c, rho) =                      (* [rho |- c = f(b1,...,bn)] *)
	    if Term.eql al bl then 
	      Jst.Eqtrans.id a
	    else 
	      sigma cfg f bl 
	  in
	    if i = Th.u || i = Th.arr || i <> j then
	      let (x, tau) = name cfg j c in  (* [tau |- x = c] *)
	      let sigma = Jst.dep3 tau rho rhos in
		(x, sigma)                    (* [sigma |- x = a] *)
	    else 
	      (c, rho)
  in
    match a with 
      | Term.Var _ -> Jst.Eqtrans.id a
      | Term.App(f, _, _) -> of_term (Sym.theory_of f) a


(** {6 Processing} *)

type config = Partition.t * t

let rec merge th (p, s) e = 
  match th with 
    | Some(i) -> 
	assert(Fact.Equal.is_pure i e);
	process_equal i (p, s) e
    | None ->
	assert(Fact.Equal.is_var e);
	Partition.merge p e;
	let x = Fact.Equal.lhs_of e in
	  Th.iter
	    (fun i -> 
	       if not(is_empty s i) && occurs s i x then
		 process_equal i (p, s) e)
	    
and process_equal th (p, s) e =
  match th with
    | Uninterpreted -> 
	U.merge  (p, s.u) e
    | Shostak(i)->  
	(match i with
	   | LA -> La.merge (p, s.a) e
	   | BV -> Bv.merge (p, s.bv) e
	   | P -> P.merge (p, s.p) e
	   | COP -> Cop.merge (p, s.cop) e
	   | SET -> Pset.merge (p, s.pset) e)
    | Can(i) ->
	(match i with
	   | ARR -> Arr.merge (p, s.arr) e
	   | APP -> L.merge  (p, s.app) e
	   | NL ->  Nl.merge (p, s.nl) e)

let process_nonneg (p, s) =
  La.process_nonneg (p, s.a) 

let process_pos (p, s) =
  La.process_pos (p, s.a) 

let dismerge (p, s) d =
  if Fact.Diseq.is_diophantine d then
    La.dismerge (p, s.a) d
  else 
    let d = Fact.Diseq.to_var (name (p, s)) d in
      Partition.dismerge p d;
      Arr.dismerge (p, s.arr) d;
      Bv.dismerge (p, s.bv) d
	

let propagate_equal ((p, s) as cfg) e =
  if Fact.Equal.is_var e then
    merge None cfg e
  else if Fact.Equal.is_pure Th.bv e then
    merge (Th.inj Th.bv) cfg e
  else if Fact.Equal.is_pure Th.la e then
    Nl.propagate (p, s.a, s.nl) e

let propagate_diseq ((p, s) as cfg) d =
  dismerge cfg d

let propagate_nonneg cfg nn =
  let nn' = Fact.Nonneg.map (can cfg) nn in
    process_nonneg cfg nn'

 
(** Garbage collection. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the non-Shostak theories, since all other 
 solution sets are kept in canonical form. *)
let gc (p, s) =
  let filter x =  
    not (Th.exists 
	   (fun i -> (Th.is_can i || Th.is_uninterpreted i) && 
	      is_dependent s i x))
  in
    Partition.gc filter p

(** {6 Model construction} *)

let model (p, s) = La.model (p, s.a)

let maximize (p, s) = La.upper (p, s.a)
let minimize (p, s) = La.lower (p, s.a)


(** {6 Splits} *)

module Split = struct

  type t = {
    finint: La.Finite.t Term.Var.Map.t;
    arridx: Term.Set2.t
  }

  let is_empty spl =
    Term.Set2.is_empty spl.arridx &&
    Term.Var.Map.empty == spl.finint

  let pp fmt spl =
    if not(spl.finint == Term.Var.Map.empty) then
      (let l = Term.Var.Map.fold (fun x fin acc -> (x, fin) :: acc) spl.finint [] in
	 Pretty.map Term.pp La.Finite.pp fmt l);
    if not(Term.Set2.is_empty spl.arridx) then
      Pretty.set Term.Equal.pp fmt (Term.Set2.elements spl.arridx)

end 

let split (p, s) = {
  Split.finint = La.Finite.of_config (p, s.a);
  Split.arridx = Arr.splits (p, s.arr)
}
