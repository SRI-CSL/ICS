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

(** {6 Fully uninterpreted theory} *)


module U = Eqs.Close(Eqs.Make(
  struct
    let th = Th.u
    let nickname = Th.to_string Th.u
    let rec apply (x, b) = function
      | Term.Var _ as y -> if Term.eq x y then b else y
      | Term.App(f, yl) as a -> 
	  let yl' = Term.mapl (apply (x, b)) yl in
	    if yl == yl' then a else Term.App.mk_app f yl'
  end))

(** {6 Product theory} *)

module P = Eqs.Close(Eqs.Make(
  struct
    let th = Th.p
    let nickname = Th.to_string Th.p
    let apply = Product.apply
  end))


(** {6 Theory of Function Application and Abstraction} *)

module APP = Eqs.Close(Eqs.Make(
  struct
    let th = Th.app
    let nickname = Th.to_string Th.app
    let apply = Apply.apply
  end))


(** {6 Array of Equality Sets} *)

type t = {
  u : U.t;
  a : La.t;
  p : P.t;
  bv : Bv.t;
  cop : Cop.t;
  nl : Nl.t;
  app : APP.t;
  arr : Arr.t
}

let eq s1 s2 =  
  U.eq s1.u s2.u &&
  La.eq s1.a s2.a &&
  P.eq s1.p s2.p &&
  Bv.eq s1.bv s2.bv &&
  Cop.eq s1.cop s2.cop &&
  Nl.eq s1.nl s2.nl &&
  APP.eq s1.app s2.app &&
  Arr.eq s1.arr s2.arr
  

let empty = {
  u = U.empty;
  a = La.empty;
  p = P.empty;
  bv = Bv.empty;
  cop = Cop.empty;
  nl = Nl.empty;
  app = APP.empty;
  arr = Arr.empty
}

let copy s = {
  u = U.copy s.u;
  a = La.copy s.a;
  p = P.copy s.p;
  bv = Bv.copy s.bv;
  cop = Cop.copy s.cop;
  nl = Nl.copy s.nl;
  app = APP.copy s.app;
  arr = Arr.copy s.arr
}

let pp i fmt s =
  match i with
    | Uninterpreted -> U.pp fmt s.u
    | Shostak(A) -> La.pp fmt s.a
    | Shostak(P)-> P.pp fmt s.p
    | Shostak(BV) -> Bv.pp fmt s.bv
    | Shostak(COP) -> Cop.pp fmt s.cop
    | Can(NL) -> Nl.pp fmt s.nl
    | Can(APP) -> APP.pp fmt s.app
    | Can(ARR) -> Arr.pp fmt s.arr

let is_empty s = function 
  | Uninterpreted -> U.is_empty s.u
  | Shostak(A)-> La.is_empty s.a
  | Shostak(P)-> P.is_empty s.p
  | Shostak(BV) -> Bv.is_empty s.bv
  | Shostak(COP) -> Cop.is_empty s.cop
  | Can(NL) -> Nl.is_empty s.nl
  | Can(APP) -> APP.is_empty s.app
  | Can(ARR) -> Arr.is_empty s.arr

let is_empty_but i s =
  Th.for_all_but i (is_empty s)
       

let is_dependent s = function
  | Uninterpreted -> U.is_dependent s.u
  | Shostak(A)-> La.is_dependent s.a
  | Shostak(P)-> P.is_dependent s.p
  | Shostak(BV) -> Bv.is_dependent s.bv
  | Shostak(COP) -> Cop.is_dependent s.cop
  | Can(NL) -> Nl.is_dependent s.nl
  | Can(APP) -> APP.is_dependent s.app
  | Can(ARR) -> Arr.is_dependent s.arr

let is_independent s = function
  | Uninterpreted -> U.is_independent s.u
  | Shostak(A)-> La.is_independent s.a 
  | Shostak(P)-> P.is_independent s.p
  | Shostak(BV) -> Bv.is_independent s.bv
  | Shostak(COP) -> Cop.is_independent s.cop
  | Can(NL) -> Nl.is_independent s.nl
  | Can(APP) -> APP.is_independent s.app
  | Can(ARR) -> Arr.is_independent s.arr
   
let occurs s i x =
  is_dependent s i x ||
  is_independent s i x


let apply s =  function
  | Uninterpreted -> U.apply s.u
  | Shostak(A)-> La.apply s.a
  | Shostak(P)-> P.apply s.p
  | Shostak(BV) -> Bv.apply s.bv
  | Shostak(COP) -> Cop.apply s.cop
  | Can(NL) -> Nl.apply s.nl
  | Can(APP) -> APP.apply s.app
  | Can(ARR) -> Arr.apply s.arr

let find s = function
  | Uninterpreted -> U.find s.u
  | Shostak(A)-> La.find s.a
  | Shostak(P)-> P.find s.p
  | Shostak(BV) -> Bv.find s.bv
  | Shostak(COP) -> Cop.find s.cop
  | Can(NL) -> Nl.find s.nl
  | Can(APP) -> APP.find s.app
  | Can(ARR) -> Arr.find s.arr


(* No interpretations for uninterpreted theory! *)
let interp (p, s) i a =
  match i with
    | Uninterpreted -> Justification.Eqtrans.id a 
    | Shostak(A) -> La.apply s.a a
    | Shostak(P) -> apply s Th.p a 
    | Shostak(BV) -> apply s Th.bv a  
    | Shostak(COP) -> apply s Th.cop a 
    | _ ->
	Partition.choose p (apply s i) a

let inv (p, s) =
  let inv' a = 
    match Term.App.theory_of a with
      | Uninterpreted -> U.inv s.u a
      | Shostak(A)-> La.inv s.a a
      | Shostak(P)-> P.inv s.p a
      | Shostak(BV) -> Bv.inv s.bv a
      | Shostak(COP) -> Cop.inv s.cop a
      | Can(NL) -> Nl.inv s.nl a
      | Can(APP) -> APP.inv s.app a
      | Can(ARR) -> Arr.inv s.arr a
  in
    Justification.Eqtrans.compose (Partition.find p) inv'

let dep s = function
  | Uninterpreted -> U.dep s.u
  | Shostak(A)-> La.dep s.a
  | Shostak(P)-> P.dep s.p
  | Shostak(BV) -> Bv.dep s.bv
  | Shostak(COP) -> Cop.dep s.cop
  | Can(NL) -> Nl.dep s.nl
  | Can(APP) -> APP.dep s.app
  | Can(ARR) -> Arr.dep s.arr

let name (p, s) i a = 
  Trace.msg (Th.to_string i) "Name" a Term.pp;
  match i with
    | Uninterpreted -> U.name (p, s.u) a
    | Shostak(A)-> La.name (p, s.a) a
    | Shostak(P)-> P.name  (p, s.p) a
    | Shostak(BV) -> Bv.name  (p, s.bv) a
    | Shostak(COP) -> Cop.name (p, s.cop) a
    | Can(NL) -> Nl.name (p, s.nl) a
    | Can(APP) -> APP.name (p, s.app) a
    | Can(ARR) -> Arr.name (p, s.arr) a

let rec process_equal (p, s) i e = 
  Trace.msg (Th.to_string i) "Process" e Fact.Equal.pp;
  assert(Fact.Equal.is_pure i e);
  match i with
    | Uninterpreted -> 
	U.fuse  (p, s.u) [e]
    | Shostak(i)-> 
	merge (p, s) i e 
    | Can(ARR) ->
	Arr.process_equal (p, s.arr) e
    | Can(i') -> 
	let e' = Fact.Equal.map (name (p, s) i) e in
	  fuse (p, s) i' e'
	    
and merge (p, s) i e =
  let e' = Fact.Equal.map (replace s i) e in
    match i with
      | A -> La.process_equal (p, s.a) e'
      | BV -> Bv.process_equal (p, s.bv) e'
      | P -> P.compose  (p, s.p) (solve P e')
      | COP -> Cop.process_equal (p, s.cop) e'

and replace s i =
  let inj = Fact.Equal.Inj.replace in
  match i with
    | A -> inj Arith.map (La.find s.a)
    | P -> inj Product.map (P.find s.p)
    | BV -> inj Bitvector.map (Bv.find s.bv)
    | COP -> inj Coproduct.map (Cop.find s.cop)
	
	  
and solve i =
  let slv = match i with
    | A -> Arith.solve
    | P -> Product.solve
    | BV -> Bitvector.solve
    | COP -> Coproduct.solve
  in
    Fact.Equal.Inj.solver slv
      
and fuse (p, s) i e =
  match i with
    | APP -> APP.fuse  (p, s.app) [e]
    | ARR -> failwith "not needed"
    | NL -> Nl.merge (p, s.nl) e


type config = Partition.t * t

let process_nonneg (p, s) =
  La.process_nonneg (p, s.a) 

let process_diseq (p, s) d =
  try
    if Fact.Diseq.is_diophantine d then
      La.process_diseq (p, s.a) d;
    if Fact.Diseq.is_var d then
      begin
	Bv.process_diseq (p, s.bv) d;
	Cop.process_diseq (p, s.cop) d
      end 
  with
      Not_found -> failwith "Error: Not found in process_diseq"

let merge (p, s) i e =
  assert(Fact.Equal.is_var e);
  if occurs s i (Fact.Equal.lhs_of e) then
    process_equal (p, s) i e
 
let dismerge (p, s) i d =
  if i = Th.arr then
    Arr.process_diseq (p, s.arr) d
  else if i = Th.a then
    if not(Fact.Diseq.is_var d) && Fact.Diseq.is_diophantine d then
      La.process_diseq (p, s.a) d

let propagate (p, s) e = 
  function
    | (Shostak(A), Can(NL)) ->            (* from/to *)
	Nl.propagate (p, s.a, s.nl) e
    | _ ->
	()


(** {6 Canonization} *)

let sigma (p, _) = 
  Partition.sigma p

	
let rec can ((p, s) as cfg) a =
  let interp_can op =                      (* Apply interpretation on canonized term. *)
    Justification.Eqtrans.compose_partial1 (* If there is no interpretation, return canonized result. *)
      (interp cfg (Th.of_sym op))
      (can cfg)
  in 
    try
      Justification.Eqtrans.compose
	(uninterp cfg)                                   (* Never fails. *)                
	(Fact.Equal.Inj.mapargs (sigma cfg) interp_can)  (* Fails if not an application. *)
	a 
    with 
	Not_found -> Partition.find p a

and uninterp ((p, _) as cfg) a =
  try
    let (a', rho') = inv cfg a in
    let (a'', rho'') = Partition.find p a' in
    let tau'' = Justification.trans (a'', a', a) rho'' rho' in
      (a'', tau'')
  with
      Not_found -> Partition.find p a
 

(** Tracing [can]. *)
let can s =
  Trace.func "rule" "Can" 
    Term.pp 
    (Pretty.pair Term.pp Justification.pp) 
    (can s)


(** {6 Predicates} *)

(** Test if terms [a] and [b] are equal or disequal in [s]. *)
let is_equal ((p, _) as cfg) =
  Justification.Rel2.apply
    (can cfg)
    (Partition.is_equal_or_diseq p)

let is_pos (p, s) a = 
  match La.is_pos (p, s.a) a with
    | Some(tau) -> 
	Justification.Three.Yes(tau)
    | None -> 
	(match La.is_nonpos (p, s.a) a with
	   | Some(tau) -> Justification.Three.No(tau)
	   | None -> Justification.Three.X)
	
let is_nonneg (p, s) a = 
 match La.is_nonneg (p, s.a) a with
    | Some(tau) -> 
	Justification.Three.Yes(tau)
    | None -> 
	(match La.is_neg (p, s.a) a with
	   | Some(tau) -> Justification.Three.No(tau)
	   | None -> Justification.Three.X)

let is_neg cfg a = is_pos cfg (Arith.mk_neg a)
let is_nonpos cfg a = is_nonneg cfg (Arith.mk_neg a)



(** {6 Solver} *)

let solve i e = 
  match i with
    | Shostak(i) ->
	(match i with
	   | A -> Arith.solve e
	   | P -> Product.solve e
	   | BV -> Bitvector.solve e
	   | COP -> Coproduct.solve e)
    | _ ->
	raise Exc.Incomplete


(** {6 Model construction} *)

let model (p, s) =
  La.model (p, s.a)

let maximize (p, s) = La.maximize (p, s.a)
let minimize (p, s) = La.minimize (p, s.a)


(** {6 Splits} *)

module Split = struct

  type t = {
    finint: La.Finite.t Term.Map.t;
    arridx: Term.Set2.t
  }

  let is_empty spl =
    Term.Set2.is_empty spl.arridx &&
    Term.Map.empty == spl.finint

  let pp fmt spl =
    if not(spl.finint == Term.Map.empty) then
      (let l = Term.Map.fold (fun x fin acc -> (x, fin) :: acc) spl.finint [] in
	 Pretty.map Term.pp La.Finite.pp fmt l);
    if not(Term.Set2.is_empty spl.arridx) then
      Pretty.set Term.Equal.pp fmt (Term.Set2.elements spl.arridx)

end 

let split (p, s) = {
  Split.finint = La.Finite.of_config (p, s.a);
  Split.arridx = Arr.splits (p, s.arr)
}
