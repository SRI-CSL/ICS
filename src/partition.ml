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


(** Sets of equalities and disequalities over variables.
  See also modules {!V.t} and {!D.t}. *)

type t = {
  mutable v : V.t;              (* Variable equalities. *)
  mutable d : D.t               (* Variables disequalities. *)
}

let v_of p = p.v
let d_of p = p.d


(** Empty partition. *)
let empty = {v = V.empty; d = D.empty}

(** Pretty-printing *)
let pp fmt p = V.pp fmt p.v; D.pp fmt p.d

(** Test if states are unchanged. *)
let eq p q = V.eq p.v q.v && D.eq p.d q.d


(** Choose in equivalence class. *)

let choose p apply y = 
  let rhs x = try Some(apply x) with Not_found -> None in
    V.choose p.v rhs y

let iter_if p f y =
  let f' x = try f x with Not_found -> () in
    V.iter p.v f y
  

(** Canonical variables module [p]. *)
let find p = V.find p.v


(** All disequalities of some variable [x]. *)
let diseqs p x =
  let (y, rho) = find p x in                   (* [rho |- x = y] *)
  let ds = D.diseqs p.d y in
    if Term.eq x y then ds else
      D.Set.fold 
	(fun (z, tau) ->                       (* [tau |- y <> z] *)
	   let sigma = Jst.subst_diseq (x, z) tau [rho] in
	     D.Set.add (z, sigma))
	ds D.Set.empty


(** Abstract domain interpretation *)
let dom p a =
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
     let (y, rho) = find p x in
     let d = Term.Var.dom_of y in
       if not(x == y) then hyps := rho :: !hyps;
       d
  in
  let d = of_term a in
  let rho = Jst.dependencies !hyps in
    (d, rho)



(** {6 Predicates} *)

(** Apply the equality test on variables only. *)
let is_equal p x y =
  if Term.is_var x && Term.is_var y then
    V.is_equal p.v x y
  else 
    None


(** Apply the equality test on canonical variables. *)
let is_diseq p = 
  Jst.Pred2.apply 
    (find p) 
    (D.is_diseq p.d)


(** Test for equality or disequality of canonical variables. *)
let is_equal_or_diseq p x y =
  if Term.is_var x && Term.is_var y then
    Jst.Rel2.apply
      (find p)
      (Jst.Rel2.of_preds
	 (V.is_equal p.v)            (* positive test *)
	 (D.is_diseq p.d))           (* negative test *)
      x y
  else 
    Jst.Three.X


(** Test whether [x] is known to be in domain [d] by looking up
  the domain of the corresponding canonical variable [x']. The
  variable ordering ensures that [x'] is 'more constraint' than [x]. *)
let is_in p d x = 
  let (x', rho') = find p x in
    try
      let d' = Term.Var.dom_of x in
	if Dom.sub d d' then
	  Jst.Three.Yes(rho')
	else if Dom.disjoint d d' then
	  Jst.Three.No(rho')
	else 
	  Jst.Three.X
    with
	Not_found -> 
	  Jst.Three.X



(** {6 Updates} *)

(** Shallow copy for protecting against destructive 
  updates in [merge], [diseq], and [gc]. *)
let copy p = {v = p.v; d = p.d}


(** Merge a variable equality. *)
let rec merge p e =  
  let (x, y, rho) = Fact.Equal.destruct e in  (* to do: add variables for *)
    merge1 p e                                (* domains. *)
	
and merge1 p e =
  let e' = Fact.Equal.map (find p) e in   (* Merge with old canonical forms. *)
    assert(Fact.Equal.both (V.is_canonical p.v) e');
    p.d <- D.merge e' p.d;
    p.v <- V.merge e' p.v
(*
    let e'' = Fact.Equal.map_rhs (find p) e' in (* Propagate new can form *)
      p.d <- D.merge e'' p.d                    (* into disequalities. *)
*)


(** Add a disequality of the form [x <> y]. *)
let dismerge p d =  
  let d' = Fact.Diseq.map (find p) d in
  let (x', y', rho') = Fact.Diseq.destruct d' in
    if Term.eq x' y' then
      raise(Jst.Inconsistent(rho'))
    else 
      p.d <- D.add d' p.d
 


(** {6 Garbage collection} *)

(** Garbage collection of noncanonical variables satisfying [f]. Since variable
  disequalities and constraints are always in canonical form, only variable equalities
  need to be considered. *)
let gc f p = 
  let v' = V.gc f p.v in
    p.v <- v'

