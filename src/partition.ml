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

open Term
open Three

(** Equalities and disequalities over variables and constraints on variables *)

type t = {
  mutable v : V.t;              (* Variable equalities. *)
  mutable d : D.t               (* Variables disequalities. *)
}


let empty = {
  v = V.empty;
  d = D.empty
}

let copy p = {v = p.v; d = p.d}

(** Accessors. *)

let v_of s = s.v
let d_of s = s.d


(** Destructive Updates. *)

let update_v p v = (p.v <- v; p)
let update_d p d = (p.d <- d; p)

(** Canonical variables module [s]. *)

let v s = V.find s.v

(** All disequalities of some variable [x]. *)

let deq s = D.deq s.d

(** Pretty-printing. *)
  
let pp fmt s =
  V.pp fmt s.v;
  D.pp fmt s.d

(** Test if states are unchanged. *)

let eq s t =
  V.eq s.v t.v &&
  D.eq s.d t.d
 

(** Equality test. *)

let is_equal s x y =
  let x' = v s x in
  let y' = v s y in
  if Term.eq x' y' then 
    Three.Yes
  else if D.is_diseq s.d x' y' then 
    Three.No
  else
    Three.X

(* Merge a variable equality. *)

let merge e s =
  let (x, y, _) = Fact.d_equal e in
  match is_equal s x y with
    | Three.Yes -> 
	s
    | Three.No -> 
	raise Exc.Inconsistent
    | Three.X ->
	Trace.msg "p" "Merge" e Fact.pp_equal;
	let v' = V.merge e s.v in
	let d' = D.merge e s.d in
	  update_v (update_d s d') v'


(** Add a disequality. *)

let diseq d s = 
  let (x, y, _) = Fact.d_diseq d in
  match is_equal s x y with
    | Three.Yes -> 
	raise Exc.Inconsistent
    | Three.No -> 
	s
    | Three.X -> 
	let d' = D.add d s.d in
	  Trace.msg "p" "Diseq" d Fact.pp_diseq;
	  update_d s d'

let restrict xs s = 
  let v' = Set.fold V.restrict xs s.v in
    update_v s v'

(** Stored facts. *)

let equality p = V.equality p.v
let disequalities p = D.disequalities p.d
