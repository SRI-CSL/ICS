
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
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
open Three
(*i*)

(*s Equalities and disequalities over variables and constraints on variables *)

type t = {
  v : V.t;              (* Variable equalities. *)
  d : D.t;              (* Variables disequalities. *)
  c : C.t               (* Constraints. *)
}

let empty = {
  v = V.empty;
  d = D.empty;
  c = C.empty 
}

(*s Canonical variables module [s]. *)

let v s = V.find s.v

(*s All disequalities of some variable [x]. *)

let deq s = D.deq s.d

(*s Constraint of [a] in [s]. *)

let cnstrnt s = C.cnstrnt s.c


(*s Pretty-printing. *)
  
let pp fmt s =
  V.pp fmt s.v;
  D.pp fmt s.d;
  C.pp fmt s.c

(*s Test if states are unchanged. *)

let unchanged s t =
  V.unchanged s.v t.v &&
  D.unchanged s.d t.d &&
  C.unchanged s.c t.c
 

(*s Equality test. *)

let is_equal s x y =
  let x' = v s x in
  let y' = v s y in
  if Term.eq x' y' then Three.Yes
  else if D.is_diseq s.d x' y' then Three.No
  else
    try
      if Cnstrnt.is_disjoint (C.cnstrnt s.c x) (C.cnstrnt s.c y) then 
	Three.No 
      else 
	Three.X
    with
	Not_found -> Three.X


(*s Test for integerness. *)

let is_int s x = 
  try Cnstrnt.dom_of (C.cnstrnt s.c x) = Dom.Int with Not_found -> false


(* Merge a variable equality. *)

let merge e s =
  Trace.msg "p" "Merge" e Fact.pp_equal;
  let v' = V.merge e s.v in
  let d' = D.merge e s.d in
  let c' = C.merge e s.c in
  let s' = {s with v = v'; d = d'; c = c'} in
  s'


(*s Add a constraint. *)

let add c s =
  Trace.msg "p" "Add" c Fact.pp_cnstrnt;
  let c' = C.add c s.c in
  let s' = {s with c = c'} in
  s'

(*s Add a disequality. *)

let diseq d s = 
  Trace.msg "p" "Diseq" d Fact.pp_diseq;
  let d' = D.add d s.d in
  let s' = {s with d = d'} in
  s'

(*s Resetting the [changed] indices. *)

let reset s = 
  {s with 
     v = V.reset s.v;
     d = D.reset s.d;
     c = C.reset s.c}
