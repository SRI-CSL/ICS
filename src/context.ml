
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
open Partition
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  v : V.t;              (* Variable equalities. *)
  d : D.t;              (* Variables disequalities. *)
  u : U.t;              (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  v = V.empty;
  d = D.empty;
  u = U.empty;
  i = Th.empty
}

(*s Canonical variables module [s]. *)

let v s = V.find s.v

(*s Constraint of [a] in [s]. *)

let cnstrnt s = Th.cnstrnt (s.v, s.i)

(*s All disequalities of some variable [x]. *)

let deq s = D.deq s.d


(*s Pretty-printing. *)
  
let pp fmt s =
  V.pp fmt s.v;
  D.pp fmt s.d;
  U.pp fmt s.u;
  Th.pp fmt s.i


(*s Equality test. *)

let is_equal s x y =
  let x' = v s x in
  let y' = v s y in
  if Term.eq x' y' then Three.Yes
  else if D.is_diseq s.d x' y' then Three.No
  else Three.X

(*s Test for integerness. *)

let is_int s x = 
  try Cnstrnt.dom_of (cnstrnt s x) = Dom.Int with Not_found -> false

(*s Parameterized operations. *)

let inv i s = 
  match i with
    | Theories.Uninterp -> U.inv s.u
    | Theories.Interp(i) -> Th.inv i s.i

let find i s x =
  match i with
    | Theories.Interp(i) -> Th.find i s.i x
    | Theories.Uninterp -> U.find s.u x

let use i s = 
  match i with
    | Theories.Interp(i) -> Th.use i s.i
    | Theories.Uninterp -> U.use s.u


	
(*s Return solution sets. *)

let solutions e s =
  match e with
    | Theories.Uninterp -> U.solutions s.u
    | Theories.Interp(i) -> Th.solutions i s.i 


(*s Variable partitioning. *)

let partition s = (s.v, s.d)


(*s Sigmatization. *)

let sigma s f =
  match Interp.index f with
    | None -> U.sigma (s.v,s.d,s.u) f
    | Some _ -> Th.sigma f


(* Merge a variable equality. *)

let merge e s =
  let (v', vfocus') = V.merge e s.v in
  let s' = {s with v = v'} in
  let focus' = Focus.add_v vfocus' Focus.empty in
  (s', focus')


(*s Add a constraint. *)

let add c s =
  let (v', d', i', focus') = Th.add c (s.v, s.d, s.i) in
  let s' = {s with v = v'; d = d'; i = i'} in
  (s', focus')


(*s Add a disequality. *)

let diseq d s =
  let (d', dfocus') = D.add d s.d in
  let s' = {s with d = d'} in
  let focus' = Focus.add_d dfocus' Focus.empty in
  (s', focus')


(*s Close. *)

let close (s, focus) =
  let (v1, d1, u1, focus1) = U.close (s.v, s.d, s.u, focus) in
  let (v2, d2, i2, focus2) = Th.close (v1, d1, s.i, focus) in
  let s' = {s with v = v2; d = d2; u = u1; i = i2} in
  let focus' = Focus.union focus1 focus2 in
  (s', focus')


(*s Abstracting a term [a] in theory [i]
 by introducing a new name for it. *)

let extend s a =
  match Theories.index a with
    | Theories.Uninterp -> 
	let (x',u') = U.extend a s.u in
	({s with u = u'}, x')
    | Theories.Interp(th) ->
	let (x',i') = Th.extend th  a s.i in
	({s with i = i'}, x')


(*s List all constraints with finite extension. *)

let split s =
  Th.split s.i @ Atom.Set.elements (U.split (s.v,s.d,s.u))


(*s Compression. *)

let compress s =
  let v' = V.external_of s.v in
  let d' = D.inst (V.find s.v) s.d in
  let u' = U.inst s.v s.u in
  let i' = Th.inst s.v s.i in
  {s with v = v'; d = d'; u = u'; i = i'}

