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
  mutable d : D.t;              (* Variables disequalities. *)
  mutable c : C.t               (* Variable constraints. *)
}

(** Empty partition. *)
let empty = {
  v = V.empty;
  d = D.empty;
  c = C.empty
}

(** Pretty-printing *)
let pp fmt s =
  V.pp fmt s.v;
  D.pp fmt s.d;
  C.pp fmt s.c

(** Test if states are unchanged. *)
let eq s t =
  V.eq s.v t.v &&
  D.eq s.d t.d &&
  C.eq s.c t.c


(** {6 Changed variables} *)

type changed = {
   chv: Term.Set.t;
   chd: Term.Set.t;
   chc: Term.Set.t
}

let nochange = {
  chv = Term.Set.empty;
  chd = Term.Set.empty;
  chc = Term.Set.empty
}

let is_unchanged ch =
  ch.chv == Term.Set.empty &&
  ch.chd == Term.Set.empty &&
  ch.chc == Term.Set.empty


(** {6 Accessors} *)

let v_of s = s.v
let d_of s = s.d
let c_of s = s.c

(** Canonical variables module [s]. *)
let v s = V.find s.v

(** All disequalities of some variable [x]. *)
let d s = D.d s.d

(** Constraint of a variable. *)
let c s = C.apply s.c

 
(** {6 Equality test} *)

let is_equal s x y =
  let (x', _) = v s x in
  let (y', _) = v s y in
    if Term.eq x' y' then 
      Three.Yes
    else if D.is_diseq s.d x' y' then 
      Three.No
    else
      try
	let (i, _) = c s x 
	and (j, _) = c s y in
	  if Sign.is_zero i && Sign.is_zero j then
	    Three.Yes
	  else if Sign.disjoint i j then
	    Three.No
	  else 
	    Three.X
      with
	  Not_found -> Three.X


(** {6 Updates} *)

let update_v p v = (p.v <- v; p)
let update_d p d = (p.d <- d; p)
let update_c p c = (p.c <- c; p)


(** Shallow copy for protecting against destructive updates. *)
let copy p = {v = p.v; d = p.d; c = p.c}


(** Merge a variable equality. *)
let merge e s = 
  Trace.msg "p" "Merge" e Fact.pp_equal;
  let (x, y, _) = Fact.d_equal e in
    match is_equal s x y with
      | Three.Yes -> 
	  (nochange, s)
      | Three.No -> 
	  raise Exc.Inconsistent
      | Three.X ->
	  let (chv', v') = V.merge e s.v
	  and (chd', d') = D.merge e s.d
	  and (chc', c') = C.merge e s.c in
	  let ch' = {chv = chv'; chd  = chd'; chc = chc'} in
	  let s' = update_c (update_v (update_d s d') v') c' in
	    (ch', s')

(** Adding a constraint *)
let add c s =
  Trace.msg "p" "Add" c Fact.pp_cnstrnt;
  let (chc', c') = C.add c s.c in
  let ch' = {nochange with chc = chc'} in
  let s' = update_c s c' in
    (ch',  s')
  

(** Add and propagate disequalities of the form [x <> y]. *)
let rec diseq d s =  
  Trace.msg "p" "Diseq" d Fact.pp_diseq;
  let (x, y, _) = Fact.d_diseq d in
    match is_equal s x y with
      | Three.Yes -> 
	  raise Exc.Inconsistent
      | Three.No -> 
	  (nochange, s)
      | Three.X -> 
	  let (chd', d') = D.add d s.d in
	  let (chc', c') = C.diseq d s.c in
	  let ch' = {nochange with chd = chd'; chc = chc'} in
	  let s' = update_c (update_d s d') c' in
	    (ch', s')

(** Garbage collection. *)
let gc f s = 
  let v' = V.gc f s.v in
    update_v s v'
