
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

(*s Updates. *)

let update_v s v = if V.eq v s.v then s else {s with v = v}
let update_d s d = if D.eq d s.d then s else {s with d = d}
let update_c s c = if C.eq c s.c then s else {s with c = c}


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

let eq s t =
  V.eq s.v t.v &&
  D.eq s.d t.d &&
  C.eq s.c t.c
 

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
  try 
    Cnstrnt.dom_of (C.cnstrnt s.c x) = Dom.Int 
  with 
      Not_found -> false


(* Merge a variable equality. *)

let merge e s =
  let (x, y, _) = Fact.d_equal e in
  match is_equal s x y with
    | Three.Yes -> s
    | Three.No -> raise Exc.Inconsistent
    | Three.X ->
	Trace.msg "p" "Merge" e Fact.pp_equal;
	let v' = V.merge e s.v in
	let d' = D.merge e s.d in
	let c' = C.merge e s.c in
	let s' = {s with v = v'; d = d'; c = c'} in
	s'


(*s Add a constraint. *)

let add c s =
  let c' = C.add c s.c in
    if C.eq s.c c' then s 
    else
      begin
	Trace.msg "p" "Add" c Fact.pp_cnstrnt;
	let s' = {s with c = c'} in
	  s'
      end

(*s Add a disequality. *)

let diseq d s = 
  let (x, y, _) = Fact.d_diseq d in
  match is_equal s x y with
    | Three.Yes -> 
	raise Exc.Inconsistent
    | Three.No -> 
	s
    | Three.X -> 
	let d' = D.add d s.d in
	  if D.eq s.d d' then s else 
	    begin
	      Trace.msg "p" "Diseq" d Fact.pp_diseq;
	      {s with d = d'}
	    end 

(*s Remove noncanonical, internal variables. Assumes that [d] and [c]
 do not contain any of these variables. *)

let removable s = V.removable s.v

let restrict xs s = 
  let v' = Set.fold V.restrict xs s.v in
  {s with v = v'}

(*s Triple of changes in the equality, disequality, and constraint parts
 (in this order). *) 

type index = V | D | C

let changed s =
  (V.changed s.v, D.changed s.d, C.changed s.c)

let changed_v s = V.changed s.v
let changed_d s = D.changed s.d
let changed_c s = C.changed s.c



(*s Resetting the [changed] indices. *)

let reset i s = 
  match i with
    | V -> {s with v = V.reset s.v}
    | D -> {s with d = D.reset s.d}
    | C -> {s with c = C.reset s.c}

let reset_v s = update_v s (V.reset s.v)
let reset_d s = update_d s (D.reset s.d)
let reset_c s = update_c s (C.reset s.c)




