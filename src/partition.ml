
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
  mutable v : V.t;              (* Variable equalities. *)
  mutable d : D.t;              (* Variables disequalities. *)
  mutable c : C.t               (* Constraints. *)
}


let empty = {
  v = V.empty;
  d = D.empty;
  c = C.empty 
}

let copy p = {v = p.v; d = p.d; c = p.c}

(*s Accessors. *)

let v_of s = s.v
let d_of s = s.d
let c_of s = s.c


(*s Destructive Updates. *)

let update_v p v = (p.v <- v; p)
let update_d p d = (p.d <- d; p)
let update_c p c = (p.c <- c; p)


(*s Canonical variables module [s]. *)

let v s = V.find s.v


(*s All disequalities of some variable [x]. *)

let deq s = D.deq s.d


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
  if Term.eq x' y' then 
    Three.Yes
  else if D.is_diseq s.d x' y' then 
    Three.No
  else
    try
      let i = C.apply s.c x in
      let j = C.apply s.c y in
      if Cnstrnt.is_disjoint i j then
	Three.No 
      else 
	Three.X
    with
	Not_found -> Three.X


(*s Test for integerness. *)

let is_int s x = 
  try 
    Cnstrnt.dom_of (C.apply s.c x) = Dom.Int 
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
	  update_v (update_d (update_c s c') d') v'

(*s Add a constraint. *)

let add c s =
  let c' = C.add c s.c in  
    if C.eq s.c c' then s else 
      begin
	Trace.msg "p" "Add" c Fact.pp_cnstrnt;
	  update_c s c'
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
	let c' = C.diseq d s.c in
	  Trace.msg "p" "Diseq" d Fact.pp_diseq;
	  update_d (update_c s c') d'

let restrict xs s = 
  let v' = Set.fold V.restrict xs s.v in
    update_v s v'

(*s Stored facts. *)

let equality p = V.equality p.v
let disequalities p x = D.disequalities p.d (V.find p.v x)
let cnstrnt p x = C.to_fact p.c (V.find p.v x)


(*s Management of changed sets. *)

module Changed = struct

  let reset () =
    V.changed := Set.empty;
    D.changed := Set.empty;
    C.changed := Set.empty

  let save () = (!V.changed, !D.changed, !C.changed)

  let restore (v, d, c) =
    V.changed := v;
    D.changed := d;
    C.changed := c
  
  let stable () =
    !V.changed = Set.empty &&
    !D.changed = Set.empty &&
    !C.changed = Set.empty

end
