
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

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  u : Cc.t;             (* Congruence closure data structure. *)
  i : Th.t;             (* Interpreted theories. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  u = Cc.empty;
  i = Th.empty;
}

(*s Canonical variables module [s]. *)

let v s = Cc.v s.u

(*s Constraint of [a] in [s]. *)

let cnstrnt s = Th.cnstrnt s.i

let deq s = Cc.diseqs s.u


(*s Pretty-printing. *)
  
let pp fmt s =
  Cc.pp fmt s.u;
  Th.pp fmt s.i

(*s Equality test. *)

let is_equal s a b = 
  match Cc.is_equal s.u a b with
    | Yes -> Yes
    | No -> No
    | X when Term.is_diseq a b -> No
    | X ->
	try
	  if Cnstrnt.cmp (cnstrnt s a) (cnstrnt s b) = Binrel.Disjoint then
	    No
	  else 
	    X
	with
	    Not_found -> X

(*s Test for integerness. *)

let is_int s x = 
  try
    let c = cnstrnt s x in
    Cnstrnt.dom_of c = Dom.Int
  with
      Not_found -> false

(*s Parameterized operations. *)

let inv i s = 
  match i with
    | Theories.Uninterp -> Cc.inv s.u 
    | Theories.Interp(i) -> Th.inv i s.i

let find i s x =
  match i with
    | Theories.Interp(i) -> Th.find i s.i x
    | Theories.Uninterp -> Cc.find s.u x

let use i s = 
  match i with
    | Theories.Interp(i) -> Th.use i s.i
    | Theories.Uninterp -> Cc.use s.u


	
(*s Return solution sets. *)

let solution e s =
  match e with
    | Theories.Uninterp -> Cc.solution s.u
    | Theories.Interp(i) -> Th.solution i s.i 


(*s Variable partitioning. *)

let partition s = Cc.partition s.u


(*s Sigmatization. *)

let sigma s f =
  match Interp.index f with
    | None -> App.sigma f
    | Some _ -> Th.sigma f



(*s List all constraints with finite extension. *)

let split s =
  Th.split s.i @ Cc.split s.u


(*s Compression. *)

let compress s =
  {s with u = Cc.compress s.u}

