
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


(*s Equalities and disequalities over variables and constraints on variables *)

type t = {
  v : V.t;              (* Variable equalities. *)
  d : D.t;              (* Variables disequalities. *)
  c : C.t               (* Constraints. *)
}

val empty : t
 
(*s Canonical variables module [s]. *)

val v : t -> Term.t -> Term.t

(*s All disequalities of some variable [x]. *)

val deq : t -> Term.t -> Term.Set.t

(*s Constraint of [a] in [s]. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t


(*s Pretty-printing. *)
  
val pp : t Pretty.printer




(*s Equality test. *)

val is_equal : t -> Term.t -> Term.t -> Three.t
 

(*s Test for integerness. *)

val is_int : t -> Term.t -> bool


(* Merge a variable equality. *)

val merge : Fact.equal -> t -> t

(*s Add a constraint. *)

val add : Fact.cnstrnt -> t -> t
  
(*s Add a disequality. *)

val diseq : Fact.diseq -> t -> t
 
(*s Test if states are unchanged. *)

val unchanged : t -> t -> bool

(*s Resetting the [changed] indices. *)

val reset : t -> t
 
