
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
 * Author: Harald Ruess
 i*)

(*s An arithmetic context consists of a solution set of equations
 of the form [x = a], where [x] is a variable and [a] is an arithmetic
 term not containing any of the rhs, and [c] is a set of domain constraints
 for variables. *)

type t

(*s Accessors. [solution s] returns a list of equalities in the form [(x,a)],
 where [x] is a variable and [a] is an arithmetic term fulfilling [Arith.is_arith].
 [domains s] returns a list of constraints [(x,c)], where [x] is a variable and
 [c] is a constraints, with the meaning that [x] is interpreted over the domain
 corresponding to [c]. *)

val solutions : t -> Solution.t

val cnstrnts : t -> C.t


(*s Empty state. *)

val empty : t

val is_empty : t -> bool


(*s Various Accessors for the solution set. *)

val apply : t -> Term.t -> Term.t
val find : t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.Set.t

(*s Constraint. *)

val cnstrnt : V.t * t -> Term.t -> Cnstrnt.t


(*s [extend a s] installs a new equality [x = a], where
  [x] is fresh, into [s]. It assumes that [a] is not yet in the 
  codomain of the solution set in [s]. *)

val extend : Term.t -> t -> Term.t * t
            
(*s Add a new constraint. *)

val add : Fact.cnstrnt -> V.t * D.t * t -> V.t * D.t * t * Focus.t

(*s Close. *)

val close : V.t * D.t * t * Focus.t -> V.t * D.t * t * Focus.t

(*s Instantiate domain variables with canonical representatives. *)

val inst : V.t -> t -> t

(*s Split *)

val split : t -> (Term.t * Cnstrnt.t) list
