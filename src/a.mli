
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

val solutions : t -> (Term.t * Term.t) list

val domains : t -> (Term.t * Cnstrnt.t) list

(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit


(*s Empty state. *)

val empty : t


(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
  [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
  is no binding with domain [a]. [inv s b] returns [a] if there is a binding
  [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
  contain [a] as a subterm. *)

val apply : t -> Term.t -> Term.t
val find : t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.Set.t


(*s List all constraints with finite extension. *)

val split : t -> (Term.t * Cnstrnt.t) list

(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

val extend : Term.t -> t -> Term.t * t

(*s [merge (a,b) s] installs an equality [a = b] into [s]. *)

val merge : Veq.t -> t -> (t * Veqs.t)


(*s Add a new constraint. *)

val add : (Term.t * Cnstrnt.t) -> t -> (t * Veqs.t)

(*s Constraint. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t

(*s Test for internal parameters. *)

val is_slack : Term.t -> bool
