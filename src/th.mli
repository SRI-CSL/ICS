
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

(*s Module [Th]: Datatype for manipulating logical contexts 
 of interpreted theories. *)


(*s [sigma op l] is the combined sigmatizer for the interpreted theories. *)

val sigma : Sym.t -> Term.t list -> Term.t

(*s Component solvers. *)

type solvedform =
  | Solved of (Term.t * Term.t) list
  | Unsat
  | Unsolved

val solve : Interp.t -> Term.t * Term.t -> solvedform

(*s Type of the context for the interpreted theories. *)


type t

(*s Empty state. *)

val empty : t

(*s Return the solution set for a theory. *)

val solutions : Interp.t -> t -> Solution.t

(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit

(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
  [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
  is no binding with domain [a]. [inv s b] returns [a] if there is a binding
  [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
  contain [a] as a subterm. *)

val find : Interp.t -> t -> Term.t -> Term.t

val inv : Interp.t -> t -> Term.t -> Term.t

val use : Interp.t -> t -> Term.t -> Term.Set.t

(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

val extend : Interp.t -> Term.t -> t -> Term.t * t


(*s Add a constraint. *)

val add : Fact.cnstrnt -> V.t * D.t * t -> V.t * D.t * t * Focus.t

(*s Propagating new equalities. *)

val close: V.t * D.t * t * Focus.t -> V.t * D.t * t * Focus.t

(*s Constraint. *)

val cnstrnt : V.t * t -> Term.t -> Cnstrnt.t


(*s List all constraints with finite extension. *)

val split : t -> Atom.t list

(*s Instantiation. *)

val inst : V.t -> t -> t
