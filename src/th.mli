
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


(*s The interpreted theories are linear arithmetic [LA], tuples [T],
 bitvectors [BV], and nonlinear arithmetic [NLA]. *)

type i = A | T | BV

val name_of : i -> string


(*s For an interpreted function symbol [op], [index op] returns the 
 corresponding name of the interpreted theory. *)

val index : Sym.interp -> i


(*s [sigma op l] is the combined sigmatizer for the interpreted theories. *)

val sigma : i -> Sym.t -> Term.t list -> Term.t

(*s Component solvers. *)

val solve : i -> Term.t * Term.t -> (Term.t * Term.t) list

(*s Type of the context for the interpreted theories. *)


type t

(*s Accessors. *)

val la_of : t -> Term.t Term.Map.t
val t_of : t -> Term.t Term.Map.t
val bv_of : t -> Term.t Term.Map.t

(*s Empty state. *)

val empty : t

(*s Return the solution set for a theory. *)

val solution : i -> t -> (Term.t * Term.t) list

(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit

(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
  [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
  is no binding with domain [a]. [inv s b] returns [a] if there is a binding
  [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
  contain [a] as a subterm. *)

val find : i -> t -> Term.t -> Term.t

val inv : i -> t -> Term.t -> Term.t

val use : i -> t -> Term.t -> Term.Set.t

(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

val extend : i -> Term.t -> t -> Term.t * t

(*s [process solve f (a,b) s] installs an equality [a = b], where at least one of [a],[b] is
  a tuple term, into the tuple context [s]. Abstractly, the manipulations on [s] can be 
  described by [s o solve(norm s a, norm s b)], where the composition [o] operator 
  includes new bindings [x |-> e] for each equality in the solved form 
  [solve(norm s a, norm s b)], and all rhs of [s] are normalized, using [norm] above, 
  with respect to the this solved form.  In addition, if the rhs of a newly introduced 
  binding reduces to an uninterpreted term (excluding fresh variables), then the 
  corresponding binding is removed and returned as a newly infered equality between 
  uninterpreted terms. *)

type cnstrnt = Term.t -> Number.t option

val merge : i -> cnstrnt -> (Term.t * Term.t) -> t 
               -> (t * V.eqs * Atom.t list)

val merge_all : cnstrnt -> (Term.t * Term.t) -> t 
                   -> (t * V.eqs * Atom.t list)

(*s Generate new constraints. *)

val propagate : cnstrnt -> (Term.t * Number.t) -> t -> Atom.t list

(*s Build new context by replacing all variables with "canonical" 
 variables. *)

val inst : (Term.t -> Term.t) -> t -> t





