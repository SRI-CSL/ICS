
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

(*s Module [Th]: Datatype for manipulating logical contexts 
 of interpreted theories. *)


module type S = sig

  type t

(*s [subst_of s] returns a substitution with bindings of the form [x |-> a],
  where [x] is a power product (but not a fresh variable), and [a] is a
  tupleterm or a fresh variable.  Substitution represents an
  tuple context by interpreting bindings as equalities [x = a]. *)

  val subst_of : t -> Term.t Term.map

(* [use_of s a] returns the set of right-hand side terms of s, in which
   [a] occurs as a subterm. *)

  val use_of : t -> Term.set Term.map

(*s The empty context. *)

  val empty: t

(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
  [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
  is no binding with domain [a]. [inv s b] returns [a] if there is a binding
  [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
  contain [a] as a subterm. *)

  val apply: t -> Term.t -> Term.t        (* may throw [Not_found] *)
  val find: t -> Term.t -> Term.t
  val inv : t -> Term.t -> Term.t         (* may throw [Not_found] *)
  val use : t -> Term.t -> Term.set


(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

  val extend : Term.t * Term.t -> t -> t

(*s [process solve f (a,b) s] installs an equality [a = b], where at least one of [a],[b] is
  a tuple term, into the tuple context [s]. Abstractly, the manipulations on [s] can be 
  described by [s o solve(norm s a, norm s b)], where the composition [o] operator 
  includes new bindings [x |-> e] for each equality in the solved form 
  [solve(norm s a, norm s b)], and all rhs of [s] are normalized, using [norm] above, 
  with respect to the this solved form.  In addition, if the rhs of a newly introduced 
  binding reduces to an uninterpreted term (excluding fresh variables), then the 
  corresponding binding is removed and returned as a newly infered equality between 
  uninterpreted terms. *)

  val process : Term.t * Term.t -> t -> t

  val propagate: Term.t * Term.t -> t -> t

end


module A : S       (* Linear arithmetic. *)
module T : S       (* Tuple theory. *)
module BV : S      (* Bitvectors. *)
module NLA : S     (* Nonlinear arithmetic. *)
