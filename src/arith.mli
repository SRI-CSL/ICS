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
 * 
 * Author: Harald Ruess
 *)

(** Module [Arith]: Constructors, recognizers, and accessors for
  linear arithmetic terms. Solvers for rational and integer arithmetic.
  Processing arithmetic equalities.

  A linear arithmetic term is built-up from rational constants,
  linear multiplication of a rational with a variable, and n-ary
  addition. 

  Linear arithmetic terms are always normalized as a sum-of-product
  [q0 + q1*x1+...+qn*xn] where the [qi] are rational constants and the
  [xi] are variables (or any other term not interpreted in this
  theory), which are ordered such that {!Term.cmp}[(xi, xj)] is
  greater than zero for [i < j]. This implies that any such variable
  occurs at most once. In addition, [qi], for [i > 0], is never zero.
  If [qi] is one, we just write [xi] instead of [qi * xi], and if [q0]
  is zero, it is simply omitted in the sum-of-product above.
 *)


val is_interp: Term.t -> bool
(** [is_interp a] holds iff [a] is a inear arithmetic terms; that is,
   [a] is either a numeral (constructed with {!mk_num}), a linear 
   multiplication ({!mk_multq}), or an addition ({!mk_add}). *)



val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
(** [fold f a e] applies [f] at uninterpreted positions of [a] and 
 accumulates the results starting with [e]. *)


(** Some normalization functions. *)

val poly_of : Term.t -> Mpa.Q.t * Term.t list
val of_poly : Mpa.Q.t -> Term.t list -> Term.t

val mono_of : Term.t -> Mpa.Q.t * Term.t
val of_mono : Mpa.Q.t -> Term.t -> Term.t

val monomials : Term.t -> Term.t list


(** {Constructors.} *)

val mk_num  : Mpa.Q.t -> Term.t
val mk_zero : Term.t
val mk_one  : Term.t
val mk_two  : Term.t


val mk_add  : Term.t -> Term.t -> Term.t
val mk_addl : Term.t list -> Term.t
val mk_incr : Term.t -> Term.t
val mk_sub  : Term.t -> Term.t -> Term.t
val mk_neg  : Term.t -> Term.t
val mk_addq : Mpa.Q.t -> Term.t -> Term.t
val mk_multq: Mpa.Q.t -> Term.t -> Term.t


(** {Recognizers}. *)

val is_num : Term.t -> bool

val is_zero : Term.t -> bool
val is_one : Term.t -> bool
val is_q : Mpa.Q.t -> Term.t -> bool

val is_multq : Term.t -> bool

(** Destructors. *)

val d_num : Term.t -> Mpa.Q.t option
val d_add : Term.t -> Term.t list option
val d_multq : Term.t -> (Mpa.Q.t * Term.t) option


val sigma : Sym.arith -> Term.t list -> Term.t
(** Given an arithmetic operation [Num(q)], [Multq(q)], [Add], [Mult], or [Div]
  as declared in [term.mli], [sigma op l] builds a normalized application by
  applying the corresponding constructor to the list [l] of terms. Hereby, it is
  assumed that [l] is empty for [Num(q)], the singleton list for [Multq(q)],
  a list of length two for [Div], and lists of length greater or equal to two
  for both [Add] and [Mult]. *)


val map: (Term.t -> Term.t) -> Term.t -> Term.t
(** Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in polynomial normal form.  Thus, [norm] can be thought
  of as the the composition of applying substitution [s] to [a] followed by 
  sigmatizing each arithmetic subterm by the function [sigma] above. *)



val solve : Fact.equal -> Fact.equal option
(**  [solve x (a,b)] solves the equation [a = b] over the rationals.
  If this equation is inconsistent, then it raises [Exc.Inconsistent].
  In case the equation holds trivially it returns the empty solution [None]. 
  Otherwise, it returns the solution [x = t] as [Some(x,t)],
  where [x] is one of the power products of either [a] or [b],
  and [x] does not occur in [t]. In addition, [t] is in normalized form.
  There are usually several  choices for the power product [x] to solve for, 
  and [qsolve] chooses to solve for the largest power product according to the 
  term ordering [<<<]. *)


val tau : (Term.t -> Cnstrnt.t) -> Sym.arith -> Term.t list -> Cnstrnt.t
(** Abstract interpretation in the domain of constraints. Given 
 a context [f], which associates uninterpreted subterms of [a]
 with constraints, [cnstrnt f a] recurses over the interpreted
 structure of [a] and accumulates constraints by calling [f] at
 uninterpreted positions and abstractly interpreting the 
 interpreted arithmetic operators in the domain of constraints.
 May raise the exception [Not_found], when some uninterpreted 
 subterm of [a] is not in the domain of [f]. *) 


val is_int : (Term.t -> Cnstrnt.t) -> Term.t -> bool
(** Test if arithmetic term is integer. *)

val cnstrnt : (Term.t -> Cnstrnt.t) -> Term.t -> Cnstrnt.t
(** Compute a subconstraint of {!Cnstrnt.mk_real} for a list of monomials. *)


val destructure : Term.t -> Mpa.Q.t * Term.t * Term.t
(** Destructure a monomial into the largest monomial [q * x] and the rest [ml],
 represented as the triple [(q, x, ml)]. If the argument term is not of such
 a form [Not_found] is raised. *)

