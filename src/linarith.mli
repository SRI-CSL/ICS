
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

(*i*)
open Mpa
open Term
(*i*)

(*s Module [Arith]: Constructors, recognizers, and accessors for
 arithmetic terms. Solvers for rational and integer arithmetic.
 Processing arithmetic equalities. *)

(*s [is_interp a] holds iff [a] is a inear arithmetic terms; that is,
  [a] is either a numeral (constructed with [num], see below), a linear 
  multiplication ([multq]), or an addition ([add], [add2]). *)

val is_interp: Term.t -> bool
val d_interp : Term.t -> (Sym.linarith * Term.t list) option

(* All non-arithmetic terms, that is, terms [a] for which [is_linarith a]
   fails are considered to be uninterpreted. [iter f a] is used to apply
   procedure [f] at uninterpreted positions of [a]. *)

val iter: (Term.t -> unit) -> Term.t -> unit  

(*s Some normalization functions. *)

val poly_of : Term.t -> Mpa.Q.t * Term.t list
val of_poly : Mpa.Q.t -> Term.t list -> Term.t

val mono_of : Term.t -> Mpa.Q.t * Term.t
val of_mono : Mpa.Q.t -> Term.t -> Term.t


(*s Constructors for building up arithmetic terms.  Arithmetic terms 
  are always in polynomial normal form. That is, they either
  represent a rational [num q], a power product [x] of 
  the form [x1 * ...* xn], for [n>= 1], where the [xi] are 
  uninterpreted, a monomial [q * x], where the coefficient [q] is a 
  non-zero rational,  and [x] a power product, or a (flattened) sum of monomials in 
  ascending order, with respect to the [Term.cmp]  comparison, from left-to-right.
  
  [num q] builds a rational numeral with value [q],
  [zero] just abbreviates [num '0'], and [one] abbreviates [num '1'].
  [add2 (a,b)] sums up two terms, [add l] sums the list [l] of terms,
  and [incr a] increments its argument term.
  [sub (a,b)] is subtraction, while [mult2] and [mult] construct
  terms for multiplication, and [div] divides two terms.  In addition,
  these constructors implement a number of simplifications such as
  [div x (mult x x) = div (num '1') x]. *)

val mk_num  : Mpa.Q.t -> Term.t
val mk_zero : Term.t
val mk_one  : Term.t


val mk_add  : Term.t -> Term.t -> Term.t
val mk_addl : Term.t list -> Term.t
val mk_incr : Term.t -> Term.t
val mk_sub  : Term.t -> Term.t -> Term.t
val mk_neg  : Term.t -> Term.t
val mk_multq: Mpa.Q.t -> Term.t -> Term.t

(*s Destructors. *)

val d_num : Term.t -> Q.t option
val d_multq : Term.t -> (Q.t * Term.t) option
val d_add : Term.t -> Term.t list option


(*s Given an arithmetic operation [Num(q)], [Multq(q)], [Add], [Mult], or [Div]
  as declared in [term.mli], [sigma op l] builds a normalized application by
  applying the corresponding constructor to the list [l] of terms. Hereby, it is
  assumed that [l] is empty for [Num(q)], the singleton list for [Multq(q)],
  a list of length two for [Div], and lists of length greater or equal to two
  for both [Add] and [Mult]. *)

val sigma : Sym.linarith -> Term.t list -> Term.t

(*s Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in polynomial normal form.  Thus, [norm] can be thought
  of as the the composition of applying substitution [s] to [a] followed by 
  sigmatizing each arithmetic subterm by the function [sigma] above. *)

val norm: (Term.t -> Term.t) -> Term.t -> Term.t

(*s  [solve x (a,b)] solves the equation [a = b] over the rationals.
  If this equation is inconsistent, then it raises [Exc.Inconsistent].
  In case the equation holds trivially it returns the empty solution [None]. 
  Otherwise, it returns the solution [x = t] as [Some(x,t)],
  where [x] is one of the power products of either [a] or [b],
  and [x] does not occur in [t]. In addition, [t] is in normalized form.
  There are usually several  choices for the power product [x] to solve for, 
  and [qsolve] chooses to solve for the largest power product according to the 
  term ordering [<<<]. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list

(*s [solve_for is like [solve] but has an extra parameter for choosing
 the variable to solve for. If this predicate never holds, then one solves
 for an unspecified variable. *)

val solve_for : (Term.t -> bool)           (* predicate for monomial to solve for. *)
                   -> Term.t * Term.t      (* Equality. *)
	             ->  (Term.t * Term.t) list




