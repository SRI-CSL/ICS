
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 * 
 * Author: Harald Ruess
 i*)


(*s Module [Arith]: Constructors, recognizers, and accessors for
 arithmetic terms. Solvers for rational and integer arithmetic.
 Processing arithmetic equalities. *)


(*s [is_linarith a] holds iff [a] is a inear arithmetic terms; that is,
  [a] is either a numeral (constructed with [num], see below), a linear 
  multiplication ([multq]), or an addition ([add], [add2]).  In addition, 
  [is_arith a] is also true for division ([div]) and nonlinear multiplication 
  ([mult], [mult2]). *)

val is_arith: Term.t -> bool
val is_linarith : Term.t -> bool


(* All non-arithmetic terms, that is, terms [a] for which [is_linarith a]
   fails are considered to be uninterpreted. [iter f a] is used to apply
   procedure [f] at uninterpreted positions of [a]. *)

val iter: (Term.t -> unit) -> Term.t -> unit        


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

val num  : Mpa.Q.t -> Term.t
val zero : Term.t
val one  : Term.t

val add  : Term.t list -> Term.t
val add2 : Term.t * Term.t -> Term.t
val incr : Term.t -> Term.t
val sub  : Term.t * Term.t -> Term.t
val neg  : Term.t -> Term.t

val multq: Mpa.Q.t -> Term.t -> Term.t
val mult2: Term.t * Term.t -> Term.t
val mult : Term.t list -> Term.t

val div : Term.t * Term.t -> Term.t


(*s Given an arithmetic operation [Num(q)], [Multq(q)], [Add], [Mult], or [Div]
  as declared in [term.mli], [sigma op l] builds a normalized application by
  applying the corresponding constructor to the list [l] of terms. Hereby, it is
  assumed that [l] is empty for [Num(q)], the singleton list for [Multq(q)],
  a list of length two for [Div], and lists of length greater or equal to two
  for both [Add] and [Mult]. *)

val sigma : Term.arith -> Term.t list -> Term.t


(*s Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in polynomial normal form.  Thus, [norm] can be thought
  of as the the composition of applying substitution [s] to [a] followed by 
  sigmatizing each arithmetic subterm by the function [sigma] above. *)

val norm: Subst.t -> Term.t -> Term.t


(*s The less-then constructor [lt (a,b)] builds a constraint corresponding to
  the fact that the difference [sub(a,b)] is negative. Similarly, 
  the less-or-equal constructor [le] associates a non-positive constraint to [sub(a,b)].
  In addition, [sub(a,b)] is normalized so that the coefficient of its least power product, 
  with respect to [<<<], is one. *)

val lt : Term.t * Term.t -> Term.t
val le : Term.t * Term.t -> Term.t


(*s  [qsolve x (a,b)] solves the equation [a = b] over the rationals.
  If this equation is inconsistent, then it raises [Exc.Inconsistent].
  In case the equation holds trivially it returns the empty solution [None]. 
  Otherwise, it returns the solution [x = t] as [Some(x,t)],
  where [x] is one of the power products of either [a] or [b],
  and [x] does not occur in [t]. In addition, [t] is in normalized form.
  There are usually several  choices for the power product [x] to solve for, 
  and [qsolve] chooses to solve for the largest power product according to the 
  term ordering [<<<]. *)

val qsolve : Eqn.t -> Eqn.t option

(*s [zsolve (a,b)] solves the polynomial equation [a = b] over the integers.  
  If this equation is unsatisfiable over the integers, an exception [Exc.Inconsistent]
  is raised. Otherwise it return a list containing the most general solution 
  for each of the power products in either [a] or [b]. These solutions may
  contain fresh integer variables. *)

val zsolve : Eqn.t -> Eqn.t list


(*s [solve ctxt (a,b)] combines solving over the rationals and over the reals.
  The context function [ctxt a] returns a constraint for [a], which is used
  to determine if the equation [a = b] is Diophantine; that is, all constraints
  of power products in [a],[b], as determined by [ctxt], are sub-constraints 
  of the integer constraint. Now, [zsolve] is applied if the argument equation 
  is Diophantine, and [qsolve] otherwise. In addition. solving over the rationals
  includes further simplifications. For example, a solved form ['x * x * y = 2']
  is further simplified to ['y = 2 / (x * x)'] if [x] is known to be non-negative
  according to the constraint context. *)

val solve : (Term.t -> Cnstrnt.t) -> Eqn.t -> Eqn.t list


(*s The type [t] comprises a set of arithmetic equalities of the form [x = a],
  where [x] is a power product that is not a fresh variable, and [a] is 
  either an arithmetic term or a fresh integer variable (as introduced by
  the integer solve [zsolve].  [empty] represents the empty arithmetic
  context, an arithmetic equality is added to an arithmetic context
  by [extend] or [process], and uninterpreted equalities are propagated 
  by [propagate].  Since [extend], [process], and [propagate] destructively
  update the argument constraints, [copy] must be used before calling these
  functions in order to save a certain state.  [Copying] is inexpensive,
  since only a shallow copy is created. *)

type t

val empty: unit -> t
val copy: t -> t

(*s [subst_of s] returns a substitution with bindings of the form [x |-> a],
 where [x] is a power product (but not a fresh variable), and [a] is an
 arithmetic term or a fresh variable.  Substitution represents an
 arithmetic context by interpreting bindings as equalities [x = a]. *)

val subst_of : t -> Subst.t

(* [use_of s a] returns the set of right-hand side terms of s, in which
 [a] occurs as a subterm. *)

val use_of : t -> Term.ts Term.Map.t

(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
 [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
 is no binding with domain [a]. [inv s b] returns [a] if there is a binding
 [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
 contain [a] as a subterm. *)

val apply: t -> Term.t -> Term.t
val find: t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.ts


(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. It assumes
  that [a] is not yet in the domain of [s]. Also, [a],[b] must be valid lhs and rhs. *)

val extend : t -> Eqn.t -> unit


(*s [process ctxt s (a,b)] installs an equality [a = b], where at least one of [a],[b] is
 an arithmetic term, into the arithmetic context [s]. The argument [ctxt] is a function for 
 determining constraints for terms.  Abstractly, the manipulations on [s] can be described
 by [s o solve(norm s a, norm s b)].  The composition [o] operator includes new bindings
 [x |-> e] for each equality in the solved form [solve(norm s a, norm s b)], and all 
 rhs of [s] are normalized, using [norm] above] with respect to the this solved form. 
 In addition, if the rhs of a newly introduced binding reduces to an uninterpreted term
 (excluding fresh variables), then the corresponding binding is removed and returned 
 as a newly infered equality between uninterpreted terms. The second return value is
 a list of newly infered constraints for variables.  For example, if the binding
 [x |-> 2 + y] is added, where [y] is known to be greater then three, then the 
 constraint [('real(5.._)', x)] is included as a new constraint. *)

val process: (Term.t -> Cnstrnt.t) -> t -> Eqn.t -> Eqn.t list * (Cnstrnt.t * Term.t) list


(*s [propagate ctxt s (a,b)] works similar to [process] above, but it assumes that both [a] and [b]
 are uninterpreted (and not fresh either). *)

val propagate: (Term.t -> Cnstrnt.t) -> t -> Eqn.t list -> Eqn.t list * (Cnstrnt.t * Term.t) list

(*s [equivs s a] returns the set of terms equivalent with [a] according to [s]. *)

val ext: t -> Term.t -> Term.ts

(*s [groebner s] computes a Groebner basis for [s]. *)

val groebner : (Term.t -> Cnstrnt.t) -> t -> Eqn.t list * (Cnstrnt.t * Term.t) list


(*s [cnstrnt ctxt s a] computes the best constraint for [a] according to the arithmetic
 bindings in [s] and the context declarations [ctxt]. *)

val cnstrnt : (Term.t -> Cnstrnt.t) -> t -> Term.t -> Cnstrnt.t
