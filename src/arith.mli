
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
 i*)


(*s Module [Arith]: Constructors, recognizers, and accessors for
 arithmetic terms. Solvers for rational and integer arithmetic. *)

(*s Constructors for building up arithmetic terms. Each
    constructed term is in polynomial form; i.e. it either
    represents a constant [q], a power product [x] of the form [$x_1$ * ...* $x_n$],
    where the $x_i$ are uninterpreted, a monomial [q * x], where
    the coefficient [q] is a rational constant and [x] a power product,
    or a (flattened) sum of monomials which are in ascending order,
    with respect to the [Term.cmp]  comparison, from left-to-rigth.
  
    [num q] builds a constant arithmetic term of value [q],
    while [zero] abbreviates [num 0], and [one] abbreviates [num 1].
    [add2 (a,b)] sums up two terms, [add l] sums the list [l] of terms,
    and [incr a] increments its argument term.
    [sub (a,b)] is subtraction, while [mult2] and [mult] construct
    terms for multiplication.
  *)

val num  : Mpa.Q.t -> Term.t
val zero : Term.t
val one  : Term.t
val add  : Term.t list -> Term.t
val add2 : Term.t * Term.t -> Term.t
val incr : Term.t -> Term.t
val sub  : Term.t * Term.t -> Term.t
val neg  : Term.t -> Term.t
val mult : Term.t list -> Term.t
val mult2: Term.t * Term.t -> Term.t
val div2 : Term.t * Term.t -> Term.t

    (*s [divq q a] divides each coefficient of [a] by [q]; similarly
      [multq] multiplies the coefficients. *)

val divq : Mpa.Q.t -> Term.t  -> Term.t
val multq : Mpa.Q.t -> Term.t -> Term.t

    (*s Test if a term is an arithmetic expression. *)

val is_arith: Term.t -> bool

    (*s Test for arithmetic constant. *)

val is_num : Term.t -> bool
val num_of : Term.t -> Mpa.Q.t

    (*s [is_diophantine is_int a] tests if all power products in [a] satisfy
      the integer test [is_int]\@. *)
    
val is_diophantine: (Term.t -> bool) -> Term.t -> bool
    
    (*s Solving of an equality [a = b], represented by the pair [a,b)]
      in the rationals and the integers.

      [qsolve x (a,b)] solves the equation [a = b] over the rationals.
      If this equation is inconsistent, then it throws the [Exc.Inconsistent]
      exception.  In case the equation holds trivially it returns the empty solution
      list. Otherwise, it returns the solution as a singleton list [(x,t)],
      where [x] is one of the power products of either [a] or [b],
      and [x] does not occur in [t]\@.  There are usually several choices
      for the power product [x] to solve for. In case, the [x] parameter is [None],
      then [qsolve] solves for the smallest variable/nonarithmetic term in
      [a] or [b]; otherwise, if [x] is of the form [Some(x)], then [qsolve] chooses
      to solve for [x] if possible. 

      [zsolve (a,b)] solves the polynomial equation [a = b] over the integers.  It
      returns a list containing the most general solution for each of
      the power products in either [a] or [b]. These solutions contain
      fresh variables, which are returned as the first part of the result
      of [zsolve].
    *)

val qsolve : Term.t option -> Term.eqn -> Term.eqn list
    
val zsolve : Term.eqn -> (Term.t list * Term.eqn list)

	  
    (*s Test if some term is trivially an integer. *)

val is_integer: Term.t -> bool

    (*s Computes the gcd of two ordered power products. *)

val gcd : Term.t list -> Term.t list -> Term.t list

    (*s Destructure an arithmetic polynomial in constant and nonconstant part. *)

val d_poly : Term.t -> Term.t * Mpa.Q.t

    (*s Normalized inequalities. *)

val lt : Term.t * Term.t -> Term.t
val le : Term.t * Term.t -> Term.t

    (*s Constructor for constraint applications *)

val int : Term.t -> Term.t
val real : Term.t -> Term.t











