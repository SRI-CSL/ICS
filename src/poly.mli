
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

(*s Module [Poly]: Constructing and manipulating polynomials *)

(*i*)
open Mpa
open Hashcons
(*i*)

(*s The input signature of the functor [Poly.Make].
  [t] is the type of the variables of the polynomial, and
  [cmp] is a comparison function. *)


module type Var = sig
  type tnode
  type t = tnode hashed
  val cmp : t -> t -> int
end

	
module Make(R: Var) : sig 

  (*s A power product [pproduct] is a list of terms which are ordered from
    left-to-right according to teh comparison [cmp]; e.g. the
    power product $x^2*y^3$ is represented by the list [\list{x;x;y;y;y}]\@.
    Since power products are hash-consed, they can be compared for equality using
    the identity [===] from the module [Hashcons. A [monomial] is a pair consisting of
    a rational coefficient and a power product. A polynomial of type [t] can be viewed
    as a a map from power products to rational coefficients.
  *)

  type pproduct_node
  type pproduct = pproduct_node hashed

  val to_pproduct : R.t list -> pproduct
  val of_pproduct : pproduct -> R.t list
    
  type monomial

  val to_monomial : Q.t * pproduct -> monomial
  val of_monomial : monomial -> Q.t * pproduct	    

  type t
  
    (*s Represent a polynomial as a list of monomials. These monomials
      are in ascending order, according to [R.cmp], from left-to-right.
      However, [of_list] assumes that its argument list is appropriately
      ordered. *)
    
  val to_list : t -> monomial list
  val of_list : monomial list -> t

      (*s Pretty-printing a polynomial. *)

  val pp : (Format.formatter -> R.t -> unit) -> Format.formatter -> t -> unit    
  
      (*s Constructors and recognizers for the [zero] polynomial, the [one]
	polynomial, and the constant polynomials [num q]. [pproduct] builds
	a polynomial consisting of only one power product, and [monomial]
	is the constructor for a polynomial consisting of only one monomial. *)
  
  val zero : t
  val one : t
  val num : Q.t -> t
  val pproduct : R.t list -> t
  val monomial : Q.t * R.t list -> t

       (*s Testing for the [zero] and [one] polynomial. *)

  val is_zero : t -> bool
  val is_num : t -> bool

       (*s Computing the constant coefficient of the polynomial *)

  val num_of : t -> Q.t

      (*s [exists f p] checks if at least one monomial of the polynomial satisfies
	the predicate [f], whereas [for_all f p] checks if every monomial of
	the polynomial satisfies the predicate [f]. *)
 
  val exists : (monomial -> bool) -> t -> bool
   
  val for_all : (monomial -> bool) -> t -> bool
         
      (*s Arithmetic operations on polynomials. *)

  val neg : t -> t
  val add2 : t -> t -> t
  val add : t list -> t
  val sub : t -> t -> t
  val mult2 : t -> t -> t
  val mult : t list -> t

      (*s Compute a polynomial by applying a function [f] to all
	coefficients of the argument polynomial. *)
 
  val multq : Q.t -> t -> t
  val addq : Q.t -> t -> t
  val divq : Q.t -> t -> t

      (*s Leading coefficient *)

  val leading : t -> Q.t
       
    (*s Destructure a polynomial into the constant coefficient and the rest of the polynomial *)

  val destructure: t -> t * Q.t
      
    (*s Choose the smallest monomial, and return it together with the rest of the polynomial. *)

  val smallest : t -> monomial * t
      
    (*s Choose a monomial satisfying some predicate from a polynomial.
       If there is such a monomial [m] then return [Some(m)] and the
       other monomials. Otherwise return [None]\@. *)

  val choose : (monomial -> bool) -> t -> monomial option * t

    (*s The usual iterators on polynomials. But care has to be taken that
        the constraint on the ordering of the polynomials is not violated
        in calls to [fold] and [map]\@. *)

  val mapq : (Q.t -> Q.t) -> t -> t
   
    (* Partition a polynomial into a polynomial consisting of
       monomials which satisfy some predicate [p] and the polynomial
       formed from the remaining monomials. *)

  val partition : (monomial -> bool) -> t -> t * t
      
    
    (*s Greatest common divisor (gcd) and least common multiple
       of a polynomial's (rational) coefficients. *)

  val gcd : t -> Z.t
  val lcm : t -> Z.t

  (*s [qsolve x p] solves the polynomial equation [p = 0] over the rationals.
    If this equation is inconsistent, then it returns [Inconsistent], and in case
    the equation holds trivially it returns [Valid]. Otherwise, it returns the
    solution in the form [Solution(pp,q)], where [pp] is a power product and [q]
    a polynomial which does not contain [pp]\@. There are usually several choices
    for the [pp] to solve for. If the [x] parameter is of the form [Some(pp)], then
    [qsolve] chooses to solve for [pp] if possible.

    [zsolve p] solves the polynomial equation [p = 0] over the integers.  It
    returns a list containing the most general solution for each of
    the power products in [p]. These solutions contain fresh variables,
    which are returned as the first part of the result of [zsolve].
   *)

  type 'a solution =
    | Valid
    | Inconsistent
    | Solution of 'a   

  val qsolve : R.t option -> t -> (pproduct * t) solution
      
  val zsolve : (unit -> R.t) -> t -> (R.t list * ((pproduct * t) list)) solution

end
















