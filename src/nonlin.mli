(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Nonlinear arithmetic

  @author Harald Ruess

  A {i nonlinear arithmetic term} is of the form [q{0} + q{1}*m{1} + ... + q{k}*m{k}] with
  - [q{i}] rationals of type {!Mpa.Q.t), 
  - [m{i}] {i monomials} of the form [inv(x{1}*...*x{n}) * (y{1}*...*y{n'})] and [x{i}], [x{j}]
  are non-arithmetic terms.
  
  This module defines 
  - recognizers and constructors for {i power products},
  - recognizers and constructors for {i monomials}, and
  - constructors for nonlinear arithmetic terms.
*)

val theory : Theory.t

type signature = Mult | Inv

module Op : (Funsym.OP with type op = signature)

val is_interp : Term.t -> bool
val is_mult : Term.t -> bool
val is_inv : Term.t -> bool
val op : Term.t -> signature
val lhs : Term.t -> Term.t
val rhs : Term.t -> Term.t
val denominator : Term.t -> Term.t


(** A {i power product} is of the form
  - [1] or
  - [x{1}*(x{2}*(...*x{n})...)] with [xi] variables or other nonarithmetic terms. *)
module Pprod : sig

  val is_interp : Term.t -> bool
    (** A term [a] is {i interpreted} in the theory of nonlinear arithmetic
      if its toplevel function symbol is a nonlinear multiplication or if 
      {!Linarith.is_interp a} holds. *)
    
  val is_pure : Term.t -> bool
    (** A term [a] is a {i pure nonlinear arithmetic term} if it is built-up entirely 
      from function symbols in the theory of linear arithmetic {!Linarith.theory},
      the theory of nonlinear multiplication {!Nl.nl}, and variables. *)
    
  val is_canonical : Term.t -> bool
    (** [is_canonical a] is true iff [a] is an ordered sum of monomials. *)

  val mk_mult : Term.t -> Term.t -> Term.t

  val mk_one : Term.t
  val is_one : Term.t -> bool

  val lhs : Term.t -> Term.t
  val rhs : Term.t -> Term.t

  val mk_mult : Term.t -> Term.t -> Term.t

  val gcd : Term.t -> Term.t -> Term.t
 
  val lcm : Term.t -> Term.t -> Term.t

  val mk_div : Term.t -> Term.t -> Term.t
end 

(** A {i monomial} is of the form 
  - 1 or
  - [x1*...*xn] or
  - [inv(y1*...*ym)*(x1*...*xn)]
  with [xi], [yj] variables or other nonarithmetic terms. *)
module Monomial : sig

  val is_interp : Term.t -> bool
  val is_pure : Term.t -> bool
  val is_pprod : Term. t-> bool
  val is_one : Term.t -> bool

  val make : Term.t -> Term.t -> Term.t
 
  val mk_inv : Term.t -> Term.t
  val mk_mult : Term.t -> Term.t -> Term.t

  val lhs : Term.t -> Term.t
  val rhs : Term.t -> Term.t

  val destruct : Term.t -> Term.t * Term.t
 
  val crossmultiply : Term.t * Term.t -> Term.t * Term.t

  val map : (Term.t -> Term.t) -> Term.t -> Term.t

  val sigma : Term.interp

end
	
val is_interp : Term.t -> bool	  
  (** A {i nonlinear} term is built up from function symbols in [la] and [nl]. *) 

val is_pure : Term.t -> bool

val is_canonical : Term.t -> bool

val mk_mult : Term.t -> Term.t -> Term.t
  (** Given two canonical terms [a] and [b], [mk_mult a b] returns a canonical
    term [c] such that [c] equals [a * b] in nonlinear arithmetic. *)
    
val mk_expt : Term.t -> int -> Term.t
  (** [mk_expt a n] iterates the [mk_mult] constructor such that
    [mk_expt a n] equals [a * ... * a] ([n] times). *)

val mk_multl : Term.t list -> Term.t

val mk_expt : Term.t -> int -> Term.t

val mk_inv : Term.t -> Term.t

val mk_div : Term.t -> Term.t -> Term.t
 
val map : (Term.t -> Term.t) -> Term.t -> Term.t
  (** Mapping a term transformer [f] over [a]. *)

val monomial_of : Term.t -> Term.t

val coeff_of : Term.t -> Mpa.Q.t

val is_nonneg : Term.t -> Three.t
