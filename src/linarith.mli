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

(** Linear arithmetic.

  @author Harald Ruess

  A {i linear arithmetic term} is an ordered sum-of-monomials built-up from 
  - rational constants, 
  - linear multiplication by a rational constant, and 
  - addition. 
  
  This module defines the theory of {i linear arithmetic} including
  its function symbols, provides constructors for building linear
  arithmetic terms, and solvers for equations over linear arithmetic
  terms in the reals (rationals) and integers.
*)

val theory : Theory.t
  (** Theory of linear arithmetic. This theory is being registered in 
    module {!Theory}. *)

val is_interp : Term.t -> bool
  (** A term [a] is {i interpreted} in the theory of linear arithmetic
    if its toplevel function symbol is in the signature of this theory.
    In general, all non-variable terms for which [Linarith.is_interp] is [false] are 
    considered to be {i uninterpreted} in this theory, and are
    treated as variables by the functions and predicates in this module. *)

val is_pure : Term.t -> bool
  (** A term [a] is {i pure} if it is built-up entirely from function
    symbols in the theory of linear arithmetic and variables. *)

val is_monomial : Term.t -> bool
  (** A {i monomial} is either of the form [q*x] or simply [x] 
    with [x] not interpreted. *)

val is_canonical : Term.t -> bool
  (** Linear arithmetic terms are always in {i canonical} ordered sum-of-product
    form [q0 + q1*x1+...+qn*xn] where the [qi*x1] monomials,  which are ordered 
    such that {!Term.cmp}[(xi, xj)] is greater than zero for [i < j]. 
    This implies that any such variable occurs at most once. In addition, [qi], 
    for [i > 0], is never zero. If [qi] is one, we just write [xi] instead 
    of [qi * xi], and if [q0] is zero, it is simply omitted in the sum-of-product 
    above. [is_canonical a] is true if [a] is such an ordered sum-of-product.
    In particular, if [is_canonical a] and [is_canonical b] holds then [a = b]
    in the theory of linear arithmetic iff [Term.eq a b]. *)

(** {i Function symbols} of the the theory of linear arithmetic.
  - [Num(q)] for representing rational constants
  - [Multq(q)] for representing linar multiplication by a rational constant [q]
  - [Add] for representing addition.
  These function symbols are registered in module {!Funsym} for the theory {!Linarith.theory}. *)
module Sig : sig
  type t = Num of Mpa.Q.t | Multq of Mpa.Q.t | Add
end

val op : Term.t -> Sig.t
  (** [op a] returns a linear arithmetic function symbol [f],
    if [is_interp a].  Otherwise, [Not_found] is raised. *)

val args : Term.t -> Term.t list
  (** [args a] returns argument list of term application [a]
    if [is_interp a].  Otherwise, [Not_found] is raised. *)

val mk_num  : Mpa.Q.t -> Term.t
  (** [mk_num q] creates a represention of the rational constant [q]. *)
  
val mk_zero : unit -> Term.t
  (** [mk_zero] is [mk_num Mpa.Q.zero] *)

val mk_one  : unit -> Term.t
  (** [mk_one] is [mk_num Mpa.Q.one] *)
    
val mk_two  : unit -> Term.t
  (** [mk_one] is [mk_num Mpa.Q.one] *)
  
val mk_add  : Term.t -> Term.t -> Term.t
  (** [mk_add a b] constructs a canonical linear arithmetic 
    term for the sum of [a] and [b]. *)

val mk_addl : Term.t list -> Term.t
  (** [mk_addl] iterates {!Linarith.mk_add} as follows:
    - [mk_addl []] is [mk_zero]
    - [mk_addl [a]] is [a]
    - [mk_addl [a0;...;an]] is [mk_add a0 (mk_addl [a1;...;an])] *)

val mk_incr : Term.t -> Term.t
  (** [mk_incr a] creates the canonical linear arithmetic 
    term representing [a + 1]. *)

val mk_decr : Term.t -> Term.t
  (** [mk_decr a] creates the canonical linear arithmetic term representing [a - 1]. *)
  
val mk_sub  : Term.t -> Term.t -> Term.t
  (** [mk_sub a b] creates the canonical linear arithmetic term representing [a - b]. *)

val mk_neg  : Term.t -> Term.t
  (** [mk_neg a] creates the canonical linear arithmetic term representing [-a]. *)

val mk_addq : Mpa.Q.t -> Term.t -> Term.t
  (** [mk_addq q a] creates the canonical linear arithmetic term representing [q + a]. *)
  
val mk_multq: Mpa.Q.t -> Term.t -> Term.t
  (** [mk_multq q a] creates the canonical linear arithmetic term representing [q * a]. *)

val integerize : Term.t -> Term.t * Mpa.Q.t
  (** For a term [a] with [Linarith.is_canonical a],
    [Linarith.integerize a] returns a pair [(b, q)]
    such that [b] equals [q*a] and [b] has only integral coefficients. *)

val is_num : Term.t -> bool
  (** [is_num a] holds iff [a] represents a constant numeral. *)

val is_q : Mpa.Q.t -> Term.t -> bool
  (** [is_q q a] holds iff [a] is equal to the numeral [mk_num q]. *)

val is_zero : Term.t -> bool
  (** [is_zero a] holds iff [a] is equal to [mk_zero]. *)

val is_one : Term.t -> bool
  (** [is_one a] holds iff [a] is equal to [mk_one]. *)

val constant_of : Term.t -> Mpa.Q.t
  (** If [a] is of the form [q + a'], then [constant_of a] returns [q]. *)

val nonconstant_of : Term.t -> Term.t
  (** If [a] is of the form [q + a'], then [nonconstant_of a] returns [a'] *)

val monomials_of : Term.t -> Term.t list
  (** If [a] is of the form [q + a1 + ... + an], then [nonconstant_of a] returns 
    the list of monomials [[a1; ...; an]] *)

val is_monomial : Term.t -> bool
  (** A monomial is either of ther form [q * a] or [a] with [a] uninterpreted. *)

val coeff_of : Term.t -> Mpa.Q.t
  (** If [is_monomial m] holds, then [coeff_of m] gets the coefficient of [m]. 
    Otherwise, [coeff_of m] is undefined. *)

val var_of : Term.t -> Term.t
  (** If [is_monomial m] holds, then [var_of m] gets the variable (or
    uninterpreted term) of [m]. Otherwise, [var_of m] is undefined. *)

val d_num : Term.t -> Mpa.Q.t
  (** [d_num a] return [q] if [a] is a constant with
    function symbol [Sym.Num(q)], and raises [Not_found] otherwise. *)

val d_add : Term.t -> Term.t list
  (** [d_add a] returns [Some(bl)] if [a] is a function application
    with symbol [Sym.Add]. *)
  
val coefficient_of : Term.t -> Term.t -> Mpa.Q.t
  (** [coefficient_of x a] returns [q] if there is a monomial of the form
    [q*x] in [a]. *)
 
val iter : (Term.t -> unit) -> Term.t -> unit
  (** [iter f a] applies [f] at uninterpreted positions of [a]. *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
 (** [fold f a e] applies [f] at uninterpreted positions of [a]
   and accumulates results starting with [e]. *)

val for_all :  (Term.t -> bool) -> Term.t -> bool
  (** [for_all f] holds iff [f x] holds for all top-level
    uninterpreted [x] in [a]. *)

val mapq : (Mpa.Q.t -> Mpa.Q.t) -> Term.t -> Term.t
  (** Given a term [a] with [is_canonical a] of the 
    form [q0 + q1*x1 + ... + qn*xn], then [mapq f a] yields [b] with
    - [is_canonical b] and
    - [b] is equal to [q0 + f(q1)*x1 + ... + f(qn)*xn] in linear arithmetic. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** Given a term [a] with [is_canonical a] of the 
    form [q0 + q1*x1 + ... + qn*xn], then [map f a] yields [b] with
    - [is_canonical b] and
    - [b] is equal to [q0 + q1*f(x1) + ... + qn*f(xn)] in linear arithmetic. *)

val apply : Term.t -> Term.t -> Term.t -> Term.t

val eval : (Term.t -> Mpa.Q.t) -> Term.t -> Mpa.Q.t
  (** [eval f a] applies [f] at all interpreted positions and computes
    the interpretation of [a]. *)

val sigma : Term.interp
  (** [sigma f al] builds linear arithmetic term [b] such that
    - [b] is equal to the term application [f(al)] in the theory of linear arithmetic,
    - [b] is_canonical,
    - and [f(al)] does not contain any variables or uninterpreted terms 
    not already in [al]. *) 

val is_diseq : Term.t -> Term.t -> bool

val qsolve : Term.t -> Term.t * Term.t
  (**  [qsolve a] solves the equation [a] of the form [a = 0] over the rationals. 
    - If [e] is inconsistent, then {!Exc.Inconsistent} is raised. 
    - In case the equation holds trivially it returns the  empty solution by raising {!Exc.Valid}.  
    - Otherwise, it returns a solution [(x, b)] with 
           - [x] does not occur in [b], 
           - [a = 0] iff [x = b], and
           - all variables and uninterpreted terms in [b] are already in [a]. *)

val qsolve_for : Term.t -> Term.t -> Term.t
  (** [qsolve x a] is similar to {!Linarith.qsolve} but result
    is solved for [x] if possible. It is assumed that [x] 
    in [Term.vars_of a]. *)

val qsolve_solved_for : Term.t -> Term.t * Term.t -> Term.t
  (** Given a variable [x] and a solved equality [y = a] with [y] a variable 
    and [y] not in [Term.vars_of a], [qsolve_solved_for x (y, a)] returns [b] such that 
    - [y = b] iff [x = a] in linear arithmetic,
    - [is_canonical b] holds, and
    - [y] does not occur in [b]. *)
  
val zsolve : Term.t  -> Term.Set.t * Term.Subst.t
  (** Solution for a linear diophantine equation [e = 0]. The result is
    {!Exc.Inconsistent} if [e <> 0] if valid in the integers or a 
    (integral) model-preserving {i solved form} consisting of 
    - a list of equalities of the form [x = t], where [x] is a variable contained in [e], 
    and [t] does not contain any variable in [e], and
    - a set of newly generated variables which may occur on right-hand sides [t]
    of the solved form above. *)

val cnstrnt : (Term.t -> Cnstrnt.t) -> Term.t -> Cnstrnt.t
  (** [cnstrnt of_term a] returns an abstract constraint interpretation [c]
    of the term [a] such that [a in c].  The weakest constraint returns in {!Cnstrnt.Real}. 
    [of_term] is called to obtain possible constraints on the variable part of monomials. 
    For example, if [of_term x] and [of_term y] both yield [Cnstrnt.Int], 
    then [cnstrnt of_term '-1 + 3x + 5y'] yields [Cnstrnt.Int]. *)
 
(** Constructors for linear arithmetic inequalities. *)
module Atom : sig

  val mk_le : Term.t -> Term.t -> Atom.t 
  val mk_ge : Term.t -> Term.t -> Atom.t

  val mk_lt : Term.t -> Term.t -> Atom.t
  val mk_gt : Term.t -> Term.t -> Atom.t

  val mk_modeq : Term.t -> Term.t -> Mpa.Z.t -> Atom.t list

end

(** {6 Monomials} *)


module Monomials : sig

  type sign = Pos | Neg | All

  val is_empty : sign -> Term.t -> bool

  val is_mem : sign -> Term.t -> Term.t -> bool

  val iter : sign -> (Term.t -> unit) -> Term.t -> unit

  val choose : Term.t -> Term.t

  val exists : sign -> (Term.t -> bool) -> Term.t -> bool

  val for_all : sign -> (Term.t -> bool) -> Term.t -> bool

  val choose_min : (Term.t -> Term.t -> int) -> Term.t -> Term.t

end
