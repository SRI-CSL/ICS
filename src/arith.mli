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
 *)

(** Equality theory of linear arithmetic.

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

  @author Harald Ruess
*)



(** {6 Function symbols} *)

val num : Mpa.Q.t -> Sym.t
val multq : Mpa.Q.t -> Sym.t
val add : Sym.t


(** {6 Constructors} *)

val mk_num  : Mpa.Q.t -> Term.t
  (** [mk_num q] creates a constant [mk_app (num q) []] *)
  
val mk_zero : Term.t
  (** [mk_zero] is [mk_num Mpa.Q.zero] *)

val mk_one  : Term.t
  (** [mk_one] is [mk_num Mpa.Q.one] *)
    
val mk_two  : Term.t
  
val mk_add  : Term.t -> Term.t -> Term.t
  (** [mk_add a b] constructs the normalized linear arithmetic 
    term for the sum of [a] and [b]. *)

val mk_addl : Term.t list -> Term.t
  (** [mk_addl] iterates {!Arith.mk_add} as follows:
    - [mk_addl []] is [mk_zero]
    - [mk_addl [a]] is [a]
    - [mk_addl [a0;...;an]] is [mk_add a0 (mk_addl [a1;...;an])] *)

val mk_incr : Term.t -> Term.t
  (** [mk_incr a] creates the normalized linear arithmetic term
    representing [a + 1]. *)
  
val mk_sub  : Term.t -> Term.t -> Term.t
  (** [mk_sub a b] creates the normalized linear arithmetic term
    representing [a - b]. *)

val mk_neg  : Term.t -> Term.t
  (** [mk_neg a] creates the normalized linear arithmetic term
    representing [-a]. *)

val mk_addq : Mpa.Q.t -> Term.t -> Term.t
  (** [mk_addq q a] creates the normalized linear arithmetic term
    representing [q + a]. *)
  
val mk_multq: Mpa.Q.t -> Term.t -> Term.t
  (** [mk_multq q a] creates the normalized linear arithmetic term
    representing [q * a]. *)


(** {6 Recognizers} *)

val is_interp: Term.t -> bool
  (** [is_interp a] holds iff [a] is a linear arithmetic term; that is,
    [a] is term equal to a numeral (constructed with {!Arith.mk_num}), a linear 
    multiplication ({!Arith.mk_multq}), or an addition ({!Arith.mk_add}). 
    All non-variable terms for which [is_interp] is [false] are considered
    to be {i uninterpreted} in the theory of linear arithmetic, and are
    treated as variables by the functions and predicates in this module. *)

val is_num : Term.t -> bool
  (** [is_num a] holds iff [a] is equal to a numeral [mk_num _]. *)

val is_zero : Term.t -> bool
  (** [is_zero a] holds iff [a] is equal to [mk_zero]. *)

val is_one : Term.t -> bool
  (** [is_one a] holds iff [a] is equal to [mk_one]. *)

val is_q : Mpa.Q.t -> Term.t -> bool
  (** [is_q q a] holds iff [a] is equal to the numeral [mk_num q]. *)

val is_multq : Term.t -> bool
  (** [is_multq a] holds iff [a] is equal to some [mk_multq _ _]. *)


(** {6 Destructors} *)

val d_num : Term.t -> Mpa.Q.t option
  (** [d_num a] return [Some(q)] if [a] is a constant with
    function symbol {!Sym.Num}[(q)], and [None] otherwise. *)

val d_add : Term.t -> Term.t list option
  (** [d_add a] returns [Some(bl)] if [a] is a function application
    with symbol {!Sym.Add}. *)

val d_multq : Term.t -> (Mpa.Q.t * Term.t) option
  (** [d_multq a] returns [Some(q, b)] if [a] is a function application
    of the symbol {!Sym.Multq}[(q)] to the unary list [[b]]. *)

val poly_of : Term.t -> Mpa.Q.t * Term.t list
  (** [poly_of a] yields (q, ml) such that [q + mk_addl ml] equals [a]. *)

val of_poly : Mpa.Q.t -> Term.t list -> Term.t
  (** [of_poly q ml] is equal to [q + ml]. *)

val mono_of : Term.t -> Mpa.Q.t * Term.t
  (** The result (q, b) of [mono_of a] is such that [q * b = a]. *)

val of_mono : Mpa.Q.t -> Term.t -> Term.t
  (** [of_mono q a] yields [b] with [q * a = b]. *)

val monomials : Term.t -> Term.t list
  (** [monomials a] yiels a list of monomials [ml] such that [mk_addl ml]
    equal [a]. *)

val destructure : Term.t -> Mpa.Q.t * Term.t * Term.t
  (** Destructure a monomial into the a triple [(q, x, m)] such
    that [q * x] is the largest monomial ([x] is the largest variable in the
    argument term [a]) and [a = q * x + m].  If the argument term is not of such
    a form, [Not_found] is raised. *)

val multiple : Term.t * Term.t -> Mpa.Q.t
  (** If there exists a rational [q] such that [q * a = b], then 
    [multiple (a, b)] returns [q]; otherwise [Not_found] is raised. *)

val leading : Term.t -> Term.t
  (** [leading a] returns the largest variable (or largest term not interpreted
    in the theory of linear arithmetic) in [a] according to the term ordering
    {!Term.cmp}. *)


(** {6 Iterators} *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** [fold f a e] applies [f] at uninterpreted positions of [a] and 
    accumulates the results starting with [e]. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** Applying a term transformer [f] at uninterpreted positions.
    - [map f (mk_num q)] equals [mk_num q]
    - [map f (mk_multq q x)] equals [mk_multq q (map f x)]
    - [map f (mk_addl al)] equals [mk_addl (List.map f al)]
    - Otherwise, [map f x] equals [f x] *)


(** {6 Canonization} *)

val sigma : Sym.arith -> Term.t list -> Term.t
  (** [sigma op al] applies the linear arithmetic function symbol
    to the list of arguments [al] such that the result is equal
    to [App(op, al)] in this theory and normalized if all terms in 
    [al] are normalized. If [op] is of the form [multq _], then [al] 
    is required to be unary, and for [op] of the form [num _], the 
    argument list must be [[]]. Otherwise, the outcome is unspecified. *)


(** {6 Solver} *)

val qsolve : Fact.equal -> Fact.equal option
  (**  [solve e] solves the equation [e] of the form [a = b] over the 
    rationals. If [e] is inconsistent, then {!Exc.Inconsistent} is
    raised. In case the equation holds trivially it returns the 
    empty solution [None].  Otherwise, it returns a solution [e']
    of the form [x = t]  as [Some(e')], where [x] is a variable
    already contained in [e], and [t] is a linear arithmetic term 
    not containing [x]. *)
  
val zsolve : Fact.equal -> Fact.equal list
  (** Solution for a linear diophantine equation. The result is
    a list of equalities of the form [x = t], where [x] is a variable
    contained in [e], and [t] does not contain any variable in [e].
    [t] usually contains newly generated variables. {!Exc.Inconsistent}
    if raised if the given equation [e] is unsatisfiable in the integers. *)


(** {6 Constraints} *)

val tau : (Term.t -> Cnstrnt.t) -> Sym.arith -> Term.t list -> Cnstrnt.t
  (** Abstract interpretation in the domain {!Cnstrnt.t} of 
    arithmetic constraints. Given a context [f], which associates 
    uninterpreted subterms of [a]
    with constraints, [cnstrnt f a] recurses over the interpreted
    structure of [a] and accumulates constraints by calling [f] at
    uninterpreted positions and abstractly interpreting the 
    interpreted arithmetic operators in the domain of constraints.
    May raise the exception [Not_found], when some uninterpreted 
    subterm of [a] is not in the domain of [f]. *) 
  
val is_int : (Term.t -> Cnstrnt.t) -> Term.t -> bool
  (** Test if arithmetic term is integer. *)
  
val is_diophantine : (Term.t -> Cnstrnt.t) -> Term.t -> bool
  (** [is_diophantine c a] holds if all variables in the linear 
    arithmetic term [a] are interpreted over the integers, that is,
    if [c(x)] yields a subconstraint of {!Cnstrnt.int}. *)
  
val cnstrnt : (Term.t -> Cnstrnt.t) -> Term.t -> Cnstrnt.t
  (** Compute a subconstraint of {!Cnstrnt.mk_real} for a list of monomials. *)
