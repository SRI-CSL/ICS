
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

  A linear arithmetic term is built-up from 
  - rational constants,
  - linear multiplication of a rational with a variable, and 
  - n-ary addition. 

  Linear arithmetic terms are always {i canonized} as a sum-of-product
  [q0 + q1*x1+...+qn*xn] where the [qi] are rational constants and the
  [xi] are variables (or any other term not interpreted in this
  theory), which are ordered such that {!Term.cmp}[(xi, xj)] is
  greater than zero for [i < j]. This implies that any such variable
  occurs at most once. In addition, [qi], for [i > 0], is never zero.
  If [qi] is one, we just write [xi] instead of [qi * xi], and if [q0]
  is zero, it is simply omitted in the sum-of-product above.

  @author Harald Ruess
*)



(** {6 Constructors} *)

val mk_num  : Mpa.Q.t -> Term.t
  (** [mk_num q] creates a constant [mk_app (num q) []] *)
  
val mk_zero : unit -> Term.t
  (** [mk_zero] is [mk_num Mpa.Q.zero] *)

val mk_one  : unit -> Term.t
  (** [mk_one] is [mk_num Mpa.Q.one] *)
    
val mk_two  : unit -> Term.t
  (** [mk_one] is [mk_num Mpa.Q.one] *)

val mk_posinf : unit -> Term.t
val mk_neginf : unit -> Term.t
val mk_eps : unit -> Term.t
  
val mk_add  : Term.t -> Term.t -> Term.t
  (** [mk_add a b] constructs the normalized linear arithmetic term for the sum 
    of [a] and [b]. *)

val mk_addl : Term.t list -> Term.t
  (** [mk_addl] iterates {!Arith.mk_add} as follows:
    - [mk_addl []] is [mk_zero]
    - [mk_addl [a]] is [a]
    - [mk_addl [a0;...;an]] is [mk_add a0 (mk_addl [a1;...;an])] *)

val mk_incr : Term.t -> Term.t
  (** [mk_incr a] creates the normalized linear arithmetic term
    representing [a + 1]. *)

val mk_decr : Term.t -> Term.t
  (** [mk_decr a] creates the normalized linear arithmetic term
    representing [a - 1]. *)
  
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

(** {6 Accessors} *)

val poly_of : Term.t -> Mpa.Q.t * Term.t list
val mono_of : Term.t -> Mpa.Q.t * Term.t

val constant_of : Term.t -> Mpa.Q.t

val monomials_of : Term.t -> Term.t list


(** {6 Recognizers} *)

val is_pure : Term.t -> bool
  (** [is_pure a] holds iff [a] is a linear arithmetic term; that is,
    [a] is term equal to a numeral (constructed with {!Arith.mk_num}), a linear 
    multiplication ({!Arith.mk_multq}), or an addition ({!Arith.mk_add}). 
    All non-variable terms for which [is_interp] is [false] are considered
    to be {i uninterpreted} in the theory of linear arithmetic, and are
    treated as variables by the functions and predicates in this module. *)

val is_interp: Term.t -> bool
  (** [is_interp a] holds iff the toplevel function symbol in [a] is 
    a linear arithmetic function symbol. *)

val is_num : Term.t -> bool

val is_q : Mpa.Q.t -> Term.t -> bool
  (** [is_q q a] holds iff [a] is equal to the numeral [mk_num q]. *)

val is_zero : Term.t -> bool
  (** [is_zero a] holds iff [a] is equal to [mk_zero]. *)

val is_one : Term.t -> bool
  (** [is_one a] holds iff [a] is equal to [mk_one]. *)

val is_diophantine : Term.t -> bool
  (** [is_diophantine a] holds iff all variables in [a] are integer. *)

val is_nonneg : Term.t -> Three.t

val is_pos : Term.t -> Three.t

(** {6 Destructors} *)

val destruct : Term.t -> Mpa.Q.t * Term.t

val d_interp : Term.t -> Sym.arith * Term.t list

val d_num : Term.t -> Mpa.Q.t
  (** [d_num a] return [q] if [a] is a constant with
    function symbol {!Sym.Num}[(q)], and raises [Not_found] otherwise. *)

val d_add : Term.t -> Term.t list
  (** [d_add a] returns [Some(bl)] if [a] is a function application
    with symbol {!Sym.Add}. *)

val d_multq : Term.t -> Mpa.Q.t * Term.t
  (** [d_multq a] returns [Some(q, b)] if [a] is a function application
    of the symbol {!Sym.Multq}[(q)] to the unary list [[b]]. *)

val monomials : Term.t -> Term.t list
  (** [monomials a] yields a list of monomials [ml] such that [mk_addl ml]
    equal [a]. *)

val destruct : Term.t -> Mpa.Q.t * Term.t
  (** [destruct a] returns a pair [(q, b)] such that [a = q + b]
    and the constant monomial of [b] is [0]. *)
  
val coefficient_of : Term.t -> Term.t -> Mpa.Q.t
   
val lcm_of_denominators : Term.t -> Mpa.Z.t


(** {6 Iterators} *)

val iter : (Term.t -> unit) -> Term.t -> unit

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** Applying a term transformer [f] at uninterpreted positions.
    - [map f (mk_num q)] equals [mk_num q]
    - [map f (mk_multq q x)] equals [mk_multq q (map f x)]
    - [map f (mk_addl al)] equals [mk_addl (List.map f al)]
    - Otherwise, [map f x] equals [f x] *)

val apply: Term.Equal.t -> Term.t -> Term.t
  (** [apply (x, b) a occurrences of [x] in [a] with [b], and normalizes. *)


module Monomials : sig

  type t = Mpa.Q.t * Term.t

  val is_true : t -> bool
  val is_pos : t -> bool
  val is_neg : t -> bool
  val is_var : Term.t -> t -> bool

  val mapq : (Mpa.Q.t -> Mpa.Q.t) -> Term.t -> Term.t
 
  val fold: (t -> bool) -> (t -> 'a -> 'a) -> Term.t -> 'a -> 'a
    (** Folding over the non-constant monomials of an arithmetic term. *)
    
  val exists : (t -> bool) -> Term.t -> bool
    
  val for_all :  (t -> bool) -> Term.t -> bool
    
  val choose : (t -> bool) -> Term.t -> Mpa.Q.t * Term.t
   
  val partition : (t -> bool) -> Term.t -> Mpa.Q.t * Term.t * Term.t

  module Pos : sig
    val is_empty : Term.t -> bool
    val exists : (t -> bool) -> Term.t -> bool
    val for_all : (t -> bool) -> Term.t -> bool
    val fold: (t -> 'a -> 'a) -> Term.t -> 'a -> 'a
    val iter : (t -> unit) -> Term.t -> unit
    val mem : Term.t -> Term.t -> bool
    val choose : (t -> bool) -> Term.t -> t
    val least : Term.t -> t
    val coefficient_of : Term.t -> Term.t -> Mpa.Q.t
  end 

  module Neg : sig
    val is_empty : Term.t -> bool
    val exists : (t -> bool) -> Term.t -> bool
    val for_all : (t -> bool) -> Term.t -> bool
    val fold: (t -> 'a -> 'a) -> Term.t -> 'a -> 'a
    val iter : (t -> unit) -> Term.t -> unit
    val mem : Term.t -> Term.t -> bool
    val choose : (t -> bool) -> Term.t -> t
    val least : Term.t -> t
    val coefficient_of : Term.t -> Term.t -> Mpa.Q.t
  end
    
end 




(** {6 Canonization} *)

val sigma : Sym.arith -> Term.t list -> Term.t
  (** [sigma op al] applies the linear arithmetic function symbol
    to the list of arguments [al] such that the result is equal
    to [App(op, al)] in this theory and normalized if all terms in 
    [al] are normalized. If [op] is of the form [multq _], then [al] 
    is required to be unary, and for [op] of the form [num _], the 
    argument list must be [[]]. Otherwise, the outcome is unspecified. *)


(** {6 Solver} *)

val qsolve : Term.t * Term.t -> (Term.t * Term.t) option
  (**  [solve e] solves the equation [e] of the form [a = b] over the 
    rationals. If [e] is inconsistent, then {!Exc.Inconsistent} is
    raised. In case the equation holds trivially it returns the 
    empty solution [None].  Otherwise, it returns a solution [e']
    of the form [x = t]  as [Some(e')], where [x] is a variable
    already contained in [e], and [t] is a linear arithmetic term 
    not containing [x]. *)
  
val zsolve : Term.t * Term.t  -> (Term.t * Term.t) list
  (** Solution for a linear diophantine equation. The result is
    a list of equalities of the form [x = t], where [x] is a variable
    contained in [e], and [t] does not contain any variable in [e].
    [t] usually contains newly generated variables. {!Exc.Inconsistent}
    if raised if the given equation [e] is unsatisfiable in the integers. *)

val integer_solve : bool ref

val solve : Term.t * Term.t -> (Term.t * Term.t) list

val isolate : Term.t -> Term.Equal.t -> Term.Equal.t
  (** [isolate y (a, b)] isolates [y] in an equality [a = b];
    that is, if there is a [b] such that [y = b] iff [a = b], then
    [b] is returned. In case [y] does not occur in [a], [Not_found]
    is raised. *)

    

(** {6 Abstract Constraint Computation} *)


val dom : (Term.t -> Dom.t) -> Sym.arith -> Term.t list -> Dom.t

val dom_of : Term.t -> Dom.t

val is_int : Term.t -> bool

val is_infeasible : Term.t * Term.t -> bool
  (** [is_infeasible (a, b)] checks if [a = b] is infeasible. *)


