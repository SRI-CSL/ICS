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

(** Theory of coproducts

  @author Harald Ruess
*)

(** The {i signature} [COP] consists of the {b unary} function 
  symbols for 
  - left injection ([Sym.Coproduct.inl]),
  - right injection ([Sym.Coproduct.inr]),
  - left coinjection ([Sym.Coproduct.outl]), and
  - right coinjection ([Sym.Coproduct.outr]).
  
  The {i theory} [COP] is the set of equalities and disequalities
  which can be derived using the usual equality and disequality 
  rules and the universally quantified rules
  - [inl(outl(a)) = a]
  - [inr(outr(a)) = a]
  - [outr(inr(a)) = a]
  - [outl(inl(a)) = a]
  - [inl(a) <> inr(b)]
  - [inl(a) <> a]
  - [inr(a) <> a]
  
  A term is said to be {i canonical} in [COP], if it does not
  contain a term, which is of the form of any of the lhs above.
  For [a], [b] in canonical form: [a = b] holds in [P] iff [a] is 
  syntactically equal to [b] (that is, {!Term.eq}[a b] holds).
  
  This module provides
  - constructors [mk_inl], [mk_inr], [mk_outl], [mk_outr] for 
    building up canonical terms in [COP].
  - test for disequalities
  - canonizer for terms in [COP]
  - a solver [solve] for solving equalities in [COP].
  
  All terms with a toplevel symbol not in [COP] are treated as 
  variables. Such terms are said to {i occur uninterpreted}.
*)

val is_interp : Term.t -> bool
  (** [in_interp a] holds if the top-level function symbol is 
    in the signature of [COP]. *)
  
val is_pure : Term.t -> bool
  (** [is_pure a] holds if every function symbol in [a] is in [COP];
    that is, only variables occur at uninterpreted positions. *)
  
val is_diseq : Term.t -> Term.t -> bool
  (** [is_diseq a b] iff [a], [b] are disequal in the theory of [COP]. *)

type direction = Left | Right
  
val mk_in : direction -> Term.t -> Term.t
  (** For canonical [a], [mk_in x a] constructs a canonical term 
    for representing  [inx(a)] *)
 
val mk_out : direction -> Term.t -> Term.t
  (** For canonical [a], [mk_out x a] constructs a canonical term 
    for representing  [outx(a)] *)
 
val mk_iterated_inj : int -> Term.t -> Term.t
  (** Generalized injection of the form [inr(inr(....(inr(x))))]
    with [x] uninterpreted or of the form [inr(y)]. 
    - [mk_inj 0 a = mk_inl a]
    - [mk_inj 1 a = mk_inr a]
    - [mk_inj i a = mk_inr (mk_inj (i - 1) x)] if [i > 1]
    - Otherwise, the value of [mk_inj] is unspecified. *)
  
val mk_iterated_out : int -> Term.t -> Term.t
  (** Generalized coinjection:
    - [mk_out 0 a = mk_outl a]
    - [mk_out 1 a = mk_outr a]
    - [mk_out i a = mk_outr (mk_out (i - 1) x)] if [i > 1]
    - Otherwise, the value of [mk_out] is unspecified. *)
  
val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] homomorphically applies [f] at uninterpreted positions 
    of [a] and returns the corresponding canonical term. More precisely,
    [map f (constr a)] equals [constr (map f a)] for [constr] one of the
    constructors [mk_inl], [mk_inr], [mk_outl], [mk_outr]. 
    Otherwise, [map f x] equals [f x] *)

val apply : Term.t * Term.t -> Term.t -> Term.t
  (** [apply (x, b)] for uninterpreted occurrences of [x] in [a] 
    with [b], and normalizes. *)

val sigma : Term.interp
  (** [sigma op [a]] applies [mk_inl a] if [op] is equal to
    the [inl] symbol. Similarly for all the symbols.  Notice
    that the argument list is required to be unary. *)

val solve : Term.t ->  Term.t -> Term.Subst.t
  (** Given an equality [a = b], [solve(a, b)] 
    - raises the exception {!Exc.Inconsistent} iff the equality 
    [a = b] does not hold in [COP], or
    - returns a solved list of equalities [x1 = a1;...;xn = an] 
    with [xi] variables in [a] or [b], the [xi] are pairwise disjoint, 
    and no [xi] occurs in any of the [ai]. The [ai] are all in
    canonical form, and they might contain newly generated variables
    (see {!Term.Var.mk_fresh}). *)


module Component: Shostak.COMPONENT
  (** Inference system for the theory {!Th.cop} of coproducts
    as defined in module {!Coproduct}.

    This inference system maintains a set of directed 
    equalities [x = a] with [x] a variable, [a] a {!Th.cop}-pure term, 
    and none of the right-hand side variables occurs in any of the left-hand sides. 

    This inference system is obtained as an instantiation of the generic
    Shostak inference system [Shostak.Infsys] with a specification of the 
    coproduct theory by means of the 
    - coproduct canonizer {!Coproduct.map}
    - coproduct solver {!Coproduct.solve} *)
  
