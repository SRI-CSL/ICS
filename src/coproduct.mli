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

(** Theory of coproducts.

  @author Harald Ruess

  The {i signature} [COP] consists of the {b unary} function 
  symbols for 
  - left injection ({!Sym.Coproduct.inl}),
  - right injection ({!Sym.Coproduct.inr}),
  - left coinjection ({!Sym.Coproduct.outl}), and
  - right coinjection ({!Sym.Coproduct.outr}).
  
  The {i theory} [COP] is the set of equalities and disequalities
  which can be derived using the usual equality and disequality 
  rules and the universally quantified rules
  - [inl(outl(a)) = a]
  - [inr(outr(a)) = a]
  - [outr(inr(a)) = a]
  - [outl(inl(a)) = a]
  - [inl(a) <> inr(b)]

  A term is said to be {i canonical} in [COP], if it does not
  contain a term, which is of the form of any of the lhs above.
  For [a], [b] in canonical form: [a = b] holds in [P] iff [a] is 
  syntactically equal to [b] (that is, {!Term.eq}[a b] holds).

  This module provides
  - constructors [mk_inl], [mk_inr], [mk_outl], [mk_outr] for 
    building up canonical terms in [P].
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
   (** [is_diseq a b] holds if the prefixes of injections 
     of [a] and [b] are different. *)

val mk_inl : Term.t -> Term.t
  (** For canonical [a], [mk_inl a] constructs a canonical term 
    for representing  [inl(a)] *)

val mk_inr : Term.t -> Term.t
  (** For canonical [a], [mk_inr a] constructs a canonical term 
    for representing  [inr(a)] *)

val mk_outl : Term.t -> Term.t
  (** For canonical [a], [mk_outl a] constructs a canonical term 
    for representing  [outl(a)] *)

val mk_outr : Term.t -> Term.t
  (** For canonical [a], [mk_outr a] constructs a canonical term 
    for representing  [outr(a)] *)

val mk_inj : int -> Term.t -> Term.t
  (** Generalized injection of the form [inr(inr(....(inr(x))))]
    with [x] uninterpreted or of the form [inr(y)]. 
    - [mk_inj 0 a = mk_inl a]
    - [mk_inj 1 a = mk_inr a]
    - [mk_inj i a = mk_inr (mk_inj (i - 1) x)] if [i > 1]
    - Otherwise, the value of [mk_inj] is unspecified. *)

val mk_out : int -> Term.t -> Term.t
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

val apply : Term.Equal.t -> Term.t -> Term.t
  (** [apply (x, b) for uninterpreted occurrences of [x] in [a] 
    with [b], and normalizes. *)

val sigma : Sym.coproduct -> Term.t list -> Term.t
  (** [sigma op [a]] applies [mk_inl a] if [op] is equal to
    the [inl] symbol. Similarly for all the symbols.  Notice
    that the argument list is required to be unary. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list
  (** Given an equality [a = b], [solve(a, b)] 
    - raises the exception {!Exc.Inconsistent} iff the equality 
    [a = b] does not hold in [COP], or
    - returns a solved list of equalities [x1 = a1;...;xn = an] 
    with [xi] variables in [a] or [b], the [xi] are pairwise disjoint, 
    and no [xi] occurs in any of the [ai]. The [ai] are all in
    canonical form, and they might contain newly generated variables
    (see {!Term.Var.mk_fresh}). *)
