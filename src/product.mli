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

(** Theory of products.

  @author Harald Ruess

  The {i signature} [P] of pairs consists of
  - [cons], the product constructor of arity two.
  - [car] of arity one, projection on first component.
  - [cdr] of arity one, projection to second component.

  A term [a] is {i pure} in [P] if it is a variable or 
  built-up completely from [cons(b, c)], [car(b)], [cdr(b)] 
  with [b], [c] interpreted. Nonpure terms are treated
  as variables.

  The {i theory} [P] of pairs consists of the equalities which
  can be derived using the usual equality rules and the universally
  quantified equations
  - [car(cons(s, t)) = s]
  - [cdr(cons(s, t)) = t]
  - [cons(car(s), cdr(s)) = s]

  A term is said to be {i canonical} in [P], if it does not
  contain any redex of the form [car(cons(.,.))],  [cdr(cons(.,.))],
  or [cons(car(.), cdr(.))]. For [a], [b] in canonical form: [a = b]
  holds in [P] iff [a] is syntactically equal to [b] (that is,
  {!Term.eq}[a b] holds).

  This module provides
  - constructors {!Product.mk_car}, {!Product.mk_cdr}, and
    {!Product.mk_cons} for building up canonical terms in [P].
  - a solver {!Product.solve} for solving equalities in [P].
*)

val is_interp : Term.t -> bool
  (** [in_interp a] holds if the top-level function symbol is 
    in the signature of [P]. *)

val is_pure : Term.t -> bool
  (** [is_pure a] holds if every function symbol in [a] is in [P];
    that is, only variables occur at uninterpreted positions. *)

val mk_cons : Term.t -> Term.t -> Term.t
  (** Given canonical terms [a], [b], [mk_cons a b] constructs a
    canonical term for representing a pair of [a] and [b]. *)

val mk_car : Term.t -> Term.t
  (** Given a canonical term [a],  [mk_car a] constructs a
    canonical term for representing the first projection on [a]. *)
 
val mk_cdr : Term.t -> Term.t
  (** Given a canonical term [a],  [mk_cdr a] constructs a
    canonical term for representing the second projection on [a]. *)

val mk_tuple : Term.t list -> Term.t
  (** For [n > 0] canonical terms [ai], [0 <= i <= n], 
    [mk_tuple [a1;a2;...;an]] constructs a canonical 
    representation of [(a1, ((a2, ...)...), an)]. *) 

val mk_proj : int -> Term.t -> Term.t
  (** [mk_proj i a] projects the term [ai] in a tuple 
    of the form  [(a1, ((a2, ...)...), an)]. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] applies [f] at uninterpreted positions of [a]
    an builds a canonical term from the results.  More precisely,
    - [map f (mk_cons a b)] equals [mk_cons (map f a) (map f b)]
    - [map f (mk_car a)] equals [mk_car (map f a)]
    - [map f (mk_cdr a)] equals [mk_cdr (map f a)]
    - Otherwise, [map f x] equals [f x]. 
    If [f] is the identity function on uninterpreted terms in [a], then
    [map f a] is [==] to [a]. *)

val apply: Term.Equal.t -> Term.t -> Term.t
  (** [apply (x, b) a] replaces uninterpreted occurrences 
    of [x] in [a] with [b], and returns a canonical form. *)

val sigma : Sym.product -> Term.t list -> Term.t
  (** [sigma op l] applies the function symbol [op] from the pair 
    theory to the list [l] of argument terms to build a canonical
    term equal to [op(l)]. *)

val solve : Term.Equal.t -> Term.Equal.t list
  (** Given an equality [a = b], the pair solver [solve(a, b)] 
    - raises the exception {!Exc.Inconsistent} iff the equality 
    [a = b] does not hold in [P], or
    - returns a solved list of equalities [x1 = a1;...;xn = an] 
    with [xi] variables in [a] or [b], the [xi] are pairwise disjoint, 
    and no [xi] occurs in any of the [ai]. The [ai] are all in
    canonical form, and they might contain newly generated variables
    (see {!Term.Var.mk_fresh}). *)


