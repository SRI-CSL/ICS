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

(** Theory of coproducts

  @author Harald Ruess

  The signature of this theory consists of symbols for left and
  right injection and the corresponding left and right accessors.
  [product] for constructing tuples and of the family of unary 
  
  The equality theory of coproducts is given by
  - [inl(outl(a)) = a]
  - [inr(outr(a)) = a]
  - [outr(inr(a)) = a]
  - [outl(inl(a)) = a]

  All terms constructed by this module are {b canonical} in the sense
  that they do not contain any redeces given by a lhs of any equation
  above.  Thus, {!Term.eq}[(a, b)] iff [a = b] is derivable in the
  coproduct equality theory.
*)


(** {6 Signature} *)

val inl : Sym.t
val inr : Sym.t
val outl : Sym.t
val outr : Sym.t


(** {6 Constructors} *)

val mk_inl : Term.t -> Term.t
  (** [mk_inl a] constructs a canonical term equivalent to
    [App(inl, [a])] in the theory of coproducts. *)

val mk_inr : Term.t -> Term.t
  (** [mk_inr a] constructs a canonical term equivalent to
    [App(inr, [a])] in the theory of coproducts. *)

val mk_outl : Term.t -> Term.t
 (** [mk_outl a] constructs a canonical term equivalent to
   [App(outl, [a])] in the theory of coproducts. *)

val mk_outr : Term.t -> Term.t
  (** [mk_outr a] constructs a canonical term equivalent to
    [App(outr, [a])] in the theory of coproducts. *)
  
val mk_inj : int -> Term.t -> Term.t
  (** Generalized injection
    - [mk_inj 0 a = mk_inl a]
    - [mk_inj 1 a = mk_inr a]
    - [mk_inj i a = mk_inr (mk_inj (i - 1) x)] if [i > 1]
    - Otherwise, the value of [mk_inj] is unspecified. *)

val mk_out : int -> Term.t -> Term.t
  (** Generalized coinjection
    - [mk_out 0 a = mk_outl a]
    - [mk_out 1 a = mk_outr a]
    - [mk_out i a = mk_outr (mk_out (i - 1) x)] if [i > 1]
    - Otherwise, the value of [mk_out] is unspecified. *)


(** {6 Iterators} *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** If [x1, ..., xn] are the variables and uninterpreted terms of [a],
    then [fold f a e] is  [f (... (f (f e x1) x2) ...) xn]. *)
  
val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f (constr a)] equals [constr (map f a)] for
    [cnstrnt] one of the constructors [mk_inl], [mk_inr],
    [mk_outl], [mk_outr]. Otherwise, [map f x] equals [f x] *)


(** {6 Canonizer} *)

val sigma : Sym.coproduct -> Term.t list -> Term.t
  (** [sigma op [a]] applies [mk_inl a] if [op] is equal to
    the [inl] symbol. Similarly for all the symbols.  Notice
    that the second argument is required to be a unary list. *)


(** {6 Solver} *)

val solve : Fact.equal -> Fact.equal list
  (** [solve e] raises the exception {!Exc.Inconsistent} if 
    the equality [e] is inconsistent in the theory of tuples.
    Otherwise, it returns a list of solved equalities such that the
    conjunction of these equalities is equivalent to [e] in the
    theory of tuples. The solved equalities are of the form [x = a]
    where [x] is a variable in [e], and none of the lhs variables [x]
    in the solved form occurs in any of the rhs [a]. In addition, 
    every rhs [a] is canonical and contains only variables in [e]. *)

