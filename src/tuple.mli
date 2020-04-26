(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** {i Open inference system for the theory of tuples}.

    This module provides an implementation of the theory of tuples together
    with an open inference system for online processing of tuple equalities.

    @author Harald Ruess *)

(** {i Term variables.} *)
module type VAR = sig
  (** Representation of variables. *)
  type t

  val equal : t -> t -> bool
  (** Equality test on variables. *)

  val compare : t -> t -> int
  (** A {i total ordering function} over elements. This is a two-argument
      function [compare] such that [compare e1 e2] is zero iff [equal e1 e2]
      and [compare e1 e2] is negative iff [compare e2 e1] is positive. A
      total order [<<] might be defined as [e1 << e2] iff
      [compare e1 e2 <= 0]. *)

  val hash : t -> int
  (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
  (** Printing an element onto the given formatter. *)

  val fresh : unit -> t
  (** Creating a fresh variable. Here, the notion of freshness depends on
      the context. *)
end

(** The {i signature} [T] consists of the

    - the [n]ary [tuple(n)] for constructing tuples of length [n]
      ([n >= 0]), and
    - the monadic function symbol [proj(i,n)] for the [i]th projection of an
      [n]-tuple ([0 <= i < n]).

    The tuple theory [T] is axiomized by the following universally
    quantified formulas.

    - [proj(i,n)(tuple(n)(t{0},...,t{i},...,t{n-1})) = t{i}]
    - [tuple(n)(proj(0,n)(t),...,(proj(n-1,n)(t))) = t]
    - [tuple(n)(t{0},...,t{i},...,t{n-1}) <> t{i}]
    - [proj(i{1},n{1})(...(proj(i{k},n{k})(t))...) <> t]

    A term is said to be {i canonical} in [T], if it does not contain any
    redex of the form [proj(i,n)(tuple(n)(t))] or
    [tuple(n)(proj(0,n)(t),...,(proj(n-1,n)(t)))]. For [s], [t] canonical :
    [s = t] holds in [T] iff [s] is syntactically equal to [t].

    This module provides

    - constructors for building up canonical terms in [T].
    - a [map] function for homomorphically applying a function [f] at all
      variable positions of a pure term.
    - a solver [solve] for solving equalities in [T].

    Altogether, the theory of tuples is a {i Shostak} theory. (see also
    module {!Shostak}) *)
module type T = sig
  (** Representation of variables. *)
  type var

  (** Representation of terms. *)
  type t

  val arity : t -> int
  (** Arity of a term. *)

  val arg : int -> t -> t
  (** [arg i t] returns the [i]th argument of a term [t] with arity [n] and
      [0 <= i < n]. *)

  val hash : t -> int
  (** Nonnegative hash value of a term. *)

  val equal : t -> t -> bool
  (** [equal s t] holds iff [s = t] is valid in the theory [T] of tuples. *)

  val diseq : t -> t -> bool
  (** [diseq s t] holds iff [s <> t] is valid in the theory of tuples. *)

  val compare : t -> t -> int
  (** [compare s t] equals [0] iff [equal s t] holds. Furthermore,
      [compare s t > 0] iff [compare t s < 0]. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing a term on given formatter. *)

  val is_var : t -> bool
  (** [is_var t] holds iff [t] represents a term variable. *)

  val of_var : var -> t
  (** [of_var x] constructs a term [t] representing the variable [x]. *)

  val to_var : t -> var
  (** For a variable term [t], [to_var t] returns the corresponding variable
      [x]. In particular, [to_var(of_var x)) = x]. *)

  val tuple : t array -> t
  (** For a array [a] of length [n] with [i]th entires [t{i}] [tuple a]
      creates a [T]-canonical term [t] with [t = tuple(n)(t{0},...,t{n-1})]
      valid in [T]. *)

  val nil : t
  (** [nil] is the canonical term [tuple(0)()]. *)

  val pair : t -> t -> t
  (** [pair s t] creates a [T]-canonical term [t] with [t = tuple(2)(s, t)]
      valid in [T]. *)

  val triple : t -> t -> t -> t
  (** [pair s t r] creates a [T]-canonical term [t] with
      [t = tuple(3)(s, t, r)] valid in [T]. *)

  val is_pair : t -> bool
  (** [is_pair(t)] holds iff [t] is of the form [tuple(2)(s0,s1)]. *)

  val is_triple : t -> bool
  (** [is_triple(t)] holds iff [t] is of the form [tuple(3)(s0,s1,s2)]. *)

  val is_tuple : int -> t -> bool
  (** [is_tuple n t] holds iff [t] is of the form
      [tuple(n)(s{0},...,s{n-1})]. *)

  val is_proj : t -> bool
  (** [is_proj t] holds iff [t] is a term of the form [proj(i,n)(t)]. *)

  val proj : int -> int -> t -> t
  (** For [0 <= i < n], [proj i n t] constructs a [T]-canonical term [s]
      with [s = proj(i,n)(t)] is valid in [T]. *)

  val map : (var -> t) -> t -> t
  (** [map f t] replaces all occurrences in [t] of a variable [x] with [f x]
      and puts the result in canonical form. *)

  val exists : (var -> bool) -> t -> bool
  (** [exists p t] holds iff there is some variable [x] in [t] with [p x]. *)

  val for_all : (var -> bool) -> t -> bool
  (** [for_all p t] holds iff for all variables [x] in [t] it is the case
      that [p x] holds. *)

  val occurs : var -> t -> bool
  (** [occurs x t] holds iff [x] occurs in term [t]. *)

  val choose : t -> var
  (** [choose t] chooses an arbitrary variable in [t]; if [t] is ground,
      [Not_found] is raised. *)

  val replace : var -> t -> t -> t
  (** For [x] does not occur in [s], [replace x s t] replaces all
      occurrences of [x] in [t] with [s] and puts the resulting term in
      canonical form. *)

  val rename : var -> var -> t -> t
  (** [rename x y t] replaces all occurrences of [x] in [t] with [y] and
      puts the result in canonical form. *)

  val iter : (var -> unit) -> t -> unit
  (** [iter f t] applies [f x] for all variables [x] occurring in [t]. *)

  (** A substitution is a finite set of bindings of the form
      [x{1} |-> t{1}, ..., x{n} |-> t{n}] with [x{i}] does not occur in any
      [t{j}]. *)
  module Subst : Subst.S with type var = var and type trm = t

  (** Exception for flagging unsatisfiability. *)
  exception Unsat

  val solve : t -> t -> Subst.t
  (** For two terms [s], [t], the solver [solve s t]

      - raise [Unsat] if [s = t] unsatisfiable in theory [T], and it
      - returns a substitution [x{1} |-> t{1}, ...,x{n} |-> t{n}] with
        [x{i}] a variable either in [s] or [t] and
        [x{1} = t{1} & ... & x{n} = t{n}] is [T]-equivalent to [s = t]. *)
end

(** {i Implementation of tuple theory} with given variables [Var.t]. *)
module Tuple (Var : VAR) : T with type var = Var.t

(** {i Inference system for tuple theory.} Given an implementation of
    variables [Var], the Shostak theory [Tuple], and the variable
    partitioning inference system [IF], {!Tuple.Infsys} constructs a closed
    inference system for the theory of tuples. *)
module Infsys
    (Var : VAR)
    (Tuple : T with type var = Var.t)
    (IF : Shostak.V with type var = Var.t) :
  Shostak.INFSYS with type var = Var.t and type trm = Tuple.t
