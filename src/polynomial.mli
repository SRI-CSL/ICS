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

(** Polynomials.

    A {i polynomial} [p] is of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}]
    with [c{i}] nonzero {i coefficients} and [x{i}] {i indeterminates}.

    - [c{0}] is called the {i constant} of [p]
    - [c{1}*x{1} + ... + c{n}*x{n}] are the {i monomials} of [p],
    - [c{i}] is the {i coefficient} of [x{i}] in [p], and
    - [x{i}] is in the {i domain} of [p].

    This module provides a functor for constructing and manipulating
    polynomials over a given field of coefficients {!Polynomial.COEFF} and
    indeterminates {!Polynomial.INDETERMINATE}.

    @author Harald Ruess *)

(** {i Coefficients of polynomials.} Signature of the {i coefficients}
    argument of the functor {!Polynomial.Make}. The structure
    [(t, add, neg, zero, mul, one, inv)] is supposed to be a {i field}. *)
module type COEFF = sig
  (** Representation type for coefficients. *)
  type t

  val equal : t -> t -> bool
  (** Equivalence relation on coefficients. *)

  val compare : t -> t -> int

  val hash : t -> int
  (** Nonnegative hash value, such that [equal p q] implies
      [hash p = hash q]. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing of coefficients. *)

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val inv : t -> t

  val random : unit -> t
  (** Generate a random coefficient. *)
end

(** {i Indeterminates of polynomials.} Signature of the {i indeterminates}
    argument of the functor {!Polynomial.Make}. *)
module type INDETERMINATE = sig
  (** Representation of indeterminates. *)
  type t

  val equal : t -> t -> bool
  (** Equivalence relation on coefficients. *)

  val compare : t -> t -> int
  (** [compare x y] is [0] whenever [equal x y] holds. Furthermore,
      [compare x y < 0] iff [compare y x > 0]. *)

  val hash : t -> int
  (** Nonnegative hash value, such that [equal p q] implies
      [hash p = hash q]. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing of indeterminates. *)

  val fresh : unit -> t
  val dummy : t
end

(** {i Polynomials}. *)
module type P = sig
  (** Indeterminates. *)
  type indet

  (** Field of coefficients. *)
  type coeff

  module Coeff : COEFF with type t = coeff
  module Indet : INDETERMINATE with type t = indet
  module Map : Maps.S with type key = indet and type value = coeff

  (** Representation of polynomials of the form
      [c{0} + c{1}*x{1} + ... + c{n}*x{n}] with [c{i}] of nonzero
      coefficients of type [coeff] and [x{j}] of type [indet]. *)
  type t = private {constant: coeff; monomials: Map.t; mutable hash: int}

  val equal : t -> t -> bool
  (** Two polynomials [c{0} + c{1}*x{1} + ... + c{n}*x{n}] and
      [d{0} + d{1}*y{1} + ... + d{,}*y{m}] are equal iff [n = m] and there
      is a permutation [pi] on [\[1..n\]] such that [c{i}] and [d{pi(i)}]
      are equal in the domain of coefficients and [x{i}] and [y{pi(i)}] are
      equal in the domain of indeterminates. *)

  val compare : t -> t -> int

  val diseq : t -> t -> bool
  (** [diseq p q] iff [p - q] disequal from [0]. *)

  val hash : t -> int
  (** Nonnegative hash value of a polynomial. Since this hash value is
      memoized, it takes constant time to recompute it. *)

  val mem : indet -> t -> bool
  (** [mem x p] holds iff indeterminate [x] is in the domain of polynomial
      [p]. *)

  val const : t -> coeff
  (** [const p] returns the constant part of of polynomial [p]. *)

  val coeff : indet -> t -> coeff
  (** [coeff x p] returns the coefficient [c] of [x] in [p]. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing of a polynomial. *)

  val constant : coeff -> t
  (** [constant c] creates a constant polynomial of the form [c]. *)

  val zero : t
  (** [zero] represents the constant polynomial [0]. *)

  val one : t
  (** [zero] represents the constant polynomial [1]. *)

  val is_zero : t -> bool
  (** [is_zero p] holds iff [equal p zero]. *)

  val is_one : t -> bool
  (** [is_zero p] holds iff [equal p zero]. *)

  val indet : indet -> t
  (** [indet x] constructs an indeterminate polynomial of the form [1*x]. *)

  val multc : coeff -> t -> t
  (** [multc c p] returns a polynomial equivalent to the multiplication
      [c * p] of the constant polynomial [c] and the polynomial [p]. *)

  val addc : coeff -> t -> t
  (** [addc c p] returns a polynomial equivalent to the sum [c + p] of the
      constant polynomial [c] and the polynomial [p]. *)

  val addm : coeff -> indet -> t -> t
  (** [addm c x p] returns a polynomial equivalent to [c*x + p]. *)

  val add : t -> t -> t
  (** [add p q] returns a polynomial equivalent to the sum [p + q] of
      polynomials [p] and [q]. *)

  val sub : t -> t -> t
  (** [sub p q] returns a polynomial equivalent to the difference [p - q] of
      polynomials [p] and [q]. *)

  val mapc : (coeff -> coeff) -> t -> t
  (** [mapc f p] for a polynomial of the form
      [c{0} + c{1}*x{1} + ... + c{n}*x{n}] returns a polynomial equivalent
      to [f(c{0}) + f(c{1})*x{1} + ... + f(c{n})*x{n}]. *)

  val minus : t -> t

  val negated : t -> t -> bool
  (** [negated p q] holds if [minus p] equals [q]. *)

  exception Valid
  exception Unsat

  val solve0 : t -> indet * t
  (** For a non-constant polynomial [p], [solve0 p] returns a
      {i solved form} [(x, q)] with [x] in the domain of [p] and [p = 0] iff
      [x = q]. Otherwise, if [p] is the [0] polynomial, then [solve0 p]
      raises [Valid], and if [p] is a constant polynomial different from
      [0], then [solve0 p] raises [Unsat]. *)

  val solve0_for : indet -> t -> t

  val rename : indet -> indet -> t -> t
  (** [rename x y p] replaces indeterminate [x] of [p] with [y]. *)

  val instantiate : indet -> coeff -> t -> t
  (** [instantiate x c p] replaces [x] with [c]. *)

  val pivot : indet -> indet -> t -> t
  (** For a polynomial [p] such that [y] in [dom p] but [x] not in [dom p],
      [pivot x y p] returns [q] such that [x = p] iff [y = q]. *)

  val eval : (indet -> coeff) -> t -> coeff
  (** For a polynomial [p] of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}],
      and a valuation [rho] which is defined on the set of indeterminates of
      [p], [eval rho p] returns the value
      [c{0} + c{1}*rho(x{1}) + ... + c{n}*rho(x{n})]. *)

  val iter : (coeff -> unit) -> (indet -> coeff -> unit) -> t -> unit
  (** For a polynomial [p] of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}],
      [iter fc fm p] applies [fc c{0}] and [fm c{i} x{i}] for [i] in
      [\[1..n\]]. The order in which these functions are applied is
      unspecified. *)

  val fold : (coeff -> 'a) -> (indet -> coeff -> 'a -> 'a) -> t -> 'a
  (** For a polynomial [p] of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}],
      [fold fc fm p] computes
      [fm x{pi(1)} c{pi{1)} (....(fm x{pi(n)} c{pi{n)} (fc c{0}))...)] with
      [pi] a fixed, but otherwise unspecified permutation on [\[1..n\]]. *)

  val map : (indet -> t) -> t -> t
  val replace : indet -> t -> t -> t

  val for_all : (coeff -> bool) -> (indet -> coeff -> bool) -> t -> bool
  (** For a polynomial [p] of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}],
      [for_all fc fm p] holds iff [fc c{0}] holds and [fm c{i} x{i}] holds
      for all [i] in [\[1..n\]]. *)

  val exists : (coeff -> bool) -> (indet -> coeff -> bool) -> t -> bool
  (** For a polynomial [p] of the form [c{0} + c{1}*x{1} + ... + c{n}*x{n}],
      [exists fc fm p] holds iff [fc c{0}] holds or [fm c{i} x{i}] holds for
      at least one [i] in [\[1..n\]]. *)

  val choose : (indet -> coeff -> bool) -> t -> indet * coeff
  val make : coeff -> Map.t -> t

  exception Nonindet
  exception Nonnum

  val d_indet : t -> indet
  val is_indet : t -> bool
  val is_constant : t -> bool
  val d_constant : t -> coeff
  val is_monomial : t -> bool
  val coeff_of_monomial : t -> coeff
  val indet_of_monomial : t -> indet
end

(** {!Polynomial.Make} constructs polynomials with the given coefficients
    and indeterminates. *)
module Make (C : COEFF) (X : INDETERMINATE) :
  P with type coeff = C.t and type indet = X.t

(**/**)

(** Following for debugging only. *)
module Test : sig
  val numofprobes : int ref
  val numofpoly : int ref
  val maxvar : int ref
  val maxcoeff : int ref
  val init : unit -> unit
  val run : unit -> unit
end
