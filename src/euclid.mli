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

(** Module [Euclid]: Euclidean solver for diophantine equations.

    @author Harald Ruess *)

(** Input signature of the functor {!Euclid.Make}

    May be instantiated with a structure isomorphic to the rationals. *)
module type RAT = sig
  (** Rationals. *)
  type t

  val eq : t -> t -> bool
  (** Equality. *)

  val pp : Format.formatter -> t -> unit
  (** Print a rational. *)

  val ( + ) : t -> t -> t
  (** Addition. *)

  val zero : t
  (** Neutral element of addition. *)

  val neg : t -> t
  (** Negation of a a rational. *)

  val ( * ) : t -> t -> t
  (** Multiplication. *)

  val one : t
  (** Neutral element of multiplication. *)

  val ( / ) : t -> t -> t
  (** Inverse of Multiplication. *)

  val floor : t -> t
  (** Floor function on rationals. *)

  val is_int : t -> bool
  (** Integer test of a rational. *)
end

(** Polynomials as used for the input signature for {!Euclid.General}. *)
module type POLYNOMIAL = sig
  (** Coefficients of polynomial. *)
  type q

  (** Representation of polynomials. *)
  type t

  val pp : Format.formatter -> t -> unit
  (** Printing polynomials. *)

  val fresh : unit -> t
  (** Construct a {i fresh} polynomial representing an indeterminate. The
      notion of freshness depends on the context of use. *)

  val of_q : q -> t
  (** Create a constraint polynomial. *)

  val add : t -> t -> t
  (** Add two polynomials. *)

  val multq : q -> t -> t
  (** Multiply a polynomial by a constant factor. *)
end

(** Particular solution of linear diophantine equations. *)
module Particular (Q : RAT) : sig
  val euclid : Q.t -> Q.t -> Q.t * Q.t * Q.t
  (** Given two rational numbers [p], [q], [euclid p q] finds integers [x],
      [y], [(p, q)] satisfying [p * x + q * y = (p, q)], where [(p, q)]
      denotes the greatest common divisor of [p], [q]. *)

  (** Raised by [solve]. *)
  exception Unsolvable

  val solve : Q.t list -> Q.t -> Q.t * Q.t list
  (** [solve \[c1;...;cn\] b] yields a particular solution for a linear
      diophantine equation [c0 * x0 + ... + cn * xn = b] with nonzero,
      rational coefficients [ci], for [i = 1,...,n] with [n >= 1]. In case
      such a solution exists, it returns the gcd of the coefficients and a
      list of solutions [li] for variable [xi]. *)
end

(** General solution of linear diophantine equations. *)
module Solve (Q : RAT) (P : POLYNOMIAL with type q = Q.t) : sig
  exception Unsolvable

  val solve : Q.t list -> Q.t -> P.t list
end

(**/**)

module Test : sig
  val num_of_tests : int ref
  val max_num_of_variables : int ref
  val max_rat : int ref
  val run : unit -> unit
end
