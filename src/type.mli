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

(** {i Type declarations and constructors}

    @author Harald Ruess *)

(** {i Equality types}. *)
module type EQUAL = sig
  (** Representation type. *)
  type t

  val equal : t -> t -> bool
  (** Equality on representations. *)

  val hash : t -> int
  (** Nonnegative hash functions. *)

  val pp : Format.formatter -> t -> unit
  (** Printing an element onto the given formatter. *)
end

(** {i Totally ordered types}. *)
module type ORDERED = sig
  (** Representation type. *)
  type t

  val equal : t -> t -> bool
  (** Equality on representations. *)

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

  val dummy : t
end

(** {i Product types} for representing lexicographically ordered pairs
    [(s, t)]. *)
module type PRODUCT = sig
  (** Representation type of first projection. *)
  type elt1

  (** Representation type of second projection. *)
  type elt2

  (** Representation type of pairs. *)
  type t

  val make : elt1 -> elt2 -> t
  (** [make e1 e2] constructs a pair [(e1, e2)]. *)

  val fill : t -> elt1 -> elt2 -> unit
  (** [fill p e1 e2] sets the first projection of pair [p] to [e1] and the
      second projection to [e2]. *)

  val lhs : t -> elt1
  (** [lhs p] returns the first (or left-hand side) projection of pair [p]. *)

  val rhs : t -> elt2
  (** [lhs p] returns the first (or left-hand side) projection of pair [p]. *)

  val equal : t -> t -> bool
  (** Two pairs [(e1, e2)], [(e1', e2')] are equal iff the first projections
      [e1] and [e1'] are equal wrt equality type [elt1] and [ew] and [e2']
      are equal wrt equality type [elt2]. *)

  val compare : t -> t -> int
  (** For pairs [p1], [p2] of the form [(e1, e1')] and [(e2, e2')],
      respectively,

      - [compare p1 p2] equals [0] iff [equal p1 p2].
      - [compare p1 p2] is negative iff [compare p2 p1] is positive.
      - if [compare e1 e1'] equals [0], then [compare p1 p2] equals
        [compare e2 e2']. *)

  val hash : t -> int
  (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
  (** Printing an element onto the given formatter. *)

  val dummy : unit -> t
end

(** {i Product types} for representing ordered pairs [(s, t)] with [s] of
    type [Ordered1.t] and [t] of type [Ordered2.t]. *)
module Product (Ordered1 : ORDERED) (Ordered2 : ORDERED) :
  PRODUCT with type elt1 = Ordered1.t and type elt2 = Ordered2.t

module Triple (Ordered1 : ORDERED) (Ordered2 : ORDERED) (Ordered3 : ORDERED) : sig
  (** Representation type of first projection. *)
  type elt1 = Ordered1.t

  (** Representation type of second projection. *)
  type elt2 = Ordered2.t

  (** Representation type of third projection. *)
  type elt3 = Ordered3.t

  (** Representation type of triples *)
  type t

  val make : elt1 -> elt2 -> elt3 -> t
  (** [make e1 e2 e3] constructs a triple [(e1, e2, e3)]. *)

  val fill : t -> elt1 -> elt2 -> elt3 -> unit
  (** [fill p e1 e2 e3] sets the first projection of pair [p] to [e1] and
      the second projection to [e2]. *)

  val arg1 : t -> elt1
  val arg2 : t -> elt2
  val arg3 : t -> elt3
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val hash : t -> int
  (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
  (** Printing an element onto the given formatter. *)

  val dummy : unit -> t
end
