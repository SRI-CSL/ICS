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

(** {i Finite, destructive functions over ordered types.}

    This module implements finite maps, given a total ordering function over
    the keys. All update operations over maps are destructive
    (side-effects).

    The implementation uses {i Splay trees} for dynamic balancing of binary
    tree representations. Therefore, insertion of a new element, for
    example, is of {i amortized} logarithmic cost. *)

(** {i Totally ordered type.} Input signature of the functor {!Maps.Make}. *)
module type OrderedType = sig
  (** The type of the map keys. *)
  type t

  val compare : t -> t -> int
  (** A total ordering function over the keys. This is a two-argument
      function [f] such that [f e1 e2] is zero if the keys [e1] and [e2] are
      equal [f e1 e2] is strictly negative if [e1] is smaller than [e2], and
      [f e1 e2] is strictly positive if [e1] is greater than [e2]. Example:
      a suitable ordering function is the generic structural comparison
      function [Pervasives.compare]. *)

  val pp : Format.formatter -> t -> unit
  (** Printer for type [t]. *)
end

(** {i Equality type.} Input signature of the functor {!Maps.Make}. *)
module type EqType = sig
  (** The type of map values. *)
  type t

  val equal : t -> t -> bool
  (** Equality on map values. *)

  val pp : Format.formatter -> t -> unit
  (** Printer for values. *)
end

(** Output signature of the functor {!Maps.Make}. *)
module type S = sig
  (** The type of the map keys. *)
  type key

  (** The type of codomain values. *)
  type value

  (** The type of maps from type [key] to type ['a]. *)
  type t

  val empty : unit -> t
  (** The empty map. *)

  val is_empty : t -> bool
  (** Test for empty map. *)

  val singleton : key -> value -> t
  val is_singleton : t -> bool

  val find : key -> t -> value
  (** [find x m] returns the current binding of [x] in [m], or raises
      [Not_found] if no such binding exists. *)

  val set : key -> value -> t -> unit
  (** [add x y m] returns a map containing the same bindings as [m], plus a
      binding of [x] to [y]. If [x] was already bound in [m], its previous
      binding disappears. *)

  val remove : key -> t -> unit
  (** [remove x m] returns a map containing the same bindings as [m], except
      for [x] which is unbound in the returned map. *)

  val copy : t -> t
  (** [copy s] returns a set [s'] such that [s] and [s'] are equal and
      operations on [s'] do not affect [s] in the sense that new elements
      are added or deleted. Copy allocates at most one new ocaml value. *)

  val mem : key -> t -> bool
  (** [mem x m] returns [true] if [m] contains a binding for [x], and
      [false] otherwise. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** [iter f m] applies [f] to all bindings in map [m]. [f] receives the
      key as first argument, and the associated value as second argument.
      The order in which the bindings are passed to [f] is unspecified. Only
      current bindings are presented to [f]: bindings hidden by more recent
      bindings are not passed to [f]. *)

  val fold : (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)], where
      [k1 ... kN] are the keys of all bindings in [m], and [d1 ... dN] are
      the associated data. The order in which the bindings are presented to
      [f] is unspecified. *)

  val cardinal : t -> int
  (** [cardinal m] returns the number of bindings in [m]. *)

  val map : (value -> value) -> t -> t
  (** [map f m] returns a binding [m'] by replacing all bindings [k |-> v]
      in [m] with [k |-> f v]. *)

  val replace : t -> key -> key -> t
  (** [replace m k k'] returns a map [m'] obtained by replacing a binding
      [k |-> v] by [k' |-> v]. *)

  val for_all : (key -> value -> bool) -> t -> bool
  (** [for_all p m] holds iff [p k v] for all bindings [k |-> v] in map [m]. *)

  val exists : (key -> value -> bool) -> t -> bool
  (** [exists p m] holds iff [p k v] for some binding [k |-> v] in map [m]. *)

  val choose_if : (key -> value -> bool) -> t -> key * value
  (** [choose_if p m] returns [(k, v)] if there is binding [k |-> v] in [m]
      with [p k v]. Otherwise, [Not_found] is raised. *)

  val destruct : (key -> value -> bool) -> t -> key * value * t
  (** [destruct p m] returns [(k, v, m')] with [k |-> v] a binding in [m]
      such that [p k v] holds and [m'] is obtained from [m] by deleting the
      binding [k |-> v]. Otherwise, [Not_found] is raised. *)

  val to_list : t -> (key * value) list
  (** Represent a map as a list [l] with [(k, v)] in [l] iff [k |-> v] is a
      binding in [l]. *)

  val equal : t -> t -> bool
  (** Two maps [m1] and [m2] are equal iff they contain the same bindings. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print a map on formatter. *)
end

(** Functor building an implementation of a map with bindings [k |-> v]
    where the keys [k] are totally ordered. *)
module Make (Key : OrderedType) (Val : EqType) :
  S with type key = Key.t and type value = Val.t

(** {i Exponential map}. Functor building an implementation of a map with
    bindings [k |-> {k{1},...,k{n}}] where the keys [k] are totally ordered
    and values are sets of elements of keys. *)
module Expt (T : OrderedType) :
  S with type key = T.t and type value = Sets.Make(T).t
