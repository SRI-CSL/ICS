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

(** Finite sets over ordered types.

  This module implements a finite set data structure, given a total 
  ordering function over the set elements. The implementation uses
  {i Splay trees} for dynamic balancing of binary tree representations.
  
  Set operations such as insertion or deletion are {i destructive}. 

  Insertion of a new element, for example, is of {i amortized} 
  logarithmic cost.

  @author Harald Ruess
*)

(** Input signature of the functor {!Sets.Make}. *)
module type OrderedType = sig
  type t
    (** The type of the set elements. *)

  val compare : t -> t -> int
    (** A total ordering function over the set elements
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the elements [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is
      the generic structural comparison function [Pervasives.compare]. *)

  val pp : Format.formatter -> t -> unit
    (** Printer for type [t]. *)
end

(** Output signature of the functor {!Sets.Make}. *)
module type S = sig
  type elt
    (** The type of the set elements. *)
    
  type t
    (** The type of sets. *)
    
  val empty: unit -> t
    (** The empty set. *)

  val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)
    
  val is_empty: t -> bool
    (** Test whether a set is empty or not. *)
    
  val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

  val copy : t -> t
    (** [copy s] returns a set [s'] such that [s] and [s'] are
      equal and operations on [s'] do not affect [s] in the sense
      that new elements are added or deleted. Copy allocates 
      at most one new ocaml value. *)
    
  val add: elt -> t -> unit
    (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val union: t -> t -> unit
    (** Set union. *)
    
  val remove: elt -> t -> unit
    (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)

  val replace: elt -> elt -> t -> unit
    (** [replace x y s] is [add y (remove x s)]. *)
    
  val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
      the set [s2]. *)
    
  val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
      The order in which the elements of [s] are presented to [f]
      is unspecified. *)
    
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s].
      The order in which elements of [s] are presented to [f] is
      unspecified. *)
    
  val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
      satisfy the predicate [p]. *)
    
  val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)
    
  val cardinal: t -> int
    (** Return the number of elements of a set. *)
    
  val to_list: t -> elt list
    (** Return the list of all elements of the given set.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Sets.Make}. *)

  val of_list : elt list -> t
    (** Return a set with the elements in the argument list. *)
    
  val min_elt: t -> elt
    (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the set is empty. *)
    
  val max_elt: t -> elt
    (** Same as {!Sets.S.min_elt}, but returns the largest element 
      of the given set. *)

  val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
      the set is empty. Which element is chosen is unspecified,
      but equal elements will be chosen for equal sets. *)
    
  val choose_if: (elt -> bool) -> t -> elt
    (** [choose_if p s] returns [x] when [mem x s] and [p x] holds;
      otherwise [Not_found] is raised. *)
    
  val pp : Format.formatter -> t -> unit
    (** Pretty-print a set on given formatter. *)

  val equal : t -> t -> bool
    (** [equal s1 s2] iff [s1] and [s2] as sets are equa. *)
end


module Make(Ord : OrderedType) : (S with type elt = Ord.t)
  (** Functor building an implementation of the set structure
    given a totally ordered type. *)


(**/**)

(** Following for debugging only. *)
module Test : sig
  val numofprobes : int ref
    (** Number of probes in a {!Sets.Test.run} simulation. *)

  val numofsets : int ref
    (** Maximum number of different sets in a {!Sets.Test.run} simulation. *)

  val maxelt : int ref
    (** Maximum element in a set. *)

  val init : unit -> unit 
    (** Seeding random generators for {!Sets.Test.run}. *)

  val run : unit -> unit 
    (** Run a random simulation. *)
end
