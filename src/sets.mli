
(** Finite sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
*)

(** Input signature of the functor {!Set.Make}. *)
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
      the generic structural comparison function {!Pervasives.compare}. *)
  val pp : Format.formatter -> t -> unit
    (** Printer for type [t]. *)
end

(** Output signature of the functor {!Set.Make}. *)
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
      given to {!Set.Make}. *)
    
  val min_elt: t -> elt
    (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the set is empty. *)
    
  val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
      given set. *)

  val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
      the set is empty. Which element is chosen is unspecified,
      but equal elements will be chosen for equal sets. *)
    
  val choose_if: (elt -> bool) -> t -> elt
    
  val pp : Format.formatter -> t -> unit
end


(** Functor building an implementation of the set structure
  given a totally ordered type.  The implementation uses 
  balanced binary trees, and is therefore reasonably efficient: 
  insertion and membership take time logarithmic in the size of the set, 
  for instance. *)
module Make(Ord : OrderedType) : S with type elt = Ord.t
