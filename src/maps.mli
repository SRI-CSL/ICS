
(** Finite, destructive functions over ordered types.
  
  This module implements finite maps, given a total ordering function
  over the keys.  All update operations over maps are destructive 
  (side-effects).
*)


(** Input signature of the functor {!Map.Make}. *)
module type OrderedType = 
  sig
    type t
      (** The type of the map keys. *)
    val compare : t -> t -> int
      (** A total ordering function over the keys.
        This is a two-argument function [f] such that
        [f e1 e2] is zero if the keys [e1] and [e2] are equal,
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
        Example: a suitable ordering function is
        the generic structural comparison function {!Pervasives.compare}. *)
    val pp : Format.formatter -> t -> unit
      (** Printer for type [t]. *)
  end
 

(** Output signature of the functor {!Map.Make}. *)
module type S =
  sig
    type key
      (** The type of the map keys. *)
      
    type 'a t
      (** The type of maps from type [key] to type ['a]. *)
      
    val empty: unit -> 'a t
      (** The empty map. *)

    val is_empty: 'a t -> bool
      (** Test for empty map. *)

    val find: key -> 'a t -> 'a
      (** [find x m] returns the current binding of [x] in [m],
	or raises [Not_found] if no such binding exists. *)
      
    val set: key -> 'a -> 'a t -> unit
      (** [add x y m] returns a map containing the same bindings as
	[m], plus a binding of [x] to [y]. If [x] was already bound
	in [m], its previous binding disappears. *)
      
    val remove: key -> 'a t -> unit
      (** [remove x m] returns a map containing the same bindings as
	[m], except for [x] which is unbound in the returned map. *)

    val copy : 'a t -> 'a t
      
    val mem: key -> 'a t -> bool
      (** [mem x m] returns [true] if [m] contains a binding for [x],
	and [false] otherwise. *)
      
    val iter: (key -> 'a -> unit) -> 'a t -> unit
      (** [iter f m] applies [f] to all bindings in map [m].
	[f] receives the key as first argument, and the associated value
	as second argument. The order in which the bindings are passed to
	[f] is unspecified. Only current bindings are presented to [f]:
	bindings hidden by more recent bindings are not passed to [f]. *)
   
      
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
	where [k1 ... kN] are the keys of all bindings in [m],
	and [d1 ... dN] are the associated data.
	The order in which the bindings are presented to [f] is
	unspecified. *)

    val for_all : (key -> 'a -> bool) -> 'a t -> bool

    val to_list : 'a t -> (key * 'a) list

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end

(** Functor building an implementation of the map 
  structure given a totally ordered type. *)
module Make(Ord : OrderedType) : S with type key = Ord.t
