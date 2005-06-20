(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** {i Type declarations and constructors}

  @author Harald Ruess
*)

(** {i Equality types}. *)
module type EQUAL = sig
  type t
    (** Representation type. *)

  val equal : t -> t -> bool
    (** Equality on representations. *)

  val hash : t -> int
    (** Nonnegative hash functions. *)

  val pp : Format.formatter -> t -> unit
    (** Printing an element onto the given formatter. *)
end

(** {i Totally ordered types}. *)
module type ORDERED = sig
  type t  
    (** Representation type. *)

  val equal : t -> t -> bool
    (** Equality on representations. *)
    
  val compare : t -> t -> int
    (** A {i total ordering function} over elements.
      This is a two-argument function [compare] such that
      [compare e1 e2] is zero iff [equal e1 e2] and [compare e1 e2]
      is negative iff [compare e2 e1] is positive. A total order [<<]
      might be defined as [e1 << e2] iff [compare  e1 e2 <= 0]. *)
  val hash : t -> int
    (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
    (** Printing an element onto the given formatter. *)
end

(** {i Product types} for representing lexicographically
  ordered pairs [(s, t)]. *)
module type PRODUCT = sig
  type elt1
    (** Representation type of first projection. *)

  type elt2 
    (** Representation type of second projection. *)

  type t
    (** Representation type of pairs. *)
  val make : elt1 -> elt2 -> t
    (** [make e1 e2] constructs a pair [(e1, e2)]. *)

  val fill : t -> elt1 -> elt2 -> unit
    (** [fill p e1 e2] sets the first projection of pair [p]
      to [e1] and the second projection to [e2]. *)

  val lhs : t -> elt1
    (** [lhs p] returns the first (or left-hand side) projection of pair [p]. *)

  val rhs : t -> elt2
    (** [lhs p] returns the first (or left-hand side) projection of pair [p]. *)

  val equal : t -> t -> bool
    (** Two pairs [(e1, e2)], [(e1', e2')] are equal iff the first projections
      [e1] and [e1'] are equal wrt equality type [elt1] and [ew] and [e2']
      are equal wrt equality type [elt2]. *)

  val compare : t -> t -> int
    (** For pairs [p1], [p2] of the form [(e1, e1')] and [(e2, e2')], respectively,
      - [compare p1 p2] equals [0] iff [equal p1 p2].
      - [compare p1 p2] is negative iff [compare p2 p1] is positive. 
      - if [compare e1 e1'] equals [0], then [compare p1 p2] equals [compare e2 e2']. *)

  val hash : t -> int  
    (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
    (** Printing an element onto the given formatter. *)
end 

(** {i Product types} for representing ordered pairs [(s, t)] 
  with [s] of type [Ordered1.t] and [t] of type [Ordered2.t]. *)
module Product(Ordered1: ORDERED)(Ordered2: ORDERED)
  : (PRODUCT with type elt1 = Ordered1.t 
	     and type elt2 = Ordered2.t)


module Triple(Ordered1: ORDERED)(Ordered2: ORDERED)(Ordered3: ORDERED) : sig
  type elt1 = Ordered1.t
    (** Representation type of first projection. *)

  type elt2 = Ordered2.t
    (** Representation type of second projection. *)

  type elt3 = Ordered3.t
    (** Representation type of third projection. *)

  type t
    (** Representation type of triples *)

  val make : elt1 -> elt2 -> elt3 -> t
    (** [make e1 e2 e3] constructs a triple [(e1, e2, e3)]. *)

  val fill : t -> elt1 -> elt2 -> elt3 -> unit
    (** [fill p e1 e2 e3] sets the first projection of pair [p]
      to [e1] and the second projection to [e2]. *)

  val arg1 : t -> elt1
  val arg2 : t -> elt2
  val arg3 : t -> elt3

  val dummy : unit -> t
   
  val equal : t -> t -> bool
   
  val compare : t -> t -> int
  
  val hash : t -> int  
    (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
    (** Printing an element onto the given formatter. *)
end 
