(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** {i Data type of finite sets with disjoint union.}

  @author Harald Ruess
*)

(** Used for input signature of {!Setunion.Make}. *)
module type ELT = Type.EQUAL


(** {i Finite sets}. Output signature of {!Setunion.Make}. *)
module type S = sig
  type elt
    (** Element type. *)

    (** Representation of sets. 
      - [Empty] is the empty set;
      - [Singleton(x)] is the singleton set [{x}];
      - [Add(x, s)] represents [{x} union s], as 
      an invariant [x] not in [s];
      - [Union(s1, s2)] represents [s1 union s2],
      as an invariant [s1] and [s2] are disjoint. *)
  type t = private
     | Empty
     | Singleton of elt
     | Add of elt * t
     | Union of t * t

  val equal : t -> t -> bool
    (** [equal s1 s2] holds iff for all elements,
      [mem x s1] holds iff [mem x s2] holds. *)

  val is_empty : t -> bool
    (** [is_empty s] holds iff [s] represents the
      empty set. *)

  val is_singleton : t -> bool
    (** [is_singleton s] iff [s] represents a singleton
      set with only one elements. *)

  val pp : Format.formatter -> t -> unit
    (** Print a set on the given formatter. *)

  val mem :  elt -> t -> bool
    (** [mem x s] holds iff [x] is an element in [s]. *)

  val empty : t
    (** The empty set. *)

  val singleton : elt -> t
    (** [singleton x] represents the singleton set [{x}]. *)

  val add : elt -> t -> t   
    (** For [x] not in [s], [add x s] represents the set [{x} union s]. *)

  val rem : elt -> t -> t
    (** For [x] in [s] , [rem x s] removes [x] from [s]. *)

  val disjoint : t -> t -> bool
    (** [disjoint s1 s2] holds iff the sets [s1] and [s2] are disjoint. *)

  val union : t -> t -> t
    (** For disjoint sets [s1] and [s2], [union s1 s2] returns the union
      of the two sets. [union] is a constant time operation. *)

  val to_list : t -> elt list
    (** [to_list s] contains all the elements of [s]. *)

  val iter : (elt -> unit) -> t -> unit
    (** [iter f s] applies [f x] to all elements [x] of [s]. *)

  val for_all : (elt -> bool) -> t -> bool
    (** [for_all p s] holds iff [p x] holds for all elements [x] of [s]. *)

  val exists : (elt -> bool) -> t -> bool
    (** [exists p s] holds iff there exists [x] in [s] with [p x]. *)

  val choose : (elt -> bool) -> t -> elt
    (** If there is an [x] such that [p x] holds, then
      [choose p s] returns such an [x]; otherwise, [Not_found] is raised. *)
end

(** {!Setunion.Make} constructs a finite set implementation with
  elements of type [Elt.t]. The [union] operator of this implementation
  is restriced to disjoint sets and is constant time. *)
module Make(Elt: ELT): (S with type elt = Elt.t)
