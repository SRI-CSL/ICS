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

(** Weak hash tables

  @author Harald Ruess

  Hash tables with weak keys.
*)

(** The input signature of the functor {!Weakhash.Make}. *)
module type HASH = sig
  type t
    (** The type of the hashtable keys. *)
  val equal : t -> t -> bool
    (** The equality predicate used to compare keys. *)
  val hash : t -> int
    (** A hashing function on keys. It must be such that if two keys are
      equal according to [equal], then they have identical hash values
      as computed by [hash].
      Examples: suitable ([equal], [hash]) pairs for arbitrary key
      types include
      ([(=)], {!Hashtbl.hash}) for comparing objects by structure, and
      ([(==)], {!Hashtbl.hash}) for comparing objects by addresses
      (e.g. for mutable or cyclic keys). *)
end


(** The output signature of the functor {!Weakhash.Make}. *)
module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val count : 'a t -> int
    val add : 'a t -> key -> 'a -> unit
    val find : 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val to_list : 'a t -> (key * 'a) option list
    type stats = { length : int; count : int; del: int }
    val stats : 'a t -> stats
  end

module Make (H : HASH) : (S with type key = H.t)
  (** Functor building an implementation of the hashtable structure.
    The functor [Weakhash.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    This association is {i weak} in the sense that [key]
    can be garbage collected even though it is a [key]
    in the hash table.  On garbage collection of [key],
    the association of [key] with its value is deleted from
    the hash table. *)
  
