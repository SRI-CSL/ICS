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

(** Datatype of dependency lists.

  @author Harald Ruess
*)

type t

val mem : Term.t -> t -> bool

module Set : (Sets.S with type elt = Term.t)

val find : t -> Term.t -> Set.t

val pp : Format.formatter -> t -> unit

val empty : unit -> t
  (** [empty] use lists. *)

val is_empty : t -> bool

val add : Term.t -> Term.t -> t -> unit
  (** [add x a use] adds [x] to the use of [y] for each variable in [a]. *)

val remove : Term.t -> Term.t -> t -> unit
  (** [remove x a s] deletes [x] from the use of [y] for each variable in [a]. *)

val replace : Term.t -> Term.t -> Term.t -> t -> unit
  (** [replace x y z] replaces [x] by [y] in the use of [z]. *)

val copy : t -> t
