(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Datatype of use lists. (should be moved to {!Solution}.

  @author Harald Ruess
*)

type t 

val mem : Term.t -> t -> bool

val apply : t -> Term.t -> Term.Set.t

val find : t -> Term.t -> Term.Set.t

val set : Term.t -> Term.Set.t -> t -> t

val empty : t
  (** [empty] use lists. *)

val add : Term.t -> Term.t -> t -> t
  (** [add x a use] adds [x] to the use of [y] for each variable in [a]. *)


val remove : Term.t -> Term.t -> t -> t
  (** [remove x a s] deletes [x] from the use of [y] for each variable in [a]. *)

