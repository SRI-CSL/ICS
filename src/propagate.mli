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

(** Propagation facts

  @author Harald Ruess
*)

module type S = sig 
  type t
  val clear : unit -> unit
  val is_empty : unit -> bool
  val put : t -> unit
  val get : unit -> t
  val to_list: unit -> t list
end

module Equal : (S with type t = Term.t)

module Nonneg : (S with type t = Term.t)

module Diseq : (S with type t = Term.t * Term.t)

module Cnstrnt : (S with type t = Term.t)

val is_empty : unit -> bool

val clear : unit -> unit
