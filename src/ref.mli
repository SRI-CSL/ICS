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

(** References

  @author Harald Ruess

  This module provides functionality for creating global references.
*)

type value = string

val get : Name.t -> value

val set : Name.t -> value -> unit

val reset : unit -> unit

val description : Name.t -> string

val iter : (Name.t -> unit) -> unit

module type KIND = sig
  type t
  val to_string : t -> value
  val of_string : value -> t
end

module type DESCRIPTION = sig
  type t
  val name : string
  val default : t
  val description : string
end

module type REF = sig
  type t 
  val set : t -> unit
  val get : unit -> t
  val reset : unit -> unit
end

module Make(Kind: KIND)(Descr: (DESCRIPTION with type t = Kind.t))
  : (REF with type t = Kind.t)

module type BOOLEAN = (REF with type t = bool)

module Boolean(Descr: (DESCRIPTION with type t = bool)): BOOLEAN

module type STRING = (REF with type t = string)

module String(Descr: (DESCRIPTION with type t = string)): STRING
