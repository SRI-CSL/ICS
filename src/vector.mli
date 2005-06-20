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

(** {i Datatype of vectors}

  @author Harald Ruess
*)

module type ELT = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val zero : t
  val one : t
  val add : t -> t -> t
  val mult : t -> t -> t
end

module type S = sig
  type elt
  type t
  val dim : t -> int
  val get : t -> int -> elt 
  val sub : t -> int -> int -> t
  val postfix : t -> int -> t
  val const : int -> elt -> t
  val add : t -> t -> unit
  val map : (elt -> elt) -> t -> unit
  val iter : (elt -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit
  val ( ** ) : t -> t -> elt
end

module Make(E: ELT): (S with type elt = E.t)
