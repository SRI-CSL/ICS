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

(** Theory names.
  
  @author Harald Ruess
*)

type t = 
  | Top
  | U
  | A 
  | P 
  | F

val equal : t -> t -> bool

val sub : t -> t -> bool

val pp : Format.formatter -> t -> unit

val to_string : t -> string
  (** [to_string i] returns a name for theory [i]. *)

val description : t -> string
  (** Return a description of the theory. *)

module Set : (Set.S with type elt = t)
  (** Finite sets of theories. *)
  
module Map : (Map.S with type key = t)
  (** Finite maps with theories as domain. *)
  
module Hash :  (Hashtbl.S with type key = t)
  (** Hash table with theories as keys. *)

module type T = sig
  val theory : t
end

module Top: T
module U: T
module A: T
module P: T
module F: T
