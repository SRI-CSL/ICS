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

(** Theories
  
  @author Harald Ruess
*)

type t

val eq : t -> t -> bool

val pp : Format.formatter -> t -> unit

val create : string -> t

val of_string : string -> t
  (** [of_string n] returns theory identifier [i]
    if [to_string i] equals to [n]; otherwise the 
    result is undefined. *)

val to_string : t -> string
  (** [to_string i] returns a name for theory [i]. *)

module Description : sig

  val add : t -> string -> unit

  val get : t -> string
    (** Return informal description of the argument theory. *)

end

module Set : (Set.S with type elt = t)
  (** Finite sets of theories. *)
  
module Map : (Map.S with type key = t)
  (** Finite maps with theories as domain. *)
  
module Hash :  (Hashtbl.S with type key = t)
  (** Hash table with theories as keys. *)
