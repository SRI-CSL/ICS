
(*i
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
 * 
 * Author: Harald Ruess
 i*)

(*s Module [Name]: Datatype of names. *)

type t

val of_string : string -> t
val to_string : t -> string

val eq : t -> t -> bool

val cmp : t -> t -> int

val pp : Format.formatter -> t -> unit

val hash : t -> int

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)

val pp_map: (Format.formatter -> 'a -> unit)
               -> Format.formatter -> 'a Map.t -> unit
