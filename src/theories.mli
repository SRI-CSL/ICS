
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

(*i*)
open Mpa
(*i*)

(*s Classification of function symbols. *)


type t = U | T | BV | A | S


val to_string : t -> string

val of_string : string -> t

val pp : t Pretty.printer

val iter : (t -> unit) -> unit

val for_all : (t -> bool) -> bool

module Array : sig

  type 'a arr

  val create: 'a -> 'a arr

  val copy : 'a arr -> 'a arr

  val get : 'a arr -> t -> 'a
 
  val set : 'a arr -> t -> 'a -> unit

  val reset : 'a arr -> 'a -> unit

  val iter : (t -> 'a -> unit) -> 'a arr -> unit

  val for_all : ('a -> bool) -> 'a arr -> bool

  val for_all2 : ('a -> 'b -> bool) -> 'a arr -> 'b arr -> bool

  val to_list : 'a arr -> (t * 'a) list

  val pp : 'a Pretty.printer -> 'a arr Pretty.printer


end
