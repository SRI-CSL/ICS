
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
 * Author: Jean-Christophe Filliatre
 i*)

(*s Module [Ptset]: Finite sets implemented as Patricia trees. *)

(*i*)
open Hashcons
(*i*)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a hashed -> 'a t -> bool
val add : 'a hashed -> 'a t -> 'a t
val singleton : 'a hashed -> 'a t
val remove : 'a hashed -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val cardinal : 'a t -> int
val iter : ('a hashed -> unit) -> 'a t -> unit
val iter2 : ('a hashed -> 'a hashed -> unit) -> 'a t -> unit
val fold : ('a hashed -> 'b -> 'b) -> 'a t -> 'b -> 'b
val map : ('a hashed -> 'b hashed) -> 'a t -> 'b t
val inter : 'a t -> 'a t -> 'a t
val exists : ('a hashed -> bool) -> 'a t -> bool
val for_all : ('a hashed -> bool) -> 'a t -> bool
val filter : ('a hashed -> bool) -> 'a t -> 'a t
val to_list : 'a t -> 'a hashed list
val sub : 'a t -> 'a t -> bool
val equal : 'a t -> 'a t -> bool
val pp : (Format.formatter -> 'a hashed -> unit) -> Format.formatter -> 'a t -> unit

(*s Return one element of the given set, or raise exception [Not_found] if
  the set is empty. Which element is chosen is unspecified,
  but equal elements will be chosen for equal sets. *)
      
val choose : ('a hashed -> bool) -> 'a t -> 'a hashed

(*s [destructure s] returns an element term [a] of a nonempty set 
  [s] of terms together with the set in which [a] is removed from [s]. 
  The exception [Not_found] is raised if the argument set is empty. *)
      
val destructure : 'a t -> 'a hashed * 'a t

(*s Comparing two sets. *)

val cmp : 'a t -> 'a t -> Binrel.t

(*s Test if two sets are disjoint. *)

val is_disjoint : 'a t -> 'a t -> bool
