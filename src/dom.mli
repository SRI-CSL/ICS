
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
 * Authors: Ritvik Sahajpa, Harald Ruess
 i*)

(*s Module [Dom]: Various subdomains of numbers. [Real] denotes the domain
 of the real numbers, [Int] all integers.  There is a reflexive subdomain 
 ordering [<] with [Int < Real] *)

type t = Int | Real

(*s Equality on domains. *)

val eq : t -> t -> bool

(*s [union d1 d2] returns [d] iff the domain of [d] is the union of the
 domains of [d1] and [d2]. *)

val union : t -> t -> t


(*s [inter d1 d2] returns [d] iff the domain of [d] is the intersection 
 of the domains of [d1] and [d2]. *)

val inter : t -> t -> t

(* [sub d1 d2] tests if the domain of [d1] is a subdomain of [d2]. *)

val sub : t -> t -> bool

(*s [cmp d1 d2] returns [Sub] ([Equal], [Disjoint], [Super]) if the domain of [d1] 
 is a subset (is equal, is disjoint, is a superset) of [d2]. *)

val cmp : t -> t -> unit Binrel.t

(*s [of_q u] returns [Int] if the rational [u] is integer and [Real] *)

val of_q : Mpa.Q.t -> t

(*s [mem q d] tests if [q] is a member of [d]. *)

val mem : Mpa.Q.t -> t -> bool

(*s Pretty-printing *)

val pp: Format.formatter -> t -> unit
