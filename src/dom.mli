
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
 of the real numbers, [Int] all integers, and [Nonint] all reals which are
 not integers.  Obviously, [Int] and [Nonint] domains are disjoint, and there
 there is a reflexive subdomain ordering [<] with [Int < Real], [Nonint < Real]. *)

type t = Int | Nonint | Real

(*s Equality on domains. *)

val eq : t -> t -> bool

(*s [union d1 d2] returns [d] iff the domain of [d] is the union of the
 domains of [d1] and [d2]. *)

val union : t -> t -> t

(*s [compl d] returns the complement domain. *)

val compl : t -> t

(*s [inter d1 d2] returns [d] iff the domain of [d] is the intersection of the
 domains of [d1] and [d2]. In case, this intersection is empty, the exception
 [Empty] is raised. *)

exception Empty

val inter : t -> t -> t

(*s [disjoint d1 d2] tests if the domains of [d1] and [d2] are disjoint. *)

val disjoint: t -> t -> bool

(* [sub d1 d2] tests if the domain of [d1] is a subdomain of [d2]. *)

val sub : t -> t -> bool

(*s [cmp d1 d2] returns [Sub] ([Equal], [Disjoint], [Super]) if the domain of [d1] 
 is a subset (is equal, is disjoint, is a superset) of [d2]. *)

val cmp : t -> t -> Binrel.t

(*s [of_q u] returns [Int] if the rational [u] is integer and [Nonint] 
 otherwise. *)

val of_q : Mpa.Q.t -> t

(*s Pretty-printing *)

val pp: Format.formatter -> t -> unit
