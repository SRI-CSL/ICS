(*
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
 *)

(** Various subdomains of numbers. The symbol [Real] {b denotes} the 
  set of real numbers, [Int] all integers, and [Nonint] all 
  non-integer reals.

  @author Ritvik Sahajpa
  @author Harald Ruess
*)

type t = Int | Nonint | Real 


(** {6 Relations} *)

val eq : t -> t -> bool
  (** [eq d e] holds iff the denotation of [d] equals the
    denotation of [e]. *)

val sub : t -> t -> bool
  (** [sub d e] holds iff the denotation of [d] is a subset of the
    denotation of [e]. *)

val disjoint : t -> t -> bool
  (** [disjoint d e] holds iff the denotations of [d] and [e] 
    are disjoint. *)

val cmp : t -> t -> (unit, Mpa.Q.t) Binrel.t
  (** [cmp d e] returns 
    - [Sub] if [sub d e] holds and [eq d e] does not hold
    - [Equal] if [eq d e] holds
    - [Super] if [sub e d] holds and [eq d e] does not hold
    - [Disjoint] if the denotation of [d] and [e] are disjoint *)


(** {6 Connectives} *)

val union : t -> t -> t
  (** [union d1 d2] returns [d] iff the denotation
    [d] is the union of the denotations of [d1] and [d2]. *)


val inter : t -> t -> t
 (** [inter d1 d2] returns [d] iff the denotation of [d] is the 
   nonempty intersection of the denotations of [d1] and [d2].
   If this intersection is empty, then {!Dom.Empty} is raised. *)

exception Empty


(** {6 Rationals} *)

val of_q : Mpa.Q.t -> t
  (** [of_q q] returns [Int] if the rational [q] is 
    an integer and [Nonint] otherwise. *)

val mem : Mpa.Q.t -> t -> bool
  (** [mem q d] tests if the rational [q] is an element of
    the denotation of [d]. *)


(** {6 Pretty-printing} *)

val pp: t Pretty.printer
