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

(** Various subdomains of numbers.

  @author Harald Ruess
*)

type t = Int | Real | Nonint
 (** The symbol 
   - [Real] {b denotes} the set of real numbers, 
   - [Int] all integers, 
   - and [Nonint] all reals which are not integer. *)


val eq : t -> t -> bool
  (** [eq d e] holds iff the denotation of [d] equals the
    denotation of [e]. *)


val cmp : t -> t -> int
  (** [cmp d e] is 
    - [0] if [eq d e] holds
    - [-1] if [sub d e] holds
    - [1] if [sub e d] holds.
    Otherwise the result is unspecified. *)


val sub : t -> t -> bool
  (** [sub d e] holds iff the denotation of [d] is a subset of the
    denotation of [e]. *)


val disjoint : t -> t -> bool
  (** [disjoint d e] holds iff the denotations of [d] and [e] 
    are disjoint. *)


val union : t -> t -> t
  (** [union d1 d2] returns [d] iff the denotation
    [d] is the union of the denotations of [d1] and [d2]. *)


exception Empty

val inter : t -> t -> t
 (** [inter d1 d2] returns [d] iff the denotation of [d] is the 
   nonempty intersection of the denotations of [d1] and [d2]. *)

val inj : t -> t option

val multq : Mpa.Q.t -> t -> t

val add : t -> t -> t

val addl : t list -> t

val expt : int -> t -> t

val mult : t -> t -> t

val multl : t list -> t


val of_q : Mpa.Q.t -> t
  (** [of_q q] returns [Int] if the rational [q] is 
    an integer, and [Real] otherwise. *)


val mem : Mpa.Q.t -> t -> bool
  (** [mem q d] tests if the rational [q] is an element of
    the denotation of [d]. *)


val pp: t Pretty.printer


val to_string : t -> string
