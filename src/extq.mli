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

(** Arithmetic operations on the rationals extended with
  positive and negative infinity. 

  @author Harald Ruess
*)

type t
(** An extended rational is either an {b representation of a 
  rational number}, {b positive infinity}, or {b negative infinity} *)


(** {6 Constructors} *)

val of_q : Mpa.Q.t -> t
  (** [of_q q] construct an extended real for representing 
    the rational [q]. *)

val posinf : t
  (** The constant for representing positive infinity. *)
  
val neginf : t
  (** The constant for representing negative infinity. *)

val zero : t
  (** Constant for representing the number [0]. *)


(** {6 Destructor} *)

type extq =
  | Inject of Mpa.Q.t
  | Posinf
  | Neginf

val destruct : t -> extq
  (** Return a representation of type [extq] of an extended real.
    Injections of a rationals are returned as [Inject(q)], positive
    infinity as [Posinf], and negative infinity as [Neginf]. *)


(** {6 Recognizers} *)

val is_q: t -> bool
  (** [is_q x] holds iff [x] represents a rational number. *)

val is_z: t -> bool
  (** [is_z x] holds iff [x] represents an integer number. *)
  
val is_zero : t -> bool
  (** [is_zero x] holds iff [x] represents the number [0]. *)

(** {6 Transformers} *)

val to_q : t -> Mpa.Q.t option
  (** [to_q x] returns [Some(q)] if [x] represents the rational
    number [q], and [None] otherwise. *)

val to_z : t -> Mpa.Z.t option
  (** [to_z x] returns [Some(n)] if [x] represents the 
    number [n], and [None] otherwise. *)


(** {6 Pretty-printing} *)

val pp : t Pretty.printer
  (** Printing an extended rational. *)


(** {6 Relations} *)

val eq : t -> t -> bool
  (** Equality relation on the extended reals.  
    [eq x y] holds if
    - [x] and [y] represent rational numbers [q] and [p], respectively,
      and [q] is equal to [p].
    - [x] and [y] are either both positive infinity or negative infinity.
  *)

val lt : t -> t -> bool
  (** The 'less-than' relation. [lt x y] holds if
    - [x] and [y] represent rational numbers [q] and [p], respectively,
      and [q] is less than [p].
    - [x] is negative infinity and [y] is positive infinity. *)

val le : t -> t -> bool
  (** The 'less-or-equal' relation. [le x y] holds iff
    [lt x y] or [eq x y] holds. *)


val cmp : t -> t -> Mpa.Q.cmp
  (** - [cmp x y = Zero] if [eq x y] holds
      - [cmp x y = Less] if [lt x y] holds
      - [cmp x y = Greater] if [lt y x] holds. *)

val min : t -> t -> t
  (** [min x y] returns the minimum of [x] and [y], that is
    [x] if [le x y], and [y] otherwise. *)

val max : t -> t -> t
  (** [max x y] returns the maximum of [x] and [y], that is
    [x] if [le x y], and [y] otherwise. *)


val sign : t -> Sign.t
  (** Sign computation. 
    - [sign x = Neg] if [x] represents a negative rational or negative infinity
    - [sign x = Pos] if [x] represents a positive rational or positive infinity
    - [sign x = Zero] if [x] represents zero.
  *)


(** {6 Arithmetic operations} *)

exception Undefined

val add : t -> t -> t

val sub : t -> t -> t

val mult : t -> t -> t

val div : t -> t -> t
