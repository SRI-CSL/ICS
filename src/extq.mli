
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

(*s Module [Extq]: Arithmetic operations on the rationals extended with
 positive and negative infinity. *)

type extq =
  | Inject of Mpa.Q.t
  | Posinf
  | Neginf

and t

(*s Constructors. *)

val of_q : Mpa.Q.t -> t
val posinf : t
val neginf : t

(*s Destructor. *)

val destruct : t -> extq

(*s Is the extended real a real, and translating to a rational if possible. *)

val is_q: t -> bool
val to_q : t -> Mpa.Q.t option

val is_z: t -> bool
val to_z : t -> Mpa.Z.t option


(*s Test if argument is an integer. *)

val is_int : t -> bool

(*s Printing an extended rational. *)

val pp : Format.formatter -> t -> unit

(*s Derived constructors. *)

val zero : t
val is_zero : t -> bool

(*s Equality on extended reals. *)

val eq : t -> t -> bool

(*s Extended reals are ordered as among the reals, if both are real.
 Moreover, [-inf < c < +inf] for any real [c]. *)

val lt : t -> t -> bool
val le : t -> t -> bool

(*s Comparison. *)

val cmp : t -> t -> Mpa.Q.cmp

(*s Minimum and maximum. *)

val min : t -> t -> t
val max : t -> t -> t

(*s Sign computation. *)

val sign : t -> Sign.t

(*s Arithmetic operations. *)

val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t
val div : t -> t -> t
