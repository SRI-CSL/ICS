
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

(*s Module [Endpoint]: datatype for endpoints of intervals. *)


type t = Extq.t * bool

(*s Constructor. *)

val make : Extq.t * bool -> t

(*s Accessors. *)

val destruct : t -> Extq.t * bool

val value : t -> Extq.t
val kind : t -> bool

(*s Test if endpoint is a rational or integer. *)

val is_q : t -> bool

val is_z : t -> bool

(*s Get value of a rational/integer endpoint. *)

val q_of : t -> Mpa.Q.t

val z_of : t -> Mpa.Z.t

(*s Strictness/Nonstrictness test. *)

val is_strict : t -> bool
val is_nonstrict : t -> bool


(*s Extreme endpoints *)

val neginf : t
val posinf : t

val strict : Mpa.Q.t -> t
val nonstrict : Mpa.Q.t -> t

val eq : t -> t -> bool
