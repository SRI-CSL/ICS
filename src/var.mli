
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

(*s Module [Var]: datatype for variables. *)

type t

val name_of : t -> Name.t

val eq : t -> t -> bool

val cmp : t -> t -> int

val (<<<) : t -> t -> bool

(*s Constructors. *)

val mk_var : Name.t -> t
val mk_fresh : Name.t -> int option -> t

(*s Recognizers. *)

val is_var : t -> bool
val is_fresh : t -> bool

(*s Printing variables. *)

val pp : Format.formatter -> t -> unit

(*s Sets and maps of terms. *)

type var = t

module Set : (Set.S with type elt = var)

module Map : (Map.S with type key = var)
