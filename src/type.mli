
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

(*s Module [Type]: type constraints. *)

(*i*)
open Mpa
open Hashcons
(*i*)

type t = cnstrnt hashed

and cnstrnt = 
  | Bot
  | Number of Number.t
  | Enumeration of Name.Set.t     
  | Bitvector of int option
  | Top

val destruct : t -> cnstrnt

val eq : t -> t -> bool

(*s Constructors. *)

val mk_top : t
val mk_number : Number.t -> t
val mk_enumerative : Name.Set.t -> t
val mk_bitvector : int option -> t
val mk_bot : t


(*s Recognizers and Accessors. *)

val is_bot : t -> bool
val is_top : t -> bool

val d_bv : t -> int option

val d_number : t -> Number.t option

val d_enumerative : t -> Name.Set.t option

val d_singleton: t -> Q.t option

(*s Comparison of constraints. *)

val cmp : t -> t -> Binrel.t

val sub : t -> t -> bool

(*s Analyze. *)

val analyze : t -> Q.t Status.t

(*s Check if two constraints are disjoint. *)

val is_disjoint : t -> t -> bool

(*s Union and intersection of constraints. *)

val union : t -> t -> t
val inter : t -> t -> t


(*s Pretty-printing constraints. *)

val pp : Format.formatter -> t -> unit

(*s Sets of types and maps with types as domain. *)

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)


(*s Derived constructors. *)

val mk_real : t
val mk_int : t
val mk_nat : t
