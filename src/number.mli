
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

(*s Module [Numtype]: number constraints. *)

(*i*)
open Mpa
open Hashcons
(*i*)

type t

val make : Dom.t * Intervals.t -> t

val destruct : t -> Dom.t * Intervals.t

val eq : t -> t -> bool

(*s Derived Constructors. *)

val mk_real : t
val mk_int : t
val mk_nat : t

val mk_singleton : Q.t -> t
val mk_diseq : Q.t -> t

(*s Recognizers and Accessors. *)

val is_bot : t -> bool

val is_real : t -> bool
val is_int : t -> bool

val d_singleton : t -> Q.t option

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

val compl: t -> t

(*s Pretty-printing constraints. *)

val pp : Format.formatter -> t -> unit

(*s Sets of types and maps with types as domain. *)

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)

(*s Additional constructors. *)

val mk_oo : Dom.t -> Q.t -> Q.t -> t
val mk_oc : Dom.t -> Q.t -> Q.t -> t
val mk_co : Dom.t -> Q.t -> Q.t -> t
val mk_cc : Dom.t -> Q.t -> Q.t -> t

val mk_lt : Dom.t -> Q.t -> t
val mk_le : Dom.t -> Q.t -> t
val mk_gt : Dom.t -> Q.t -> t
val mk_ge : Dom.t -> Q.t -> t

val mk_pos : Dom.t -> t
val mk_neg : Dom.t -> t
val mk_nonneg : Dom.t -> t
val mk_nonpos : Dom.t -> t


(*s Abstract interpretation. *)

val add : t -> t -> t
val addl : t list -> t
val mult : t -> t -> t
val expt : int -> t -> t
val multq : Q.t -> t -> t
val div : t -> t -> t
