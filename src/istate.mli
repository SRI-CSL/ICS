
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

type t 

val current : unit -> Shostak.t
val symtab : unit -> Symtab.t
val inchannel : unit -> in_channel
val outchannel : unit -> Format.formatter

(*s Resetting the state. *)

val reset : unit -> unit

(*s Setting input and output channels. *)

val set_inchannel : in_channel -> unit
val set_outchannel : Format.formatter -> unit

val flush : unit -> unit
val nl : unit -> unit

(*s Adding definitions to state *)

val def : Name.t -> Term.t -> unit
val sgn : Name.t -> int -> unit
val typ : Name.t -> Number.t -> unit

(*s State-dependent destructors. *)

(*s Context of. *)

val ctxt_of : unit -> Atom.Set.t

(*s Canonization w.r.t current state. *)

val can_t : Term.t -> Term.t
val can_a : Atom.t -> Atom.t

(*s Adding a new fact *)

val process_a : Atom.t -> Shostak.t Shostak.status

val process_p :Prop.t -> Shostak.t Shostak.status

(*s Compress the current state. *)

val compress : unit -> unit

 
(*s Change current state. *)

val save : Name.t -> unit
val restore : Name.t -> unit
val remove : Name.t -> unit
val forget : unit -> unit

(*s Accessors. *)

val u_of : unit -> Term.t Term.Map.t
val v_of : unit -> Term.t Term.Map.t
val a_of : unit -> Term.t Term.Map.t
val t_of : unit -> Term.t Term.Map.t
val bv_of : unit -> Term.t Term.Map.t

(*s Applying maps. *)

val find : Shostak.e -> Term.t -> Term.t
val inv : Shostak.e -> Term.t -> Term.t
val use : Shostak.e -> Term.t -> Term.Set.t

(*s Solution set for equality theories. *)

val solution: Shostak.e -> (Term.t * Term.t) list

(*s Variable partitioning. *)

val partition: unit -> Term.t Term.Map.t


(*s Theory-specific operations. *)

val diseq_of: unit -> Term.Set.t Term.Map.t
val diseq : Term.t -> Term.Set.t

val cnstrnt_of : unit -> Number.t Term.Map.t
val cnstrnt : Term.t -> Number.t option

val prop_of : unit -> Prop.t

(*s Toggle variables. *)

type toggle = 
  | Printall

val toggle : toggle ->  unit

