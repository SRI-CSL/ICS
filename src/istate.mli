
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
val typ : Name.t -> Cnstrnt.t -> unit

(*s Symbol table entry. *)

val entry_of : Name.t -> Symtab.entry option
 
(*s Type from symbol table. *)

val type_of : Name.t -> Cnstrnt.t option


(*s Getting the width of bitvector terms from the signature. *)

val width_of : Term.t -> int option


(*s State-dependent destructors. *)

(*s Context of. *)

val ctxt_of : unit -> Atom.t list

(*s Canonization w.r.t current state. *)

val can : Atom.t -> Atom.t

val cant : Term.t -> Term.t

val sigma : Sym.t -> Term.t list -> Term.t

(*s Adding a new fact *)

val process : Atom.t -> Shostak.t Shostak.status

(*s Compress the current state. *)

val compress : unit -> unit

 
(*s Change current state. *)

val save : Name.t -> unit
val restore : Name.t -> unit
val remove : Name.t -> unit
val forget : unit -> unit


(*s Applying maps. *)

val find : Theories.t -> Term.t -> Term.t
val inv : Theories.t -> Term.t -> Term.t
val use : Theories.t -> Term.t -> Term.Set.t

(*s Solution set for equality theories. *)

val solution: Theories.t -> (Term.t * Term.t) list

(*s Variable partitioning. *)

val partition: unit -> (Term.t * Term.t) list

(*s Disequalities. *)

val diseq : Term.t -> Term.t list

(*s Constraint. *)

val cnstrnt : Term.t -> Cnstrnt.t option

(*s Equality/disequality test. *)

val is_equal : Term.t -> Term.t -> bool
val is_diseq : Term.t -> Term.t -> bool

val tests : unit -> Builtin.tests
