
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2002 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

type t 

val current : unit -> Dp.t
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
val sgn : Name.t -> Arity.t -> unit
val typ : Name.t -> Type.t -> unit

(*s State-dependent destructors. *)

val funsym_of : int -> Name.t -> Sym.uninterp
val enumtype_of : Name.t -> Name.Set.t
val type_of : Name.t -> Type.t
val constant_of : Name.t -> Term.t

(*s Context of. *)

val ctxt_of : unit -> Atom.Set.t

(*s Canonization w.r.t current state. *)

val can : Term.t -> Term.t
val canatom : Atom.t -> Atom.t

(*s Adding a new fact *)

val process : Prop.t -> Dp.t Process.status

(*s Extension of an equivalence class. *)

val ext : Term.t -> Term.set
 
(*s Change current state. *)

val save : Name.t -> unit
val restore : Name.t -> unit
val remove : Name.t -> unit
val forget : unit -> unit

val sub : Name.t -> Name.t -> Three.t

(*s Theory-specific operations. *)

val use_of : Theory.t -> Term.set Term.map
val use : Theory.t -> Term.t -> Term.set

val find_of : Theory.t -> Term.t Term.map
val find : Theory.t -> Term.t -> Term.t

val diseq_of: unit -> Term.set Term.map
val diseq : Term.t -> Term.set

val cnstrnt_of : unit -> Number.t Term.map
val cnstrnt : Term.t -> Type.t

val prop_of : unit -> Prop.t

val solve : Theory.t -> Term.t * Term.t -> (Term.t * Term.t) list option

(*s Toggle variables. *)

type toggle = 
  | Printall

val toggle : toggle ->  unit

