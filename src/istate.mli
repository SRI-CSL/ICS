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

(** Manipulating {b global} states.

  A command is a procedure for manipulating an ICS state consisting of a 
  logical context of type {!Context.t}, a symbol table {!Symtab.t}, and
  various other parameters and settings.

  This module maintains such a global state, which is also called the
  {i current} state, and provides functionality for destructively
  updating current states.

  @author Harald Ruess
*)


type t 
  (** Datatype for representing global states. *)


(** {6 Accessors} *)

val current : unit -> Context.t
  (** [current()] returns the {i current} logical context. *)

val symtab : unit -> Symtab.t
  (** [symtab()] returns the {i current} symbol table. *)

val eot : unit -> string
  (** [eot()] returns the {i current} end of transmission character. *)

val inchannel : unit -> in_channel
  (** [inchannel()] returns the {i current} input channel. *)

val outchannel : unit -> Format.formatter
  (** [outchannel()] returns the {i current} output channel. *)


(** {6 Context-dependent operations} *)

val get_context : Name.t option -> Context.t
  (** [get_context (Some(n))] returns the context associated with the
    name [n] in the current symbal table. [get_context None] is 
    {!Istate.current}[()]. *)

val ctxt_of : Name.t option -> Atom.Set.t

val diseq : Name.t option -> Term.t -> Term.Set.t
  (** Disequalities. *)
  
val cnstrnt : Name.t option -> Term.t -> Cnstrnt.t
  (** Constraint. *)


val solve : Th.t ->  (Term.t * Term.t) -> (Term.t * Term.t) list
  (** Solver. *)

val solution: Name.t option -> Th.t -> (Term.t * Term.t) list
  (** [solution n th] returns the {i solution set} associated
    with equality theory [th] in context {!Istate.get_context}[(n)]. *)

val find : Name.t option -> Th.t -> Term.t -> Term.t

val inv : Name.t option -> Th.t -> Term.t -> Term.t

val use : Name.t option -> Th.t -> Term.t -> Term.Set.t


(** {6 Controls} *)

val initialize: bool -> string -> in_channel -> Format.formatter -> unit
  (** [initialize pp eot inch outch] sets
    - [pretty()] to [pp]
    - [eot()] to [eot]
    - [inchannel()] to [inch]
    - [outchannel()] to [outch]. *)

val reset : unit -> unit
  (** Resetting the current imperative state to its initial value *)


(** {6 Settings} *)

val set_inchannel : in_channel -> unit
  (** [set_inchannel inch] sets the current input channel {!Istate.inchannel}[()]
    to [inch]. *)

val set_outchannel : Format.formatter -> unit
  (** [set_outchannel outch] sets the current input channel {!Istate.outchannel}[()]
    to [outch]. *)


(** {6 Channels} *)

val flush : unit -> unit
  (** Flushing the current output channel. *)

val nl : unit -> unit
  (** Printing a newline onto the current output channel. *)


(** {6 Symbol table commands} *)

val def : Name.t -> Symtab.defn -> unit
  (** [def n a] adds a {i term definition} of name [n] for term [a] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val sgn : Name.t -> int -> unit
  (** [sgn n i] adds a {i signature definition} [i] for name [n] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val typ : Name.t -> Cnstrnt.t -> unit
  (** [typ n i] adds a {i type definition} [n] for type [i] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val entry_of : Name.t -> Symtab.entry option
  (** [entry_of n] returns the entry for name [n] in the current symbol table. *)

val type_of : Name.t -> Cnstrnt.t option
  (** [type_of n] returns the corresponding type for the
    type definition [n] in the current symbol table. *)

val width_of : Term.t -> int option
  (** Getting the width of bitvector terms from the signature. *)


(** {6 Normalization} *)

(** Canonization w.r.t current state. *)

val can : Atom.t -> Atom.t
  (** [can a] normalizes atom [a] w.r.t the current logical context. *)

val cant : Term.t -> Term.t
  (** [cant a] canonizes term [a] w.r.t the current logical context. *)

val sigma : Sym.t -> Term.t list -> Term.t
  (** [sigma a] normalizes term [a] w.r.t the current logical context. *)

  
(** {6 Processing} *)

val process : Name.t option -> Atom.t -> Name.t Context.Status.t
  (** Adding a new fact *)


val valid : Name.t option -> Atom.t -> bool
  (** Checking for validity. *)

val unsat : Name.t option -> Atom.t -> bool
  (** Checking for unsatisfiablity. *)


(** {6 Context Manipulations} *)

val save : Name.t option -> Name.t

val restore : Name.t -> unit

val remove : Name.t -> unit

val forget : unit -> unit



(** {6 Predicates} *)

val is_equal : Term.t -> Term.t -> bool
  (** Equality/disequality test. *)

(** {6 Sat solver} *)

val sat : Prop.t -> Prop.Assignment.t option


(** {6 Suggested Splits} *)

val split : unit -> Atom.Set.t
  (** Splitting. *)

