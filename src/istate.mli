(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Global states.

  @author Harald Ruess

  A {i global state} consists of 
  - a {i current} logical context (see module {!Context}),
  - a {i symbol table} (see module {!Symtab}), and
  - values for the {i global parameters} (see module {!Parameters}).

  This module maintains such a global state, and provides 
  {i commands } for destructively updating a {i current} global state.
*)


module Inchannel : (Ref.REF with type t = Tools.Inchannel.t)
  (** Global reference for current input channel. Default [stdin]. *)

module Outchannel : (Ref.REF with type t = Tools.Outchannel.t)
  (** Global reference for current output channel. Default [Format.std_formatter]. *)

module Eot : Ref.STRING
  (** Global reference for {i end of transmission} marker. Defaults to empty string. *)

val batch : bool ref
val progress : bool ref


(** {6 Commands} *)

val do_cmp : Term.t * Term.t -> unit

val do_ctxt : Name.t option -> unit
  (** Return list of axioms for logical context of name [n]. *)

val do_status : Name.t option -> unit
  (** Return context status (see {!Context.status}) for logical context [n]. *)

val do_config : Name.t option -> unit
  (** Return inference system (see {!Context.config}) configuration 
    for logical context [n]. *)

val do_show : Name.t option * Theory.t option option -> unit

val do_resolve : Name.t option -> unit

val do_solve : (Term.t * Term.t) -> unit
  (** Solver. *)

val do_find : Name.t option * Theory.t option * Term.t -> unit

val do_inv : Name.t option * Theory.t * Term.t -> unit

val do_dep : Name.t option * Theory.t * Term.t -> unit

val do_reset : unit -> unit
  (** Resetting the current imperative state to its initial value *)

val do_symtab : Name.t option -> unit
  (** [do_symtab None] outputs symbol table, whereas [do_symtab Some(n)]
    outputs the entry (see {!Symtab.Entry}) corresponding to [n]. *)

type define = 
  | Term of Name.t list * Term.t
  | Prop of Name.t list * Prop.t
  | Spec of Spec.t
  | Context of Context.t

val do_define : Name.t * define -> unit
  (** [def n defn] adds a {i definition} of name [n] for the definition [defn]
    to symbol table.  Raises [Invalid_argument], if [n] is already a key 
    in the symbol table. *)


val do_sigma : Term.t -> unit
  (** [sigma a] normalizes term [a] w.r.t the current logical context. *)

val do_can : Name.t option * Term.t -> unit
  (** [can a] canonizes term [a] w.r.t the current logical context. *)

val do_eval : Name.t option * Atom.t -> unit

  
(** {6 Processing} *)

val do_process1 : Name.t option * Atom.t -> unit
  (** Adding a new fact *)

val do_process : Name.t option * Atom.t list -> unit


(** {6 Context Manipulations} *)

val do_save : Name.t option -> unit

val do_restore : Name.t -> unit

val do_remove : Name.t -> unit

val do_forget : unit -> unit

val do_undo : unit -> unit

val cmdBatch : (Tools.Inchannel.t -> int) ref

val do_load : Name.t option * string -> unit

val do_register : Spec.t -> unit

val do_sat :  Name.t option * Prop.t -> unit

val do_quit : int -> unit

val do_set : Name.t * string -> unit
  (** [do_set (n, v)] sets the global reference (see module {!Ref}) 
    of name [n] to the value corresponding to string [v].  If [n]
    is not a global reference or if [v] is not a valid value for this
    reference, then [Invalid_argument] is raised. *)

val do_get : Name.t option -> unit
  (** [do_get None] returns the values of all global references
    (see module {!Ref}), whereas [do_get Some(n)] only returns the
    value of the global reference [n].  If [n] is not a global reference,
    [Invalid_argument] is raised. *)


type help = 
  | All 
  | Command of string
  | Nonterminal of string

val do_help : help -> unit


