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

(** {6 Global Variables} *)

module Parameters : sig

  type t = 
    | Compactify
    | Pretty
    | Statistics
    | Justifications
    | Inchannel
    | Outchannel
    | Eot
    | Prompt
    | IntegerSolve
    | Index
    | Clock
    | Diff

  val to_string : t -> string
  val of_string : string -> t

  val get : t -> string

  val set : t -> string -> unit

  val show : t Pretty.printer

  val iter : (t -> unit) -> unit

end 


(** {6 Accessors} *)

val current : Context.t ref
  (** [current()] returns the {i current} logical context. *)

val eot :  string ref
  (** [eot()] returns the {i current} end of transmission character. *)

val inchannel : in_channel ref
  (** [inchannel()] returns the {i current} input channel. *)

val outchannel : Format.formatter ref
  (** [outchannel()] returns the {i current} output channel. *)

val batch : bool ref

val prompt : string ref

val print_prompt : unit -> unit

val set_prompt : string -> unit
val set_eot : string -> unit

val lexing : unit -> Lexing.lexbuf

val prop_of : Name.t -> Prop.t

val entry_of : Name.t -> Symtab.entry

val type_of : Name.t -> Var.Cnstrnt.t
  (** [type_of n] returns the corresponding type for the
    type definition [n] in the current symbol table. *)

val width_of : Term.t -> int option
  (** Getting the width of bitvector terms from the signature. *)

val do_cmp : Term.t * Term.t -> unit

val progress : bool ref

val help_enabled : bool ref

type help = 
  | All 
  | Command of string
  | Nonterminal of string

val do_help : help -> unit

(** {6 Context-dependent operations} *)

val do_ctxt : Name.t option -> unit

val do_show : Name.t option -> Th.t option option -> unit

val do_show_vars : unit -> unit


val do_diseq : Name.t option * Term.t -> unit
  (** Disequalities. *)


val do_dom : Name.t option * Term.t -> unit
  (** Domain Constraint. *)

val do_sup: Name.t option * Term.t -> unit
  (** Domain Constraint. *)

val do_inf : Name.t option * Term.t -> unit
  (** Domain Constraint. *)

val do_split : Name.t option -> unit

val do_solve : Th.t *  (Term.t * Term.t) -> unit
  (** Solver. *)

val do_find : Name.t option * Th.t option * Term.t -> unit

val do_inv : Name.t option * Th.t * Term.t -> unit

val do_dep : Name.t option * Th.t * Term.t -> unit


(** {6 Controls} *)

val initialize: bool -> string -> in_channel -> Format.formatter -> unit
  (** [initialize pp eot inch outch] sets
    - [pretty()] to [pp]
    - [eot()] to [eot]
    - [inchannel()] to [inch]
    - [outchannel()] to [outch]. *)

val do_reset : unit -> unit
  (** Resetting the current imperative state to its initial value *)

val do_trace : Trace.level list -> unit

val do_untrace : Trace.level list option -> unit


(** {6 Settings} *)

val do_set_inchannel : in_channel -> unit
  (** [set_inchannel inch] sets the current input channel {!Istate.inchannel}[()]
    to [inch]. *)

val do_set_outchannel : Format.formatter -> unit
  (** [set_outchannel outch] sets the current input channel {!Istate.outchannel}[()]
    to [outch]. *)


(** {6 Symbol table commands} *)

val do_symtab : Name.t option -> unit

val do_def : Name.t * Symtab.defn -> unit
  (** [def n a] adds a {i term definition} of name [n] for term [a] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val do_prop: Name.t * Prop.t -> unit
  (** [def n p] adds a {i prop definition} of name [n] for proposition [p] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val do_sgn : Name.t * int -> unit
  (** [sgn n i] adds a {i signature definition} [i] for name [n] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)

val do_typ : Name.t list * Var.Cnstrnt.t -> unit
  (** [typ n i] adds a {i type definition} [n] for type [i] to the
    current symbol table. Raises [Invalid_argument], if [n] is already in 
    the domain of the table. *)



(** {6 Normalization} *)

(** Canonization w.r.t current state. *)


val do_sigma : Term.t -> unit
  (** [sigma a] normalizes term [a] w.r.t the current logical context. *)

val do_can : Term.t -> unit
  (** [can a] canonizes term [a] w.r.t the current logical context. *)

val do_simplify : Atom.t -> unit

  
(** {6 Processing} *)

val do_process1 : Name.t option * Atom.t -> unit
  (** Adding a new fact *)

val do_process : Name.t option * Atom.t list -> unit

val do_valid : Name.t option * Atom.t list -> unit
  (** Checking for validity. *)

val do_unsat : Name.t option * Atom.t list -> unit
  (** Checking for unsatisfiablity. *)

val do_model : Name.t option *  Term.t list -> unit
  (** Model construction. *)

val do_check_sat : Name.t option -> unit
  (** Model construction. *)

(** {6 Context Manipulations} *)

val do_save : Name.t option -> unit

val do_restore : Name.t -> unit

val do_remove : Name.t -> unit

val do_forget : unit -> unit

val do_undo : unit -> unit


(** {6 Sat solver} *)

val do_sat :  Name.t option * Prop.t -> unit

val do_error : string -> unit

val do_parse_error : int -> unit

val do_quit : int -> unit

(** Global variables *)

val do_set : Parameters.t * string -> unit
val do_get : Parameters.t -> unit







