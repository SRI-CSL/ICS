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

(** Symbol table 

  @author Harald Ruess

  This module provides functionality for manipulating a global symbol 
  table with bindings [n |-> e] where {i key} [n] is a {Name.t} and [e]
  is either 
  - a {i definitional entry}, 
  - a {i context} of type {!Context.t}, or 
  - a {i theory specification} of type {!Spec.t}
*)

(** Keys of symbol table are names. *)
type key = Name.t

val reset : unit -> unit
  (** Clear the current symbol table. *)

val pp : Format.formatter -> unit
  (** [pp fmt] prints the current symbol table. *)

module Entry : sig
  type t
  val pp : Format.formatter -> t -> unit
  (** [pp fmt e] prints entry [e]. *)
end

val lookup : key -> Entry.t
  (** [lookup n] returns [e] if [n |-> e] in symbol table. 
    Otherwise, [Not_found] is raised. *)

val in_dom : Name.t -> bool
  (** [in_dom n] holds iff there is a binding [n |-> .] in the current symbol table. *)

(** Accessing symbol table. *)
module Get : sig

  val term : key -> int -> Name.t list * Term.t
    (** if key [n] is associated with a {i definitional entry}  [(xl, a)] for a term [a],
      and if the length of the argument list [xl] is [len], then
      [termDef n len] returns this entry [(xl, a)]; otherwise [Not_found] is raised. *)
 
  val prop : key -> int -> Name.t list * Prop.t
    (** if key [n] is associated with a {i definitional entry} for a propositional
      formula, then [propDef n len] returns this entry [(xl, p)], where [xl] is the 
      list of arguments and proposition [p] is the body; otherwise [Not_found] is raised. *)
    
  val spec : key -> Spec.t
    (** if key [n] is associated with a {i theory specification}, then [specDef n]
      returns this specification; otherwise [Not_found] is raised. *)

  val context : key -> Context.t
    (** if key [n] is associated with a {i context}, then [context n]
      returns this specification; otherwise [Not_found] is raised. *)

end

(** Extending symbol table. *)
module Put : sig

  val term : key -> Name.t list -> Term.t  -> unit
    (** [term n xl a] adds a term definition [(xl, a)] for key [n]. *)

  val prop : key -> Name.t list -> Prop.t  -> unit
    (** [prop n xl p] adds a propositional definition [(xl, p)] for key [n]. *)

  val spec : key -> Spec.t -> unit
    (** [spec n xl p] adds a propositional definition [(xl, p)] for key [n]. *)
  
  val context : key -> Context.t -> unit
    (** [context n ctxt] adds a binding [n |-> ctxt] to the symbol table,
      or throws [Invalid_argument] if [in_dom n]. *)

end

val restrict : key -> unit
  (** Removing an entry [n |-> ...] from the symbol table. *)
