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


(** Symbol table 

  @author Harald Ruess
*)


type entry = 
  | Def of defn
  | Arity of int
  | Type of Var.Cnstrnt.t
  | State of Context.t

and defn = 
  | Term of Term.t
  | Prop of Prop.t

and t

val lookup : Name.t -> t -> entry
  (** [lookup n s] lookup value [e] if binding [n |-> e] is in the table [s];
    otherwise exception [Not_found] is raised. *)

val empty : unit -> t
  (** Empty symbol table. *)

val add : Name.t -> entry -> t -> t
  (** Adding a binding [n |-> e] to a symbol table. Throws [Invalid_argument],
    if [n] is already in the domain of the table. *)

val remove : Name.t -> t -> t
  (** Removing an entry [n |-> ...] from the symbol table. *)

val filter : (Name.t -> entry -> bool) -> t -> t
  (** [filter p s] builds a subtable of [s] with all bindings [n |-> e] satisfying 
    predicate [p n e]. *)


(** {6 Accessors} *)

val def : t -> t      
val arity : t -> t
val typ : t -> t
val state : t -> t


(** {6 Pretty-printing} *)

val pp : t Pretty.printer

val pp_entry : entry Pretty.printer
