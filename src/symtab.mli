
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
i*)


(*s Module [Symtab]: Symbol table functions. *)


type entry = 
  | Def of Term.t
  | Arity of int
  | Type of Cnstrnt.t
  | State of Shostak.t

and t

(*s [lookup n s] lookup value [e] if binding [n |-> e] is in the table [s];
  otherwise exception [Not_found] is raised. *)

val lookup : Name.t -> t -> entry

(*s Empty symbol table. *)

val empty : t

(*s Adding a binding [n |-> e] to a symbol table. Throws [Invalid_argument],
  if [n] is already in the domain of the table. *)

val add : Name.t -> entry -> t -> t

(*s Removing an entry [n |-> ...] from the symbol table. *)

val remove : Name.t -> t -> t

(*s [filter p s] builds a subtable of [s] with all bindings [n |-> e] satisfying 
 predicate [p n e]. *)

val filter : (Name.t -> entry -> bool) -> t -> t

(*s Projections. *)

val def : t -> t      
val arity : t -> t
val typ : t -> t
val state : t -> t

(*s Pretty printing of a symbol table. *)

val pp : t Pretty.printer

val pp_entry : entry Pretty.printer
