
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


(*s Module [Shostak]: Maintaining the logical context of decision procedures. *)

(*s Abstraction. *)

val abstract_toplevel_term : Context.t -> Term.t -> Context.t * Term.t

val abstract : Context.t -> Atom.t -> Context.t * Atom.t

(*s Canonization. *)

val can_t : Context.t -> Term.t -> Term.t

val candiv : Context.t -> Term.t * Term.t -> Term.t
val canmult : Context.t -> Term.t * Term.t -> Term.tcd 
val canexpt : Context.t -> Term.t -> Term.t -> Term.t
val canupdate : Context.t -> Term.t * Term.t * Term.t -> Term.t
val canselect : Context.t -> Term.t * Term.t -> Term.t

val can : Context.t -> Atom.t -> Atom.t

(*s Test for equality. *)

val eq : Context.t -> Term.t -> Term.t -> bool

(*s Processing *)

type 'a status =
  | Valid
  | Inconsistent 
  | Satisfiable of 'a

val process: Context.t -> Atom.t -> Context.t status
