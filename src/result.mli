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

(** Results of commands

  @author Harald Ruess
*)

type t = 
  | Term of Term.t
  | Atom of Atom.t
  | Cnstrnt of Cnstrnt.t
  | Optterm of Term.t option
  | Name of Name.t
  | Terms of Term.Set.t
  | Atoms of Atom.Set.t
  | Unit of unit
  | Bool of bool
  | Solution of (Term.t * Term.t) list
  | Context of Context.t
  | Process of Name.t Context.Status.t
  | Symtab of Symtab.t
  | Entry of Symtab.entry
  | Sat of Atom.Set.t option
  | Int of int
  | String of string

val output : Format.formatter -> t -> unit
