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
 * 
 * Author: Harald Ruess
 *)

type t = 
  | Term of Term.t
  | Atom of Atom.t
  | Cnstrnt of Sign.t option
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
  | Sat of (Prop.Assignment.t * Name.t) option
  | Int of int
  | String of string


exception Result of t

let output fmt = function
  | Term(t) -> Term.pp fmt t
  | Atom(a) -> Atom.pp fmt a
  | Cnstrnt(Some(c)) -> Sign.pp fmt c
  | Cnstrnt(None) -> Format.fprintf fmt "None"
  | Optterm(Some(t)) -> Term.pp fmt t
  | Optterm(None) -> Format.fprintf fmt "None"
  | Name(n) -> Name.pp fmt n
  | Terms(ts) -> Pretty.set Term.pp fmt (Term.Set.elements ts)
  | Atoms(al) -> Pretty.set Atom.pp fmt (Atom.Set.elements al)
  | Unit() -> Format.fprintf fmt ""
  | Bool(x) -> Format.fprintf fmt "%s" (if x then "true" else "false")
  | Solution(sl) -> Pretty.list (Pretty.eqn Term.pp) fmt sl
  | Context(c) -> Context.pp fmt c
  | Process(status) -> Context.Status.pp Name.pp fmt status
  | Sat(None) -> Pretty.string fmt ":unsat"
  | Sat(Some(rho, n)) ->
      Pretty.string fmt ":sat "; Prop.Assignment.pp fmt rho;
  | Symtab(sym) -> Symtab.pp fmt sym
  | Entry(e) -> Symtab.pp_entry fmt e
  | Int(i) -> Format.fprintf fmt "%d" i
  | String(s) -> Format.fprintf fmt "%s" s






