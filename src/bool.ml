
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

(*i*)
open Hashcons
(*i*)

let is_interp a = 
  match Sym.destruct (Term.sym_of a) with
    | Sym.Interp(Sym.Bool(Sym.True | Sym.False)) -> true
    | _ -> false

let mk_tt = Term.mk_const Sym.mk_tt

let mk_ff = Term.mk_const Sym.mk_ff

let is_tt a = Sym.eq (Term.sym_of a) Sym.mk_tt

let is_ff a = Sym.eq (Term.sym_of a) Sym.mk_ff

let sigma f l =
  match f,l with
    | Sym.True, [] -> mk_tt
    | Sym.False, [] -> mk_ff
    | _ -> assert false
  



