
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
open Term
open Hashcons
(*i*)

let mk_uninterp x l =
  let f = Sym.mk_uninterp x in
  match f, l with
    | _, [a] when  Sym.eq f Sym.mk_unsigned -> (* Some builtin simplifications *)
	Builtin.mk_unsigned a                  (* for unsigned interpretation. *)
    | _ ->
	Term.mk_app (Sym.mk_uninterp(x)) l
	  
let is_uninterp a =
  Sym.is_uninterp (Term.sym_of a)

let d_uninterp a =
  assert(is_uninterp a);
  let f,l = Term.destruct a in
  (Sym.d_uninterp f, l)

let sigma x l = 
  mk_uninterp x l


let rec map f a =
  match Sym.destruct (Term.sym_of a) with 
    | Sym.Uninterp(g) -> 
	Term.mk_app (Sym.mk_uninterp(g)) (mapl (map f) (Term.args_of a))
    | _ ->
	(f a)

