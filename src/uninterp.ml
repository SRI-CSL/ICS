
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

let mk_uninterp (x,sgn) l =
  let f = Sym.mk_uninterp (x,sgn) in
  match f, l with
    | _, [a] when  f === Sym.mk_unsigned ->       (* Some builtin simplifications *)
	Builtin.mk_unsigned a                     (* for unsigned interpretation. *)
    | _ ->
	Term.make(Sym.mk_uninterp(x,sgn), l)
	  
let is_uninterp a =
  Sym.is_uninterp (Term.sym_of a)

let d_uninterp a =
  assert(is_uninterp a);
  match a.node with
    | App(f,l) -> (Sym.d_uninterp f, l)

let sigma (x,sgn) l = mk_uninterp (x,sgn) l


let rec map f a =
  match a.node with 
    | App({node=Sym.Uninterp(g,sgn)}, l) -> 
	Term.make(Sym.mk_uninterp(g,sgn), mapl (map f) l)
    | _ ->
	(f a)

