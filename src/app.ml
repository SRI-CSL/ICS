
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

let sigma f l =
  match l with
    | [a] when Sym.eq f Sym.mk_unsigned -> (* Some builtin simplifications *)
	Builtin.mk_unsigned a              (* for unsigned interpretation. *)
 (*   | [a; b] when Sym.eq f Sym.mk_expt ->
	Arith.mk_expt a b *)
    | [a;b] when Sym.eq f Sym.mk_mult ->
	Arith.mk_mult a b
    | [a;b] when Sym.eq Sym.mk_select f ->
	Builtin.mk_select a b
    | [a;b;c] when Sym.eq Sym.mk_update f ->
	Builtin.mk_update a b c
    | [a] when Sym.eq Sym.mk_sin f ->
	Builtin.mk_sin a
    | [a] when Sym.eq Sym.mk_cos f ->
	Builtin.mk_cos a
    | _ ->
	Term.mk_app f l
	  
let is_uninterp a =
  not(Term.is_var a) &&
  Sym.is_uninterp (Term.sym_of a)

let d_uninterp a =
  assert(is_uninterp a);
  let f,l = Term.destruct a in
  (f, l)
