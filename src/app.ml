
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

open Term

let sigma f l =
  Term.App.mk_app f l

let lazy_sigma a f l =
  assert(not(is_var a) && Sym.eq (Term.App.sym_of a) f);
  let m = Term.App.args_of a in
  if try List.for_all2 eq l m with Invalid_argument _ -> false then
    a
  else 
    sigma f l
	  
let is_uninterp = function
  | App(Sym.Uninterp _, _) -> true
  | _ -> false
 
let d_uninterp a =
  assert(is_uninterp a);
  let f,l = Term.App.destruct a in
    (f, l)

let map ctxt a =
  match a with
    | Var _ -> ctxt(a)
    | App(f, l) ->  
	let l' = Term.mapl ctxt l in
	  if l == l' then a else 
	    Term.App.mk_app f l'
