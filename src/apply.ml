
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
open Sym
open Term
(*i*)

let mk_apply r a al =
  mk_app (Apply(Funapp(r))) (a :: al)
    
let mk_lambda i a =
  mk_app (Apply(Lambda(i))) [a]
    
let sigma op al =
  match op, al with
    | Funapp(r), x :: xl -> mk_apply r x xl
    | Lambda(i), [x] -> mk_lambda i x
    | _ -> assert false
	
let rec map f a =
  match a with
    | App(Apply(Funapp(r)), x :: xl) ->
	let x' = map f x in
	let xl' = mapl (map f) xl in
	  if x == x' && xl == xl' then a else
	    mk_apply r x' xl'
    | App(Apply(Lambda(i)), [x]) ->
	let x' = map f x in
	  if x == x' then a else 
	    mk_lambda i x'
    | _ ->
	f a
