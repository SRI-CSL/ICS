
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

let mk_abs a =
  mk_app (Fun(Abs)) [a]


let rec mk_apply r a al =
  match a, al with
    | App(Fun(Abs), [x]), [y] -> 
	subst x y 0
    | _ ->
	mk_app (Fun(Apply(r))) (a :: al)
 

and subst a s k =
  match a with
    | Var(x) -> 
	if Var.is_free x then
          let i = Var.d_free x in
            if k < i then 
              Var(Var.mk_free(i-1))
            else if i = k then
              s
            else 
              Var(Var.mk_free i)
	else 
	  a
    | App(Fun(Abs), [x]) ->
        mk_abs (subst x (lift s 0) (k + 1))
    | App(Arith(op), xl) ->
	Arith.sigma op (substl xl s k)
(*    | App(Pp(Mult), xl) ->
	Sig.mk_multl (substl xl s k) *)
    | App(f, xl) ->
	mk_app f (substl xl s k)

and substl al s k =
  mapl (fun x -> subst x s k) al

and lift a k =
  match a with
    | Var(x) ->
	if Var.is_free x then
	  let i = Var.d_free x in
	    if i < k then a else Var(Var.mk_free(i + 1))
	else 
	  a
    | App(Fun(Abs), [x]) ->
	mk_abs (lift x (k + 1))
    | App(f, xl) ->
	mk_app f (liftl xl k)

and liftl al k =
  mapl (fun a -> lift a k) al


let sigma op al =
  match op, al with
    | Apply(r), x :: xl -> mk_apply r x xl
    | Abs, [x] -> mk_abs x
    | _ -> assert false
	
let rec map f a =
  match a with
    | App(Fun(Apply(r)), x :: xl) ->
	let x' = map f x in
	let xl' = mapl (map f) xl in
	  if x == x' && xl == xl' then a else
	    mk_apply r x' xl'
    | App(Fun(Abs), [x]) ->
	let x' = map f x in
	  if x == x' then a else 
	    mk_abs x'
    | _ ->
	f a
