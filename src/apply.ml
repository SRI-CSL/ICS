
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


let rec mk_apply sigma r a al =
  match a, al with
    | App(Fun(Abs), [x]), [y] -> 
	eval sigma (subst sigma x y 0)
    | _ ->
	mk_app (Fun(Apply(r))) (a :: al)

and eval sigma a =
  match a with
    | App(Fun(Apply(_)), [App(Fun(Abs), [x]); y]) -> 
	(subst sigma x y 0)
    | App(f, xl) ->
	sigma f (mapl (eval sigma) xl)
    | _ ->
	a

and subst sigma a s k =
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
        mk_abs (subst sigma x (lift s 0) (k + 1))
    | App(f, xl) ->
	sigma f (substl sigma xl s k)

and substl sigma al s k =
  mapl (fun x -> subst sigma x s k) al

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
    | Apply(r), x :: xl -> 
	mk_apply mk_app r x xl   (* no simplifications *)
    | Abs, [x] -> 
	mk_abs x
    | _ -> assert false
	

let rec map f a =
  match a with
    | App(Fun(Apply(r)), x :: xl) ->
	let x' = map f x in
	let xl' = mapl (map f) xl in
	  if x == x' && xl == xl' then a else
	    mk_apply mk_app r x' xl'
    | App(Fun(Abs), [x]) ->
	let x' = map f x in
	  if x == x' then a else 
	    mk_abs x'
    | _ ->
	f a
