
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

open Sym


type norm = Sym.t -> Term.t list -> Term.t

let mk_abs a = 
  Term.App.mk_app Sym.Fun.abs [a]

let rec mk_apply sigma r a b =
  match a with
    | Term.App(Fun(Abs), [x]) -> 
	byValue sigma (subst sigma x b 0)
    | _ ->
	Term.App.mk_app (Sym.Fun.apply r) [a; b]


(** evaluation, not affecting function bodies *)
and eval sigma =
  Trace.func "eval" "Eval" Term.pp Term.pp
    (fun a -> 
       match a with
	 | Term.App(Fun(Apply(r)), [x; y]) ->
	     let x' = eval sigma x in
	       (match x' with
		  | Term.App(Fun(Abs), [z]) ->
		      eval sigma (subst sigma z (eval sigma y) 0)
		  | _ -> 
		      let y' = eval sigma y in
			if x' == x && y' == y then a else 
			  Term.App.mk_app (Sym.Fun.apply r) [x'; y'])
	 | _ ->
	     a)


(** normalization using call-by-value*)
and byValue sigma a = 
  let rec  bodies = function
    | Term.App(Fun(Abs), [x]) -> 
	Term.App.mk_app Sym.Fun.abs [byValue sigma x]
    | Term.App(Fun(Apply(r)), xl) -> 
	Term.App.mk_app (Sym.Fun.apply r) (Term.mapl bodies xl)
    | a -> 
	a
  in
    bodies (eval sigma a)


(* Head normal form. *)

and hnf sigma a =
  match a with
    | Term.App(Fun(Abs), [x]) ->
	let x' = hnf sigma x in
	  if x == x' then a else 
	    Term.App.mk_app Sym.Fun.abs [x']
    | Term.App(Fun(Apply(r)), [x1; x2]) ->
	(match hnf sigma x1 with
	   | Term.App(Fun(Abs), [y]) ->
	       hnf sigma (subst sigma y x2 0)
	   | y -> 
	       if y == x1 then a else 
		 Term.App.mk_app (Sym.Fun.apply r) [y; x2])
    | _ -> 
	a

(* Normalization using call-by-name. *)

and byName sigma a =
  let rec args a =
    match a with
      | Term.App(Fun(Abs), [x]) ->
	  let x' = args x in
	    if x == x' then a else 
	      Term.App.mk_app Sym.Fun.abs [x']
       | Term.App(Fun(Apply(r)), x :: xl) ->
	  let x' = args x 
	  and xl' = Term.mapl (byName sigma) xl in
	    if x == x' && xl == xl' then a else 
	      Term.App.mk_app (Sym.Fun.apply(r)) (x' :: xl')
      | _ -> 
	  a
  in
    args (hnf sigma a)

and subst sigma a s k =
  match a with
    | Term.Var(x) -> 
	if Var.is_free x then
          let i = Var.d_free x in
            if k < i then 
              Term.Var(Var.mk_free(i - 1))
            else if i = k then
              s
            else 
              Term.Var(Var.mk_free i)
	else 
	  a
    | Term.App(Fun(Abs), [x]) ->
        mk_abs (subst sigma x (lift s 0) (k + 1))
    | Term.App(f, xl) ->
	sigma f (substl sigma xl s k)

and substl sigma al s k =
  Term.mapl (fun x -> subst sigma x s k) al

and lift a k =
  match a with
    | Term.Var(x) ->
	if Var.is_free x then
	  let i = Var.d_free x in
	    if i < k then a else Term.Var(Var.mk_free(i + 1))
	else 
	  a
    | Term.App(Fun(Abs), [x]) ->
	mk_abs (lift x (k + 1))
    | Term.App(f, xl) ->
	Term.App.mk_app f (liftl xl k)

and liftl al k =
  Term.mapl (fun a -> lift a k) al

let sigma op al =
  match op, al with
    | Apply(r), [x; y] -> 
	mk_apply Term.App.mk_app r x y   (* no simplifications *)
    | Abs, [x] -> 
	mk_abs x
    | _ -> 
	assert false

let rec map f a =
  match a with
    | Term.App(Fun(Apply(r)), [x; y]) ->
	let x' = map f x and y' = map f y in
	  if x == x' && y == y' then a else
	    mk_apply Term.App.mk_app r x' y'
    | Term.App(Fun(Abs), [x]) ->
	let x' = map f x in
	  if x == x' then a else 
	    mk_abs x'
    | _ ->
	f a

(** Replacing a variable with a term. *)
let apply (x, b) = 
  map (fun y -> if Term.eq x y then b else y)
