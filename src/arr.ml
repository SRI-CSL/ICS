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

open Sym
open Term

let create = Arrays(Create)
let update = Arrays(Update)
let select = Arrays(Select)

let mk_create a =
  mk_app create [a]

(** Reducing patterns of the form [select(update(a,i,x), j)]
  according to the equations
     [select(create(a), j) = a]
     [i = j => select(update(a,i,x), j) = x]
     [i <> j => select(update(a,i,x),j) = select(a,j)] 
 *)

let mk_select =
  let select = mk_app (Arrays(Select)) in
    fun is_equal b j ->
      match b with
	| App(Arrays(Create), [a]) ->
	    a
	| App(Arrays(Update), [a; i; x]) ->
	    (match is_equal i j with
	       | Three.Yes ->
		   x
	       | Three.No -> 
		   select [a; j]
	       | Three.X ->
		   select [b; j])
	| _ ->
	    select [b; j]

let mk_update = 
  let update = mk_app (Arrays(Update)) in
    fun is_equal a j y ->
      match a with
	| App(Arrays(Update), [b; i; x]) when Term.eq i j ->
	    update [b; i; y]
	| _ ->
	    update [a; j; y]

let sigma is_equal op l =
  match op, l with
    | Create, [a] ->
	mk_create a
    | Update, [a; i; x] ->
	mk_update is_equal a i x
    | Select, [a; j] ->
	mk_select is_equal a j
    | _ -> 
	assert false

let rec map is_equal f b =
  match b with
    | App(Arrays(Create), [a]) -> 
	let a' = map is_equal f a in
	  if a == a' then b else
	    mk_create a'
    | App(Arrays(Update), [a; i; x]) ->
	let a' = map is_equal f a and i' = map is_equal f i and x' = map is_equal f x in
	  if a == a' && i == i' && x == x' then b else
	    mk_update is_equal a' i' x'
    | App(Arrays(Select), [a; j]) ->
	let a' = map is_equal f a and j' = map is_equal f j in
	  if a == a' && j == j' then b else
	    mk_select is_equal a' j'
    | _ -> 
	f b
