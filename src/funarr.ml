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

let d_interp = function
  | App(sym, al, _) -> (Sym.Array.get sym, al)
  | _ -> raise Not_found

let d_update a =
  match d_interp a with
    | Sym.Update, [b; i; x] -> (b, i, x)
    | _ -> raise Not_found

let d_select a =
  match d_interp a with
    | Sym.Select, [a; j] -> (a, j)
    | _ -> raise Not_found


let d_select_update a =
  let (b, j) = d_select a in
  let (a, i, x) = d_update b in
    (a, i, x, j)


type equalRel = Term.t -> Term.t -> Three.t

let mk_create a =
  Term.App.mk_app Sym.Array.mk_create [a]

(** Reducing patterns of the form [select(update(a,i,x), j)]
  according to the equations
     [select(create(a), j) = a]
     [i = j => select(update(a,i,x), j) = x]
     [i <> j => select(update(a,i,x),j) = select(a,j)] 
 *)

let select a i = 
  Term.App.mk_app Sym.Array.mk_select [a; i]

let mk_select is_equal b j =
  try
    (match d_interp b with
       | Sym.Create, [a] -> a
       | Sym.Update, [a; i; x] ->
	   (match is_equal i j with
	      | Three.Yes -> x
	      | Three.No -> select a j
	      | Three.X -> select b j)
       | _ -> 
	   select  b j)
  with
     Not_found -> select b j

let update a i x = 
  Term.App.mk_app Sym.Array.mk_update  [a; i; x]

let rec mk_update is_equal a j y =
  try
    let (b, i, x) = d_update a in
      (match is_equal i j with
	 | Three.Yes -> 
	     update b i y
	 | Three.No when Term.cmp i j > 0 ->
	     mk_update is_equal (mk_update is_equal b j y) i x
	 | _ -> 
	     update a j y)
  with
      Not_found -> update a j y

let sigma is_equal op l =
  match op, l with
    | Sym.Create, [a] -> mk_create a
    | Sym.Update, [a; i; x] ->mk_update is_equal a i x
    | Sym.Select, [a; j] -> mk_select is_equal a j
    | _ -> assert false

let rec map is_equal f b =
  try
    (match d_interp b with
       | Sym.Create, [a] -> 
	   let a' = map is_equal f a in
	     if a == a' then b else
	       mk_create a'
       | Sym.Update, [a; i; x] ->
	   let a' = map is_equal f a 
	   and i' = map is_equal f i 
	   and x' = map is_equal f x in
	     if a == a' && i == i' && x == x' then b else
	       mk_update is_equal a' i' x'
       | Sym.Select, [a; j] ->
	   let a' = map is_equal f a 
	   and j' = map is_equal f j in
	     if a == a' && j == j' then b else
	       mk_select is_equal a' j'
       | _ -> 
	   f b)
  with
      Not_found -> f b

let rec splits a =
  try
    (match d_interp a with
      | Sym.Create, [b] -> splits a
      | Sym.Update, [a; i; x] -> 
	  Term.Set2.union 
	    (splits a) 
	    (Term.Set2.union (splits i) (splits x))
      | Sym.Select, [a; j] -> 
	  let splts = Term.Set2.union (splits a) (splits j) in
	    (try
	      let (_, i, _) = d_update a in
	      let (i, j) = Term.orient (i, j) in
		Term.Set2.add (i, j) splts
	    with
		Not_found -> splts)
      | _ ->
	  Term.Set2.empty)
  with
      Not_found -> Term.Set2.empty
