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

(** Operations on functional arrays. *)

open Term

let d_interp = function
  | App(sym, al, _) -> (Sym.Array.get sym, al)
  | _ -> raise Not_found


let is_interp = function
  | App(sym, _, _) when Sym.Array.is sym -> true
  | _ -> false


let d_update a =
  match d_interp a with
    | Sym.Update, [b; i; x] -> (b, i, x)
    | _ -> raise Not_found


let d_select a =
  match d_interp a with
    | Sym.Select, [a; j] -> (a, j)
    | _ -> raise Not_found


let d_create a =
  match d_interp a with
    | Sym.Create, [a] -> a
    | _ -> raise Not_found


let d_select_update a =
  let (b, j) = d_select a in
  let (a, i, x) = d_update b in
    (a, i, x, j)


type equalRel = Term.t -> Term.t -> Three.t

(** Creating constant array. *)
let mk_create a =
  Term.App.mk_app Sym.Array.mk_create [a]


let select a i = 
  Term.App.mk_app Sym.Array.mk_select [a; i]

let update a i x = 
  Term.App.mk_app Sym.Array.mk_update  [a; i; x]


(** Simplifying constructor for selection terms. *)
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


(** Simplifying constructor for update terms. *)
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


(** Array canonizer. *)
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



module Flat = struct

  let is = function
    | Term.Var _ -> true
    | Term.App(f, al, _) ->
	Sym.Array.is f && List.for_all Term.is_var al

  let mk_create a =
    assert(Term.is_var a);
    mk_create a

  let mk_update a i x =
    assert(Term.is_var a);
    assert(Term.is_var i);
    assert(Term.is_var x);
    update a i x

  let mk_select a j =
    assert(Term.is_var a);
    assert(Term.is_var j);
    select a j
	
	
  (** Replace a variable [x] by [y] in a flat term [a]. *)
  let apply (x, y) a =
    assert(Term.Equal.is_var (x, y));
    assert(is a);
    let subst z = if Term.eq x z then y else z in
      try
	(match d_interp a with
	   | Sym.Create, [z] -> 
	       let z' = subst z in
		 if z == z' then z else mk_create z'
	   | Sym.Update, [z1; z2; z3] ->
	       let z1' = subst z1 and z2' = subst z2 and z3' = subst z3 in
		 if z1 == z1' && z2 == z2' && z3 == z3' then a else mk_update z1' z2' z3'
	   | Sym.Select, [z1; z2] ->
	       let z1' = subst z1 and z2' = subst z2 in
		 if z1 == z1' && z2 == z2' then a else mk_select z1' z2'
	   | _ -> 
	       subst a)
      with
	  Not_found -> subst a
	    
end
