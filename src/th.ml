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

open Mpa

(** Classification of function symbols. *)

type t = int

let eq i j = (i = j)

let of_int i = i

let to_int i = i


let names = ["u"; "la"; "p"; "bv"; "cop"; "nl"; "app"; "arr"; "bva"]

let num_of_theories = List.length names

let u = 0

let la = 1
let p = 2
let bv = 3
let cop = 4

let pprod = 5
let app = 6
let arr = 7
let bvarith = 8

let to_string = List.nth names

exception Found of t

let of_string str =
  try
    let i = ref 0 in
      while !i < num_of_theories do
	if to_string !i = str then
	  raise(Found(!i));
	i := !i + 1
      done;
      raise(Invalid_argument str)
  with
      Found(i) -> i

let pp fmt th =
  Format.fprintf fmt "%s" (to_string th)

let is_fully_uninterp i = (i = u)

let is_fully_interp i = 
  (la <= i && i <= cop)

let of_sym = function
  | Sym.Uninterp _ -> u
  | Sym.Arith _ -> la
  | Sym.Product _ -> p
  | Sym.Bv _ -> bv
  | Sym.Coproduct _ -> cop
  | Sym.Arrays _ -> arr
  | Sym.Pp _ -> pprod
  | Sym.Fun _ -> app
  | Sym.Bvarith _ -> bvarith


module Array = struct

  type 'a arr = 'a array

  let create x = Array.create num_of_theories x

  let copy = Array.copy

  let get = Array.unsafe_get
  let set = Array.unsafe_set

  let reset a x =
    for i = 0 to num_of_theories - 1 do
      set a i x
    done

  let iter f a =
    for i = 0 to num_of_theories - 1 do
      f i (get a i)
    done

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right

  let to_list a = failwith "to do"
    

  let of_list = Array.of_list

  exception No

  let for_all p a =
    try
      for i = 0 to num_of_theories - 1 do
	if not (p (get a i)) then
	  raise No
      done;
      true
    with
	No -> false

  let for_all2 p a b = 
    try
      for i = 0 to num_of_theories - 1 do
	if not(p (get a i) (get b i)) then
	  raise No
      done;
      true
    with
	No -> false
	
  let pp p fmt a =
    Pretty.map pp p fmt (to_list a)

end

(** Theory-specific normalization. *)

let maps = 
  let a = Array.create (fun _ a -> a) in
    List.iter
      (fun (i, m) -> Array.set a i m)
      [u, App.map;
       la, Arith.map;
       p, Tuple.map;
       bv, Bitvector.map;
       cop, Coproduct.map;
       pprod, Pp.map;
       app, Apply.map;
       arr, Arr.map;
       bvarith, Bvarith.map];
    a

let map = Array.get maps


(** Theory-specific solver *)

let solvers = 
  let a = Array.create (fun e -> [e]) in
    List.iter
      (fun (i, m) -> Array.set a i m)
      [la, (fun e ->
	      match Arith.qsolve e with
		| None -> []
		| Some(e') -> [e']);
       p, Tuple.solve;
       bv, Bitvector.solve;
       cop, Coproduct.solve];
    a

let solve = Array.get solvers
