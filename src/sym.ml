
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
open Mpa
(*i*)

(*s Interpreted symbols. *)

type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

type tuple = 
  | Product 
  | Proj of int * int

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

(*s Symbols. *)

type t = 
  | Uninterp of Name.t
  | Arith of arith
  | Tuple of tuple
  | Bv of bv


(*s Comparison. *)

let cmp = Pervasives.compare

(*s Equality. *)

let rec eq s t =
  match s, t with  
    | Uninterp(x), Uninterp(y) ->
	Name.eq x y
    | Arith(op1), Arith(op2) ->
	(match op1, op2 with
	   | Num(q1), Num(q2) -> Q.equal q1 q2
	   | Multq(q1), Multq(q2)  -> Q.equal q1 q2
	   | Add, Add -> true
	   | _ -> false)
    | Tuple(op1), Tuple(op2) ->
	(match op1, op2 with
	   | Product, Product -> true 
	   | Proj(i,n), Proj(j,m) -> i = j && n = m
	   | _ -> false)
    | Bv(op1), Bv(op2) ->
	(match op1, op2 with
	   | Const(b1), Const(b2) -> 
	       Pervasives.compare b1 b2 = 0
	   | Conc(n1,m1), Conc(n2,m2) ->
	       n1 = n2 && m1 = m2
	   | Sub(n1,i1,j1), Sub(n2,i2,j2) ->
	       n1 = n2 && i1 = i2 && j1 = j2
	   | Bitwise(n1), Bitwise n2 ->
	       n1 = n2
	   | _ -> false)
    | _ ->
	false


let pp fmt s = 
  let rec sym s =
    match s with 
      | Uninterp(f) -> Name.pp fmt f
      | Arith(op) -> arith op
      | Tuple(op) -> tuple op
      | Bv(op) -> bv op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q 
      | Add -> Format.fprintf fmt "+"
      | Multq(q) ->  Mpa.Q.pp fmt q; Format.fprintf fmt "*" 

  and tuple op =
    match op with
      | Product -> Format.fprintf fmt "tuple"
      | Proj(i,n) -> Format.fprintf fmt "proj[%d,%d]" i n

  and bv op =
    match op with
      | Const(b) -> Format.fprintf fmt "0b%s" (Bitv.to_string b)
      | Conc(n,m) -> Format.fprintf fmt "conc[%d,%d]" n m
      | Sub(n,i,j) -> Format.fprintf fmt "sub[%d,%d,%d]" n i j
      | Bitwise(n) -> Format.fprintf fmt "bitwise[%d]" n
  in
  sym s



(*s Width of bitvector function symbols. *)

let rec width f =
  match f with
    | Bv(b) -> Some(width_bv b)
    | _ -> None

and width_bv b =
  match b with
    | Const(c) ->
	Bitv.length c
    | Sub(n,i,j) ->
	assert(0 <= i && i <= j && j < n);
	j-i+1
    | Conc(n,m) -> 
	assert(0 <= n && 0 <= m);
	n + m
    | Bitwise(n) ->
	assert(n >= 0);
	n

(*s Classification of function symbols. *)


type theories = 
  | U
  | T
  | BV
  | A

let theory_of = function
  | Arith _ -> A
  | Tuple _ -> T
  | Bv _ -> BV
  | Uninterp _ -> U

let name_of_theory = function
  | U -> "u"
  | A -> "a"
  | BV -> "bv"
  | T -> "t"

let theory_of_name = function
  | "u" -> U
  | "a" -> A
  | "t" -> T
  | "bv" -> BV
  | x -> raise (Invalid_argument x)

let interp_theory_of_name = function
  | "a" -> A
  | "t" -> T
  | "bv" -> BV
  | x -> raise (Invalid_argument x)


(*s Some predefined function symbols. *)


let select = Uninterp(Name.select)
let update = Uninterp(Name.update)
let unsigned = Uninterp(Name.unsigned)
let floor = Uninterp(Name.floor)
let ceiling = Uninterp(Name.ceiling)
let mult = Uninterp(Name.mult)
let expt = Uninterp(Name.expt)
let div = Uninterp(Name.div)
let sin = Uninterp(Name.sin)
let cos = Uninterp(Name.cos)

