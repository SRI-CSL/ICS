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

open Mpa

type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Multq of Mpa.Q.t

type product = 
  | Tuple
  | Proj of int * int

type coproduct = InL | InR | OutL | OutR

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type pprod = 
  | Mult
  | Expt of int

type apply = 
  | Apply of Dom.t option
  | Abs

type arrays = 
  | Create
  | Select 
  | Update

type bvarith = 
  | Unsigned


type t = 
  | Uninterp of Name.t
  | Arith of arith
  | Product of product
  | Coproduct of coproduct
  | Bv of bv
  | Pp of pprod
  | Fun of apply
  | Arrays of arrays  
  | Bvarith of bvarith 


let cmp = Pervasives.compare

let hash = function
  | Uninterp(x) -> (2 + Hashtbl.hash x) land 0x3FFFFFFF
  | Arith(f) -> (3 + Hashtbl.hash f) land 0x3FFFFFFF
  | Product(f) -> (5 + Hashtbl.hash f) land 0x3FFFFFFF
  | Coproduct(f) -> (7 + Hashtbl.hash f) land 0x3FFFFFFF
  | Bv(f) -> (11 + Hashtbl.hash f) land 0x3FFFFFFF
  | Pp(f) -> (17 + Hashtbl.hash f) land 0x3FFFFFFF
  | Fun(f) -> (23 + Hashtbl.hash f) land 0x3FFFFFFF
  | Bvarith(f) -> (29 + Hashtbl.hash f) land 0x3FFFFFFF
  | Arrays(f) ->(31 + Hashtbl.hash f) land 0x3FFFFFFF

let rec eq s t =
  match s, t with  
    | Uninterp(x), Uninterp(y) -> Name.eq x y 
    | Arith(f), Arith(g) -> eq_arith f g 
    | Product(f), Product(g) -> eq_product f g
    | Coproduct(op1), Coproduct(op2) -> op1 = op2
    | Bv(f), Bv(g) -> eq_bv f g
    | Pp(f), Pp(g) -> eq_pp f g
    | Fun(f), Fun(g) -> eq_apply f g
    | Bvarith(f), Bvarith(g) -> eq_interp f g
    | Arrays(f), Arrays(g) -> eq_arrays f g
    | _ -> false

and eq_interp f g =
  match f, g with
    | Unsigned, Unsigned -> true

and eq_bv f g = 
  match f, g with
    | Const(b1), Const(b2) -> 
	Pervasives.compare b1 b2 = 0
    | Conc(n1,m1), Conc(n2,m2) ->
	n1 = n2 && m1 = m2
    | Sub(n1,i1,j1), Sub(n2,i2,j2) ->
	n1 = n2 && i1 = i2 && j1 = j2
    | Bitwise(n1), Bitwise n2 ->
	n1 = n2
    | _ -> false

and eq_product f g = 
  match f, g with
    | Tuple, Tuple -> true 
    | Proj(i,n), Proj(j,m) -> i = j && n = m
    | _ -> false

and eq_apply f g =
  match f, g with
    | Apply _, Apply _ -> true
    | Abs, Abs -> true
    | _ -> false

and eq_arith f g =
  match f, g with
    | Num(q1), Num(q2) -> Q.equal q1 q2
    | Multq(q1), Multq(q2)  -> Q.equal q1 q2
    | Add, Add -> true
    | _ -> false

and eq_arrays f g =
  match f, g with
    | Create, Create -> true
    | Select, Select -> true
    | Update, Update -> true
    | _ -> false

and eq_pp f g =
  match f, g with
    | Expt(n), Expt(m) -> n = m
    | Mult, Mult -> true
    | _ -> false


let pp fmt s = 
  let rec sym s =
    match s with 
      | Uninterp(f) -> Name.pp fmt f
      | Arith(op) -> arith op
      | Product(op) -> product op
      | Bv(op) -> bv op
      | Coproduct(op) -> coproduct op
      | Arrays(op) -> array op
      | Pp(op) -> pprod op
      | Fun(op) -> apply op
      | Bvarith(op) -> interp op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q 
      | Add -> Format.fprintf fmt "+"
      | Multq(q) ->  Mpa.Q.pp fmt q; Format.fprintf fmt "*" 

  and product op =
    match op with
      | Tuple -> Format.fprintf fmt "tuple"
      | Proj(i,n) -> Format.fprintf fmt "proj[%d,%d]" i n

  and coproduct op =
    match op with
      | InL -> Format.fprintf fmt "inl"
      | InR ->  Format.fprintf fmt "inr"
      | OutL -> Format.fprintf fmt "outl"
      | OutR -> Format.fprintf fmt "outr"

  and bv op =
    match op with
      | Const(b) -> Format.fprintf fmt "0b%s" (Bitv.to_string b)
      | Conc(n,m) -> Format.fprintf fmt "conc[%d,%d]" n m
      | Sub(n,i,j) -> Format.fprintf fmt "sub[%d,%d,%d]" n i j
      | Bitwise(n) -> Format.fprintf fmt "ite[%d]" n

  and array op =
    match op with
      | Create -> Format.fprintf fmt "create"
      | Select -> Format.fprintf fmt "select"  
      | Update -> Format.fprintf fmt "update"

  and interp op =
    match op with
      | Unsigned -> Format.fprintf fmt "unsigned"

  and apply op =
    match op with
      | Apply(Some(c)) -> 
	  Pretty.string fmt ("apply[" ^ Pretty.to_string Dom.pp c ^ "]")
      | Apply(None) ->
	  Pretty.string fmt "apply"
      | Abs -> 
	  Pretty.string fmt "lambda"

  and pprod op =
    match op with
      | Mult ->
	  Format.fprintf fmt "."
      | Expt(n) ->
	  Format.fprintf fmt "^%d" n
  in
  sym s



(** Width of bitvector function symbols. *)
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


(** {6 Some predefined symbols} *)

let add = Arith(Add)
let tuple = Product(Tuple)
let car = Product(Proj(0, 2))
let cdr = Product(Proj(1, 2))
