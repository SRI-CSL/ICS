
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


type coproduct = InL | InR | OutL | OutR

(*s Builtin Symbols *)

type builtin = 
  | Select | Update
  | Unsigned 
  | Mult | Expt | Div
  | Apply of range
  | Lambda of int

and range = Cnstrnt.t option

(*s Symbols. *)

type t = 
  | Uninterp of Name.t
  | Builtin of builtin
  | Arith of arith
  | Tuple of tuple 
  | Coproduct of coproduct
  | Bv of bv


(*s Comparison. *)

let cmp = Pervasives.compare


(*s Equality. *)

let rec eq s t =
  match s, t with  
    | Uninterp(x), Uninterp(y) ->
	Name.eq x y
    | Builtin(x), Builtin(y) ->
	(match x, y with
	   | Apply _, Apply _ -> true
	   | Lambda i, Lambda j -> i = j
	   | _ -> x = y)
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
    | Coproduct(op1), Coproduct(op2) -> 
	op1 = op2
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

and applyeq u v = 
  match u, v with
    | None, None -> true
    | Some(c), Some(d) -> Cnstrnt.eq c d
    | _ -> false

let pp fmt s = 
  let rec sym s =
    match s with 
      | Uninterp(f) -> Name.pp fmt f
      | Builtin(f) -> builtin f
      | Arith(op) -> arith op
      | Tuple(op) -> tuple op
      | Bv(op) -> bv op
      | Coproduct(op) -> coproduct op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q 
      | Add -> Format.fprintf fmt "+"
      | Multq(q) ->  Mpa.Q.pp fmt q; Format.fprintf fmt "*" 

  and tuple op =
    match op with
      | Product -> Format.fprintf fmt "tuple"
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

  and builtin op = 
    let name = 
      match op with
	| Select -> "select" 
	| Update -> "update"
	| Unsigned -> "unsigned"
	| Apply(Some(c)) -> 
	    "apply[" ^ Pretty.to_string Cnstrnt.pp c ^ "]"
	| Apply _ -> "apply"
	| Lambda(i) -> 
	    "lambda[" ^  string_of_int i ^ "]"
	| Mult -> "*"
	| Expt -> "^"
	| Div -> "/"
    in
      Format.fprintf fmt "%s" name
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



let theory_of = function
  | Arith _ -> Theories.A
  | Tuple _ -> Theories.T
  | Bv _ -> Theories.BV
  | Coproduct _ -> Theories.S
  | _ -> Theories.U


(*s Predefined symbol. *)

let add = Arith(Add)

(*s Function symbols for tuple theory. *)

let product = Tuple(Product)
let car = Tuple(Proj(0, 2))
let cdr = Tuple(Proj(1, 2))


(*s Predefined function symbols for arrays *)

let select = Builtin(Select)
let update = Builtin(Update)

let is_array f =
  f == select || f == update

(*s Unsigned interpretation. *)

let unsigned = Builtin(Unsigned)

let is_unsigned f = (f == unsigned)

(*s Function application. *)

let apply =
  let app = Builtin(Apply(None)) in
    function 
      | None -> app
      | r -> Builtin(Apply(r))

(*s Predefined nonlinear function symbols. *)


let mult =  Builtin(Mult)
let expt = Builtin(Expt)
let div = Builtin(Div)


let is_nonlin = function
  | Builtin(Mult | Expt | Div) -> true
  | _ -> false


(*s Test if argument is a builtin function symbol. *)

let is_builtin f =
  is_array f || is_nonlin f

