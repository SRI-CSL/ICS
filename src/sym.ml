
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
open Hashcons
(*i*)

(*s Interpreted symbols. *)

type arith = 
  | Num of Mpa.Q.t  
  | Add
  | Mult
  | Expt of int

type tuple = 
  | Product 
  | Proj of int * int

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type interp = 
  | Arith of arith
  | Tuple of tuple
  | Bv of bv

(*s Symbols. *)

type sym = 
  | Uninterp of Name.t
  | Interp of interp

type t = sym Hashcons.hashed

let destruct f = f.node

let eq = (===)

(*s Comparison. *)

let cmp = Pervasives.compare

(*s Equality. *)

let rec equal s t =
  match s, t with
    | Interp(f), Interp(g) -> 
	interp_equal f g
    | Uninterp(x), Uninterp(y) ->
	Name.eq x y
    | _ ->
	false

and interp_equal sym1 sym2 =
  match sym1, sym2 with
    | Arith(op1), Arith(op2) ->
	(match op1, op2 with
	   | Num(q1), Num(q2) -> Q.equal q1 q2
	   | Mult, Mult  -> true
	   | Add, Add -> true
	   | Expt(n), Expt(m) -> n = m
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
    | _ -> false

(*s Initial size of hashconsing tables. *)

let tablesize = 31

(*s Hashconsing Symbols. *)


module Sym = Hashcons.Make(        (*s Hashconsing of symbols *)
  struct 
    type t = sym
    let equal = equal
    let hash = Hashtbl.hash
  end)

let ht = Sym.create tablesize      (* do not reset! *)

let make = Sym.hashcons ht

let pp fmt s = 
  let rec sym s =
    match s.node with 
      | Uninterp(f) -> Name.pp fmt f
      | Interp(Arith(op)) -> arith op
      | Interp(Tuple(op)) -> tuple op
      | Interp(Bv(op)) -> bv op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q 
      | Add -> Format.fprintf fmt "+"
      | Mult -> Format.fprintf fmt "*"
      | Expt(n) -> Format.fprintf fmt "expt(%d)" n
     

  and tuple op =
    match op with
      | Product -> Format.printf "tuple"
      | Proj(i,n) -> Format.printf "proj[%d,%d]" i n

  and bv op =
    match op with
      | Const(b) -> Format.printf "0b%s" (Bitv.to_string b)
      | Conc(n,m) -> Format.printf "conc[%d,%d]" n m
      | Sub(n,i,j) -> Format.printf "sub[%d,%d,%d]" n i j
      | Bitwise(n) -> Format.printf "bitwise[%d]" n
  in
  sym s

(*s Building symbols. *)

let mk_uninterp a = make(Uninterp(a))
let mk_interp f = make(Interp(f))

(*s Interpreted symbols. *)

let mk_num q = make(Interp(Arith(Num(q))))
let mk_add = make(Interp(Arith(Add)))
let mk_mult = make(Interp(Arith(Mult)))
let mk_expt n = make(Interp(Arith(Expt(n))))

let mk_tuple = make(Interp(Tuple(Product)))
let mk_proj i n = make(Interp(Tuple(Proj(i,n))))

let mk_bv_const b = make(Interp(Bv(Const(b))))
let mk_bv_conc n m = make(Interp(Bv(Conc(n,m))))
let mk_bv_sub n i j = make(Interp(Bv(Sub(n,i,j))))
let mk_bv_bitwise n = make(Interp(Bv(Bitwise(n))))


(*s Builtin function symbols. *)

let mk_sin = make(Uninterp(Name.of_string "sin"))
let mk_cos = make(Uninterp(Name.of_string "cos"))
let mk_unsigned = make(Uninterp(Name.of_string "unsigned"))
let mk_update = make(Uninterp(Name.of_string "update"))
let mk_select = make(Uninterp(Name.of_string "select"))
let mk_floor = make(Uninterp(Name.of_string "floor"))
let mk_ceiling = make(Uninterp(Name.of_string "ceiling"))
let mk_div = make(Uninterp(Name.of_string "div"))


let is_builtin f = 
  eq f mk_sin ||
  eq f mk_cos ||
  eq f mk_unsigned ||
  eq f mk_update ||
  eq f mk_select ||
  eq f mk_floor ||
  eq f mk_ceiling ||
  eq f mk_div

type builtin = Sin | Cos | Unsigned | Update | Select | Floor | Ceiling | Div

let builtin f =
  assert (is_builtin f);
  if eq f mk_sin then Sin
  else if eq f mk_cos then Cos
  else if eq f mk_unsigned then Unsigned
  else if eq f mk_update then Update
  else if eq f mk_select then Select
  else if eq f mk_floor then Floor
  else if eq f mk_ceiling then Ceiling
  else if eq f mk_div then Div
  else assert false

(*s Some recognizers. *)

let is_interp f =
  match f.node with
    | Interp _ -> true
    | _ -> false

let is_arith f =
  match f.node with
    | Interp(Arith _) -> true
    | _ -> false

let d_arith f = 
  match f.node with
    | Interp(Arith(op)) -> Some(op)
    | _ -> None

let is_tuple f =
  match f.node with
    | Interp(Tuple _) -> true
    | _ -> false

let is_uninterp f =
  match f.node with
    | Uninterp _ -> true
    | _ -> false

let d_uninterp f =
  assert(is_uninterp f);
  match f.node with
    | Uninterp(f) -> f
    | _ -> assert false

let d_interp f =
  match f.node with
    | Interp(op) -> Some(op)
    | _ -> None


(*s Test if [f] is an interpreted constant. *)

let is_interpreted_const f =
  match f.node with
    | Interp(Arith(Num _)) -> true
    | _ -> false


(*s Width of bitvector function symbols. *)

let rec width f =
  match f.node with
    | Interp(Bv(b)) -> Some(width_bv b)
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

(*s Fresh uninterpreted symbols. *)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let rec mk_fresh str = function
  | Some(k) ->
      mk_uninterp (name_of_fresh str k)
  | None ->
      mk_really_fresh str
      
and mk_really_fresh str =
  incr(k);
  let sym = Uninterp(name_of_fresh str !k) in
  if Sym.mem ht sym then
    mk_really_fresh str  (* terminating, since [k] is increased as a side effect. *)
  else 
    make sym

and name_of_fresh str k = 
  Name.of_string (str ^ "!" ^ (string_of_int k))





