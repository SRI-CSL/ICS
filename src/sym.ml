
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

type linarith = 
  | Num of Q.t  
  | Multq of Q.t
  | Add

type tuple = 
  | Product 
  | Proj of int * int

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type interp = 
  | Arith of linarith
  | Tuple of tuple
  | Bv of bv

type internal =
  | Label of int
  | Slack of int * Number.t
  | FreshBv of int
  | FreshT of int

type uninterp = 
  | External of Name.t
  | Internal of internal

type sym = 
  | Uninterp of uninterp
  | Interp of interp

type t = sym Hashcons.hashed


let destruct f = f.node

let eq = (===)

(*s Comparison. *)

let cmp f g =
  Pervasives.compare f g

(*s Equality. *)

let rec equal s t =
  match s, t with
    | Interp(f), Interp(g) -> 
	interp_equal f g
    | Uninterp(Internal(f)), Uninterp(Internal(g)) ->
	fresh_equal f g
    | Uninterp(External(x)), Uninterp(External(y)) ->
	Name.eq x y
    | _ ->
	false

and fresh_equal sym1 sym2 =
  match sym1, sym2 with
    | Label(n), Label(m) -> n = m
    | Slack(n,c), Slack(m,d) -> n = m && Number.eq c d
    | FreshBv(n), FreshBv(m) -> n = m
    | FreshT(n), FreshT(m) -> n = m
    | _ -> false
  

and interp_equal sym1 sym2 =
  match sym1, sym2 with
    | Arith(op1), Arith(op2) ->
	(match op1, op2 with
	   | Num(q1), Num(q2) -> Q.equal q1 q2
	   | Multq(q1), Multq(q2) -> Q.equal q1 q2
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

let ht = Sym.create tablesize
let _ = Tools.add_at_reset (fun () -> Sym.clear ht)

let make = Sym.hashcons ht

let pp full fmt s = 
  let rec sym s =
    match s.node with 
      | Uninterp(External(f)) -> Name.pp fmt f
      | Uninterp(Internal(x)) -> fresh x
      | Interp(Arith(op)) -> arith op
      | Interp(Tuple(op)) -> tuple op
      | Interp(Bv(op)) -> bv op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q
      | Multq(q) -> Mpa.Q.pp fmt q; Format.fprintf fmt "*";
      | Add -> Format.fprintf fmt "+"

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

  and fresh op =
    match op with
      | Label(n) -> Format.printf "v!%d" n
      | Slack(n,c) when full ->
          let str = Tools.pp_to_string Number.pp c in
	  Format.printf "@[k!%d{%s}@]" n str
      | Slack(n,_) ->
	  Format.printf "@[k!%d@]" n
      | FreshBv(n) -> Format.printf "bv!%d" n
      | FreshT(n) -> Format.printf "t!%d" n
  in
  sym s

(*s Building symbols. *)

let mk_uninterp a = make(Uninterp(External(a)))
let mk_internal a = make(Uninterp(Internal(a)))
let mk_interp f = make(Interp(f))

(*s Interpreted symbols. *)

let mk_num q = make(Interp(Arith(Num(q))))
let mk_multq q = make(Interp(Arith(Multq(q))))
let mk_add = make(Interp(Arith(Add)))

let mk_tuple = make(Interp(Tuple(Product)))
let mk_proj i n = make(Interp(Tuple(Proj(i,n))))

let mk_bv_const b = make(Interp(Bv(Const(b))))
let mk_bv_conc n m = make(Interp(Bv(Conc(n,m))))
let mk_bv_sub n i j = make(Interp(Bv(Sub(n,i,j))))
let mk_bv_bitwise n = make(Interp(Bv(Bitwise(n))))

let mk_sin = 
  let x = Name.of_string "sin" in
  make(Uninterp(External(x)))

let mk_cos =
  let x = Name.of_string "cos" in
  make(Uninterp(External(x)))

let mk_unsigned =
  make(Uninterp(External(Name.of_string "unsigned")))

let mk_update =
  make(Uninterp(External(Name.of_string "update")))

let mk_select =
  make(Uninterp(External(Name.of_string "select")))

let mk_floor = 
  make(Uninterp(External(Name.of_string "floor")))

let mk_ceiling = 
  let name = Name.of_string "ceiling" in
  make(Uninterp(External(name)))

let mk_expt = make(Uninterp(External(Name.of_string "expt")))
let mk_mult = make(Uninterp(External(Name.of_string "mult")))


(*s Some recognizers. *)

let is_interp f =
  match f.node with
    | Interp _ -> true
    | _ -> false

let is_arith f =
  match f.node with
    | Interp(Arith _) -> true
    | _ -> false

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


let is_internal f =
  match f.node with
    | Uninterp(Internal _) -> true
    | _ -> false

(*s Test if [f] is an interpreted constant. *)

let is_interpreted_const f =
  match f.node with
    | Interp(Arith(Num _)) -> true
    | _ -> false


(*s Creating fresh labels. *)

let k = ref 0
let _ = Tools.add_at_reset (fun () -> k := 0)

let mk_fresh_label () = 
  incr(k);
  make(Uninterp(Internal(Label(!k))))

let mk_fresh_slack c =
  incr(k);
  make(Uninterp(Internal(Slack(!k,c))))

let mk_label k =
  make(Uninterp(Internal(Label(k))))

let mk_slack k c =
  make(Uninterp(Internal(Slack(k,c))))

let is_slack f =
  match f.node with
    | Uninterp(Internal(Slack _)) -> true
    | _ -> false

let is_label f =
  match f.node with
    | Uninterp(Internal(Label _)) -> true
    | _ -> false


(*s Width of bitvector function symbols. *)

let rec width f =
  match f.node with
    | Interp(Bv(b)) -> Some(width_bv b)
    | Uninterp(Internal(FreshBv(n))) -> Some(n)
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

