
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

type nonlin = 
  | Mult
  | Expt of int

type tuple = 
  | Product 
  | Proj of int * int

type boolean = 
  | True 
  | False

type bv =
  | Const of Bitv.t
  | Conc of int * int
  | Sub of int * int * int
  | Bitwise of int

type enum = {
  elems: Name.Set.t;
  idx : Name.t
}

type interp = 
  | Arith of linarith
  | Nonlin of nonlin
  | Tuple of tuple
  | Bool of boolean
  | Enum of enum
  | Bv of bv

type uninterp = Name.t * Arity.t

type sym = 
  | Uninterp of uninterp
  | Interp of interp

type t = sym Hashcons.hashed


let destruct f = f.node

let eq = (===)

let tag f = f.tag

(*s Comparison. *)

let cmp f g =
  Pervasives.compare f g

(*s Equality. *)

let rec equal s t =
  match s, t with
    | Interp(sym1), Interp(sym2) -> 
	interp_equal sym1 sym2
    | Uninterp(x,a), Uninterp(y,b) ->
	Name.eq x y && Arity.eq a b
    | _ ->
	false

and interp_equal sym1 sym2 =
  match sym1, sym2 with
    | Arith(op1), Arith(op2) ->
	(match op1, op2 with
	   | Num(q1), Num(q2) -> Q.equal q1 q2
	   | Multq(q1), Multq(q2) -> Q.equal q1 q2
	   | Add, Add -> true
	   | _ -> false)
    | Nonlin(op1), Nonlin(op2) ->
	(match op1, op2 with
	   | Mult, Mult -> true
	   | Expt(n1), Expt(n2) -> n1 = n2
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
    | Bool(op1), Bool(op2) ->
	op1 = op2
    | Enum({elems = ns; idx = n}), Enum({elems = ms; idx = m}) ->
	Name.Set.equal ns ms && Name.eq n m
    | _ -> false

(*s Initial size of hashconsing tables. *)

let tablesize = 31


(*s Hashconsing Symbols. *)


module Sym = Hashcons.Make(        (*s Hashconsing of symbols *)
  struct 
    type t = sym
    let equal = equal
    let hash = function
      | Uninterp(f,_) -> Hashtbl.hash f
      | Interp(op) -> Hashtbl.hash op
  end)

let ht = Sym.create tablesize
let _ = Tools.add_at_reset (fun () -> Sym.clear ht)

let make = Sym.hashcons ht

let pp full fmt s = 
  let rec sym s =
    match s.node with 
      | Uninterp(f,arity) -> 
	  if full then
	    begin
	      Format.fprintf fmt "@[";
	      Name.pp fmt f; 
	      Format.fprintf fmt "{";
	      Arity.pp fmt arity;
              Format.fprintf fmt "}@]"
	    end
	  else  
	    Name.pp fmt f
      | Interp(Arith(op)) -> arith op
      | Interp(Nonlin(op)) -> nonlin op
      | Interp(Bool(op)) -> boolean op
      | Interp(Tuple(op)) -> tuple op
      | Interp(Bv(op)) -> bv op
      | Interp(Enum(op)) -> enum op

  and arith op =
    match op with
      | Num(q) -> Mpa.Q.pp fmt q
      | Multq(q) -> Mpa.Q.pp fmt q; Format.fprintf fmt "*";
      | Add -> Format.fprintf fmt "+"

  and nonlin op =
    match op with
      | Expt(n) -> Format.printf "expt[%d]" n;
      | Mult -> Format.printf "*"

  and boolean op =
    match op with
      | True -> Format.printf "true"
      | False -> Format.printf "false"

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

  and enum {elems = _; idx = n} =
    Name.pp fmt n

  in
  sym s

(*s Building symbols. *)

let mk_uninterp (a,sgn) = make(Uninterp(a,sgn))
let mk_interp f = make(Interp(f))

(*s Make fresh function symbol *)

let counter = ref (-1)
let _ = Tools.add_at_reset (fun () -> counter := -1)

let fresh = ref Ptset.empty
let _ = Tools.add_at_reset (fun () -> fresh := Ptset.empty)

let is_fresh f = 
  match f.node with
    | Uninterp _ -> Ptset.mem f !fresh
    | _ -> false

let rec mk_fresh (a,sgn) =
  let b = fresh_name a in
  let f' = Uninterp(b,sgn) in
  if Sym.mem ht f' then         (* check if [f'] is really fresh. *)
    mk_fresh (a,sgn)    (* terminates because of side effect in [fresh_name] *)
  else 
    let f = make(f') in            
    (fresh := Ptset.add f !fresh; f)
   
and fresh_name a =
  incr counter;
  Name.of_string (a ^ string_of_int !counter)

(*s Interpreted symbols. *)

let mk_num q = make(Interp(Arith(Num(q))))
let mk_multq q = make(Interp(Arith(Multq(q))))
let mk_add = make(Interp(Arith(Add)))

let mk_mult = make(Interp(Nonlin(Mult)))
let mk_expt n = make(Interp(Nonlin(Expt(n))))

let mk_tt = make(Interp(Bool(True)))
let mk_ff = make(Interp(Bool(False)))

let mk_tuple = make(Interp(Tuple(Product)))
let mk_proj i n = make(Interp(Tuple(Proj(i,n))))

let mk_bv_const b = make(Interp(Bv(Const(b))))
let mk_bv_conc n m = make(Interp(Bv(Conc(n,m))))
let mk_bv_sub n i j = make(Interp(Bv(Sub(n,i,j))))
let mk_bv_bitwise n = make(Interp(Bv(Bitwise(n))))

let mk_enum ns n =
  assert(Name.Set.mem n ns);
  make(Interp(Enum{elems = ns; idx = n}))

let mk_sin = 
  let x = Name.of_string "sin" in
  let dom = [Type.mk_real] in
  let cod = Type.mk_real in
  let arity = Arity.mk_functorial dom cod in
  make(Uninterp(x, arity))

let mk_cos =
  let x = Name.of_string "cos" in
  let dom = [Type.mk_real] in
  let cod = Type.mk_real in
  let arity = Arity.mk_functorial dom cod in
  make(Uninterp(x, arity))

let mk_unsigned = 
  let arity = Arity.mk_functorial [Type.mk_bitvector None] Type.mk_nat in
  make(Uninterp(Name.of_string "unsigned", arity))

let mk_floor = 
  let arity = Arity.mk_functorial [Type.mk_real] Type.mk_int in
  make(Uninterp(Name.of_string "floor", arity))

let mk_ceiling = 
  let name = Name.of_string "ceiling" in
  let dom = [Type.mk_real] in
  let cod = Type.mk_int in
  let arity = Arity.mk_functorial dom cod in
  make(Uninterp(name, arity))


(*s Classify function symbols. *)

type classify =
  | A     (* Linear arithmetic *)
  | NLA   (* Nonlinear arithmetic *)
  | T     (* Tuples *)
  | B     (* Boolean *)
  | U     (* Uninterpreted *)
  | BV    (* Bitvectors. *)
  | E     (* Enumeration *)

let classify f =
  match f.node with
    | Interp(x) ->
	(match x with
	  | Arith _ -> A
	  | Nonlin _ -> NLA
	  | Tuple _ -> T
	  | Bv _ -> BV
	  | Enum _ -> E
	  | Bool _ -> B)
    | Uninterp _ -> U


(*s Some recognizers. *)

let is_interp f =
  match f.node with
    | Interp _ -> true
    | _ -> false

let is_arith f =
  match f.node with
    | Interp(Arith _) -> true
    | _ -> false

let is_nonlin f =
  match f.node with
    | Interp(Nonlin _) -> true
    | _ -> false

let is_bool f =
  match f.node with
    | Interp(Bool _) -> true
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
    | Uninterp(f,sgn) -> (f,sgn)
    | _ -> assert false

(*s Test if [f] is an interpreted constant. *)

let is_interpreted_const f =
  match f.node with
    | Interp(Bool(True | False)) -> true
    | Interp(Arith(Num _)) -> true
    | Interp(Enum _) -> true
    | _ -> false


(*s Width of bitvector function symbols. *)

let rec width f =
  match f.node with
    | Uninterp(_,a) -> 
	(match Arity.destruct a with
	   | Arity.Constant c -> 
	       (match Type.destruct c with
		  | Type.Bitvector(n) -> n
		  | _ -> None)
	   | _ -> None)
    | Interp(Bv(b)) -> 
	Some(width_bv b)
    | _ -> 
	None

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
