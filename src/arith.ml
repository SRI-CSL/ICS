
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*i*)
open Tools
open Hashcons
open Mpa
open Term
(*i*)


(*s Test if argument is an arithmetic expression. *)

let is_arith a =
  match a.node with
    | Arith _ -> true
    | _ -> false
  
(*s Symbols and basic constructors *)

let num q = hc(Arith(Num(q)))
let zero  = num Q.zero
let one  = num Q.one

let is_one = function
  | {node=Arith(Num q)} when Q.equal q Q.one -> true
  | _ -> false

let is_zero = function
  | {node=Arith(Num q)} when Q.is_zero q -> true
  | _ -> false

(*s Building product and sum. *)

let rec mk_multq q a =
  if Q.is_zero q then
    zero
  else if Q.is_one q then
    a
  else
    match a.node with
      | Bool(Ite(x,y,z)) ->
	  Bool.cond(x, mk_multq q y, mk_multq q z)
      | _ ->
	  hc(Arith(Multq(q,a)))

let rec mk_mult2 a b = 
  match a.node, b.node with 
    | Arith(Num(q)), Arith(Num(p)) ->
	num(Q.mult q p)
    | Arith(Multq(q,x)), Arith(Num(p)) ->
	mk_multq (Q.mult q p) x
    | Arith(Num(q)), Arith(Multq(p,y))->
	mk_multq (Q.mult q p) y
    | Arith(Multq(q,x)), Arith(Multq(p,y)) ->
	mk_multq (Q.mult q p) (hc(Arith(Mult([x;y]))))
    | Arith(Multq(q,x)), _ ->
	mk_multq q (mk_mult2 x b)
    | _, Arith(Multq(p,y)) ->
	mk_multq p (mk_mult2 y b)
    | _ ->
	if Term.cmp a b <= 0 then
	  hc(Arith(Mult([a;b])))
	else 
	  hc(Arith(Mult([b;a])))
      
let rec mk_mult = function
  | [] -> hc(Arith(Num(Q.one)))
  | [t] -> t
  | a :: l -> mk_mult2 a (mk_mult l)
	       
let mk_add = function
  | [] -> hc(Arith(Num(Q.zero)))
  | [t] -> t
  | l -> hc(Arith(Add l)) 

(*s Building a power product *)

let pproduct l =
  match l with
  | [t] -> t
  | _ -> mk_mult l

	  
(*s Test for arithmetic constant. *)

let is_num a =
  match a.node with
    | Arith(Num _) -> true
    | _ -> false
	  
let num_of a =
  match a.node with
    | Arith(Num q) -> q
    | _ -> assert false

let d_num = num_of

let is_one a =
   is_num a && Q.is_one (num_of a)

let is_zero a =
   is_num a && Q.is_zero (num_of a)

let is_unary_minus a =
  match a.node with
    | Arith(Multq(q,x)) -> Q.is_negone q
    | _ -> false

let d_unary_minus a = 
  match a.node with
    | Arith(Multq(q,x)) -> 
	assert(Q.is_negone q);
	x
    | _ ->
	assert false

(*s Some normalization functions. [poly_of a] constructs
  a list of monomials, where a monomial is every term except
  an addition. [d_monomial a] normalizes a monomial into the
  coefficient and the variable part, if any. *)

let poly_of a =
  match a.node with
    | Arith(x) ->
	(match x with
	   | Num(q) ->
	       if Q.is_zero q then [] else [a]
	   | Add(l) -> l
	   | _ -> [a])
    | _ -> 
	[a]

let d_monomial a =
  match a.node with
    | Arith(x) ->
	(match x with
	   | Num(q) ->
	       (q, None)
	   | Multq(q,x) ->
	       (q, Some(x))
	   | Add _ ->
	       failwith "d_monomial: monomial expected"
	   | _ ->
	       (Q.one,Some(a)))
    | _ ->
	(Q.one,Some(a))


(*s Addition *)

let rec addl l1 l2 = 
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' -> 
	match d_monomial m1, d_monomial m2 with
	  | (q1,None), (q2,None) ->
	      num(Q.add q1 q2) :: addl l1' l2' 
	  | (_,None), _ ->
	      m1 :: addl l1' l2
	  | _, (_, None) ->
	      m2 :: addl l1 l2'
	  | (q1,Some(x1)), (q2,Some(x2)) ->
	      let cmp = cmp x1 x2 in
	      if cmp = 0 then
		let q = Q.add q1 q2 in
		if Q.is_zero q then
		  addl l1' l2'
		else 
		  mk_multq q x1 :: addl l1' l2'
	      else if cmp < 0 then
		m1 :: addl l1' l2
	      else
		m2 :: addl l1 l2'


let add2 (a,b) =
  mk_add (addl (poly_of a) (poly_of b))
    
let addl l =
  List.fold_right (fun x acc -> add2(x,acc)) l zero
  
let add = 
  Bool.nary_lift_ite addl

let incr =
  Bool.unary_lift_ite 
    (fun a -> add2(a, one))


(*s Operations on the coefficients of a polynomial *)

let mapq f a =
  let mapq_mono m =
    match m.node with
      | Arith(Num(q)) -> 
	  num (f q)
      | Arith(Multq(q,x)) ->
	  mk_multq (f q) x
      | _ ->
	  mk_multq (f Q.one) m
  in 
  match a.node with
    | Arith(Add(l)) -> 
	let l' = List.fold_right
		   (fun x acc ->
		      let x' = mapq_mono x in
		      if is_zero x' then acc else x' :: acc)
		   l
		   []
	in
	mk_add l'
    | _ ->
	mapq_mono a

let neg     = mapq Q.minus
let divq q  = mapq (Q.div q)
let addq q  = mapq (Q.add q)

let sub =
  Bool.binary_lift_ite
    (fun (a,b) -> add2(a,neg b))

let multq q =
  Bool.unary_lift_ite (mapq (Q.mult q))

let mult2 (a,b) =
  let times (a,b) =
    mk_mult [a;b]
  in
  match a.node, b.node with
    | Arith(Num(q1)), Arith(Num(q2)) ->
	num(Q.mult q1 q2)
    | Arith(Num(q1)), _ ->
	multq q1 b
    | _, Arith(Num(q2)) ->
	multq q2 a
    | _ ->
	Bool.binary_lift_ite times (a,b)

let multl l =
  let times l =
    mk_mult l
  in
  match l with
    | [] -> one
    | [x] -> x
    | [a1;a2] ->
	mult2(a1,a2)
    | _ ->
	cachel 107 times l

let mult = (* Tools.profile "Mult" (cachel 107 multl) *)
  Tools.profile "Mult" (Bool.nary_lift_ite multl)

	     
(*s Division of polynomials. Rather incomplete. *)

let div2 =
  Bool.binary_lift_ite
    (fun (t1,t2) ->
       match t2.node with
	 | Arith(Num q)
	     when not(Q.is_zero q) ->
	       divq q t1
	 | _ ->
	     hc(Arith(Div(t1,t2))))


(*s [destructure a] into the nonconstant and the constant part. *)

let destructure a =
  match poly_of a with
    | [] -> 
	([], Q.zero)
    | x :: l -> 
	if is_num x then 
	  (l, (num_of x)) 
	else 
	  (x :: l, Q.zero)

(*s Solving of an equality [a = b], represented by the pair [a,b)]
    in the rationals and the integers. *)

let qsolve (a,b) =
  let p = sub(a,b) in
  match p.node with
    | Arith(Num(q)) -> 
	if Q.is_zero q then
	  None
	else 
	  raise(Exc.Inconsistent)
    | Arith(Multq(q, x)) ->
	Some(x, num(Q.inv q))
    | Arith(Add(x::y::l')) ->
	(match d_monomial x with
	   | (_,None) ->                     (* [x] is a numeral. *)
	       (match d_monomial y with      (* Solve: [q' + q''*y'' + l' = 0] *)
		  | (q'', Some(y'')) ->
                      assert(not(Q.is_zero q''));
		      Some(y'', multq (Q.minus (Q.inv q'')) (mk_add (x :: l')))
		  | _ ->
		      assert false)
	   | (q', Some(x')) ->                (* Solve: [q'*x' + y + l' = 0] *) 
	       assert(not(Q.is_zero q'));
	       Some(x', multq (Q.minus (Q.inv q')) (neg(mk_add(y::l')))))
    | _ ->
	Some(p, zero)

    
let zsolve (a,b) = 
  failwith "to do"

    
		    (*s Gcd and Lcm of a polynomial's coefficients. *)
let lcm a =
  let lcm_of_mono m = 
    match m.node with
      | Arith(Num(q)) -> 
	  Q.denominator q
      | Arith(Multq(q, _)) -> 
	  Q.denominator q
      | _ ->
	  Z.of_int 1
  in
  List.fold_right
    (fun m acc ->
       Z.lcm (lcm_of_mono m) acc)
    (poly_of a)
    (Z.of_int 1)

 let gcd a = failwith "to do"
(*
   let lcm_of_mono m = 
     match m.node with
       | Arith(Num(q)) ->
	   assert(Q.is_integer q);
	   Q.to_z q
       | Arith(Multq(q, _)) -> 
	   assert(Q.is_integer q);
	   Q.to_z q
       | _ ->
	   Z.of_int 0
   in 
   List.fold_right
     (fun m acc ->
	Z.gcd (lcm_of_mono m) acc)
     (poly_of a)
     (Z.of_int 0)
 *)


(*s Leading coefficient *)

let leading l =
  let leading_of_mono m =
    match m.node with
      | Arith(Num(q)) -> q
      | Arith(Multq(q,_)) -> q
      | _ -> Q.one
  in
  match l with
    | [] -> Q.zero
    | m :: _ -> leading_of_mono m
    
   
(*s Normalized inequalities. First we make all the coefficients integer,
    then we make the [gcd] of these new coefficients equal to 1. *)

let normalize a =
  let lcm = Q.of_z(lcm a) in
  assert(Q.gt lcm Q.zero);
  let a' = multq lcm a in
  let gcd = Q.of_z(gcd a') in
  assert (Q.gt gcd Q.zero);
  let a'' = divq gcd a' in
  destructure a''

let rec mem c a =
  if Interval.is_top c then 
    tt()
  else if Interval.is_bot c then
    ff()
  else if is_num a then
    let q = d_num a in
    if Interval.mem q c then tt() else ff()
  else if is_ground a then
    ff()
  else 
    Cnstrnt.app c a

let lt (a,b) =
  let p = sub(a,b) in
  if is_num p then
    let q = num_of p in
    if Q.lt q Q.zero then tt() else ff()
  else if is_unary_minus p then
    let c = Interval.gt Interval.Real Q.zero in
    mem c (d_unary_minus p)
  else 
    let c = Interval.lt Interval.Real Q.zero in
    mem c p


let le (a,b) =
  let p = sub(a,b) in
  if is_num p then
    let q = num_of p in
    if Q.le q Q.zero then Bool.tt() else Bool.ff()
  else if is_unary_minus p then
    let c = Interval.ge Interval.Real Q.zero in
    mem c (d_unary_minus p)
  else
    let c = Interval.le Interval.Real Q.zero in
    mem c p
    
(*s Constructor for domain constraints *)

let int = Cnstrnt.app Interval.int
    
let real = Cnstrnt.app Interval.real

(*s Integer test for power products of a polynomial. *)

let is_diophantine is_int a = false
