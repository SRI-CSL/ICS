
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
open Tools
open Hashcons
open Mpa
open Term
open Sym
(*i*)


(*s Theory-specific recognizers *)

let is_interp a =
  Sym.is_arith (Term.sym_of a)

let d_interp a =
  match a.node with
    | App({node=Interp(Arith(f))},l) -> 
	Some(f,l)
    | _ -> 
	None

(*s Apply [f] at uninterpreted positions. *)

let rec iter f a =          
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num _, [] -> ()
	   | Multq _, [x] -> f x
	   | Add, _::_::_ -> List.iter (iter f) l
	   | _ -> assert false)
    | _ ->
	f a

(*s Destructors. *)

let d_num a =
  match a.node with
    | App({node=Interp(Arith(Num(q)))},[]) -> Some(q)
    | _ -> None

let d_multq a =
  match a.node with
    | App({node=Interp(Arith(Multq(q)))},[x]) -> Some(q,x)
    | _ -> None

let d_add a =
  match a.node with
    | App({node=Interp(Arith(Add))},l) -> Some(l)
    | _ -> None


(*s Constants. *)

let mk_num q = Term.make(Sym.mk_num(q),[])

let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)


(*s Some normalization functions. *)

let poly_of a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num(q), [] -> 
	       (q, [])
	   | Multq _, [_] -> 
	       (Q.zero, [a])
	   | Add, ((x :: xl') as xl) ->
	       (match d_num x with
		  | Some(q) -> (q, xl')
		  | None -> (Q.zero, xl))
	   | _ -> 
	       failwith ("Linarith.poly_of: Ill-formed monomial"))
    | _ -> 
	(Q.zero, [a])

let of_poly q l =
  let m = if  Q.is_zero q then l else mk_num q :: l in
  match m with 
    | [] -> mk_zero
    | [x] -> x
    | _ -> Term.make(Sym.mk_add, m)  

let mono_of a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op,l with
	   | Multq(q), [x] -> (q, x)
	   | _ -> failwith("Arith.mono_of: Ill-formed monomial"))
    | _ -> 
	(Q.one ,a)

let of_mono q x =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then 
    x
  else 
    match d_num x with
      | Some(p) -> mk_num (Q.mult q p) 
      | None -> Term.make(Sym.mk_multq(q),[x])


(*s Constructors. *)

let rec mk_multq q a =
  if Q.is_zero q then 
    mk_zero
  else if Q.is_one q then 
    a
  else 
    let (p,l) = poly_of a in
    of_poly (Q.mult q p) (multq q l)

and multq q l =
  match l with
    | [] -> []
    | m :: ml ->
	let (p,x) = mono_of m in
	(of_mono (Q.mult q p) x) :: (multq q ml)

and mk_add a b =
  let (q,l) = poly_of a in
  let (p,m) = poly_of b in
  of_poly (Q.add q p) (map2 Q.add l m)

and map2 f l1 l2 =      (* Add two polynomials *)
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' ->
	let (q1,x1) =  mono_of m1 in
	let (q2,x2) = mono_of m2 in
	let cmp = Term.cmp x1 x2 in
	if cmp = 0 then
	  let q = f q1 q2 in
	  if Q.is_zero q then 
	    map2 f l1' l2'
	  else 
	    (of_mono q x1) :: (map2 f l1' l2')
	else if cmp < 0 then
	  m2 :: map2 f l1 l2'
	else (* cmp > 0 *)
	  m1 :: map2 f l1' l2

and mk_addl l =
  match l with
    | [] -> mk_zero
    | [x] -> x
    | x :: xl -> mk_add x (mk_addl xl)
 
and mk_incr a =
  let (q,l) = poly_of a in
  of_poly (Q.add q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)

(*s Apply term transformer [f] at uninterpreted positions. *)

let rec norm f a =
  match Term.destruct a with
    | {node=Interp(Arith(op))},l ->
	(match op, l with
	   | Num _, [] -> a
	   | Multq(q), [x] -> mk_multq q (norm f x)
	   | Add, [x;y] -> mk_add (norm f x) (norm f y)
	   | Add, _::_::_ -> mk_addl (Term.mapl (norm f) l)
	   | _ -> assert false)
    | _ ->
	f a

(*s Interface for sigmatizing arithmetic terms. *)

let rec sigma op l =
  match op, l with
    | Num(q), [] -> mk_num q
    | Add, [x;y] -> mk_add x y
    | Add, _ :: _ :: _ -> mk_addl l
    | Multq(q), [x] -> mk_multq q x
    | _ ->  assert false

(*s Solving of an equality [a = b] in the rationals and the integers. 
  Solve for largest term. *)

let rec solve (a,b) = 
  let pred _ = true in         (* solve for maximal monomial. *)
  solve_for pred (a,b)

and solve_for pred (a,b) =
  let (q,l) = poly_of (mk_sub a b) in
  if l = [] then
    if Q.is_zero q then [] else raise(Exc.Inconsistent)
  else
    let ((p,x), ml) = destructure pred l in
    assert(not(Q.is_zero p));             (*s case [q + p * x + ml = 0] *)
    let b = mk_multq (Q.minus (Q.inv p)) (of_poly q ml) in
    if x === b then [] else [order x b]

and order x b =
  if is_interp b then
    (x, b)
  else if x <<< b then
    (b, x)
  else 
    (x,b)


(*s Destructuring a polynomial into the monomial which satisfies predicate [f]
  and the remaining polynomial. *)

and destructure pred l =
   let rec loop acc l =   
     match l with 
       | [m] -> 
             (mono_of m, List.rev acc)
       | m :: ml ->
	   let (p,x) = mono_of m in
	   if pred x then
	     ((p, x), List.rev acc @ ml)
	   else
	     loop (m :: acc) ml
       | _ -> assert false
   in
   loop [] l


