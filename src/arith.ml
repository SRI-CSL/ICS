
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
open Sym
open Term

(** {Symbols} *)

let num q = Arith(Num(q))

let multq q = Arith(Multq(q))

let add = Arith(Add)


(** {Theory-specific recognizers}. *)

let is_interp a =
  match a with
    | App(Arith _, _) -> true
    | _ -> false


(** {Iterators}. *)

let rec fold f a e = 
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num _, [] -> e
	   | Add, l -> List.fold_right (fold f) l e
	   | Multq _, [x] -> fold f x e
	   | _ -> assert false)
    | _ ->
	f a e
	

(** {Destructors}. *)

let d_num = function
  | App(Arith(Num(q)), []) -> Some(q)
  | _ -> None

let d_multq = function
  | App(Arith(Multq(q)), [x]) -> Some(q, x)
  | _ -> None

let d_add = function
  | App(Arith(Add), xl) -> Some(xl)
  | _ -> None


let monomials = function
  | App(Arith(Add), xl) -> xl
  | x -> [x]

(** {Recognizers}. *)

let is_num = function
  | App(Arith(Num _), []) -> true
  | _ -> false

let is_zero = function
  | App(Arith(Num(q)), []) -> Q.is_zero q
  | _ -> false

let is_one = function
  | App(Arith(Num(q)), []) -> Q.is_one q
  | _ -> false

let is_q q = function
  | App(Arith(Num(p)), []) -> Q.equal q p
  | _ -> false

let is_multq = function
  | App(Arith(Multq(_)), _) -> true
  | _ -> false


(** {Constants}. *)

let mk_num q = App(Arith(Num(q)), [])

let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)
let mk_two = mk_num(Q.of_int 2)


(** {Normalizations}. *)

let poly_of a =
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num(q), [] -> (q, [])
	   | Multq _, _ -> (Q.zero, [a])
	   | Add, ((x :: xl') as xl) ->
	       (match d_num x with
		  | Some(q) -> (q, xl')
		  | None -> (Q.zero, xl))
	   | _ -> assert false)
    | _ -> 
	(Q.zero, [a])

let of_poly q l =
  let m = if  Q.is_zero q then l else mk_num q :: l in
    match m with 
      | [] -> mk_zero
      | [x] -> x
      | _ -> Term.mk_app add m

let mono_of = function
  | App(Arith(Multq(q)), [x]) -> (q, x)
  | a -> (Q.one, a)

let of_mono q x =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then 
    x
  else 
    match d_num x with
      | Some(p) -> mk_num (Q.mult q p) 
      | None -> mk_app (multq q) [x]


(** {Constructors}. *)

let rec mk_multq q a =
  let rec multq q = function
    | [] -> []
    | m :: ml ->
	let (p,x) = mono_of m in
	  (of_mono (Q.mult q p) x) :: (multq q ml)
  in
    if Q.is_zero q then 
      mk_zero
    else if Q.is_one q then 
      a
    else 
      let (p, ml) = poly_of a in
	of_poly (Q.mult q p) (multq q ml)

and mk_addq q a =
  match a with
    | App(Arith(Num(p)), []) ->
	mk_num (Q.add q p)
    | App(Arith(Multq(_)), [_]) ->
	mk_app add [mk_num q; a]
    | App(Arith(Add), xl) ->
	(match xl with
	   | App(Arith(Num(p)), []) :: xl' ->
	       mk_app add (mk_num (Q.add q p) :: xl')
	   | _ -> 
	       mk_app add (mk_num q :: xl))
    | _ ->
	mk_app add [mk_num q; a]

and mk_add a b =
  let rec map2 f l1 l2 =      (* Add two polynomials *)
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let (q1, x1) =  mono_of m1 in
	  let (q2, x2) = mono_of m2 in
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
  in
  let (q, l) = poly_of a in
  let (p, m) = poly_of b in
    of_poly (Q.add q p) (map2 Q.add l m) 

and mk_addl l =
  match l with
    | [] -> mk_zero
    | [x] -> x
    | [x; y] -> mk_add x y
    | x :: xl -> mk_add x (mk_addl xl)
 
and mk_incr a =
  let (q, l) = poly_of a in
  of_poly (Q.add q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)


(** Mapping a term transformer [f] over [a]. *)
let rec map f a =
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num _, [] -> 
	       a
	   | Multq(q), [x] ->
	       let x' = map f x in
		 if x == x' then a else 
		   mk_multq q x'
	   | Add, [x; y] -> 
	       let x' = map f x and y' = map f y in
		 if x == x' && y == y' then a else 
		   mk_add x' y'
	   | Add, xl -> 
	       let xl' = Term.mapl (map f) xl in
		 if xl == xl' then a else
		   mk_addl xl'
	   | _ -> 
	       assert false)
    | _ ->
	f a


(** Interface for sigmatizing arithmetic terms. *)
let rec sigma op l =
  match op, l with
    | Num(q), [] -> mk_num q
    | Add, [x; y] -> mk_add x y
    | Add, _ :: _ :: _ -> mk_addl l
    | Multq(q), [x] -> mk_multq q x
    | _ ->  assert false



(** Solving of an equality [a = b] in the rationals. *)
let rec solve e =
  let (a, b, j) = Fact.d_equal e in
    match mk_sub a b with
      | App(Arith(Num(q)), []) ->     (* [q = 0] *)
	  if Q.is_zero q then None else raise(Exc.Inconsistent)
      | App(Arith(Multq(q)), [x]) ->  (* [q * x = 0] *)
	  if Q.is_zero q then None else 
	    Some(Fact.mk_equal x mk_zero j)
      | App(Arith(Add), xl) ->
	  (match xl with
	     | App(Arith(Num(q)), []) :: m :: ml ->
		 let (p, x) = mono_of m in (* case [q + p * x + ml = 0] *)
		 let b' = mk_multq (Q.minus (Q.inv p)) (of_poly q ml) in
		 let e' = Fact.mk_equal x b' j in
		   Some(e')
	     | m :: ml ->
		 let (p, x) = mono_of m in (* case [p * x + ml = 0] *)
		   assert(is_var x);
		   let b' = mk_multq (Q.minus (Q.inv p)) (of_poly Q.zero ml) in
		   let e' = Fact.mk_equal x b' j in
		     Some(e')
	     | [] -> 
		 None)
      | a_sub_b ->
	  let e' = Fact.mk_equal a_sub_b mk_zero j in
	    Some(e')
	  

(** Largest monomial and rest. *)
let destructure a = 
  match a with
    | App(Arith(Num(_)), []) ->
	raise Not_found
    | App(Arith(Multq(q)), [x]) ->
	(q, x, mk_zero)
    | App(Arith(Add), xl) ->
	(match xl with
	   | (App(Arith(Num _), []) as p) :: m :: ml ->
	       let (q, x) = mono_of m in
		 (q, x, mk_app add (p :: ml))
	   | m :: ml ->
	       let (q, x) = mono_of m in
		 (q, x, mk_app add ml)
	   | _ ->
	       raise Not_found)
    | _ ->
	raise Not_found
	

(** Integer test. *)
let rec is_int c a = 
  let is_int_var x = 
    try 
      Cnstrnt.dom_of (c x) = Dom.Int 
    with 
	Not_found -> false
  in
    match a with
      | App(Arith(Num(q)), []) ->
	  Mpa.Q.is_integer q
      | App(Arith(Multq(q)), [x]) ->
	  Mpa.Q.is_integer q &&
	  is_int_var x
      | App(Arith(Add), xl) ->
	  List.for_all (is_int c) xl
      | _ ->
	  false


(** Constraints. *)
let rec tau c op al = 
  try
    (match op, al with
       | Num(q), [] ->  
	   Cnstrnt.mk_singleton q
       | Multq(q), [x] -> 
	   Cnstrnt.multq q (c x)
       | Add, _ -> 
	   cnstrnt_of_monomials c al
       | _ -> 
	   Cnstrnt.mk_real)
  with
      Not_found -> Cnstrnt.mk_real
	

(** Constraint of a list of monomials. *)
and cnstrnt_of_monomials c ml =
  try
    let of_monomial = function
      | App(Arith(Num(q)), []) -> Cnstrnt.mk_singleton q
      | App(Arith(Multq(q)), [x]) -> Cnstrnt.multq q (c x)
      | x -> c x 
    in
      match ml with
	| [] -> 
	    Cnstrnt.mk_zero
	| [m] -> 
	    of_monomial m
	| [m1; m2] -> 
	    let i1 = of_monomial m1 in
	    let i2 = of_monomial m2 in
	      Cnstrnt.add i1 i2
	| m :: ml' -> 
	    let i = of_monomial m in
	      Cnstrnt.add i (cnstrnt_of_monomials c ml')
      with
	  Not_found -> Cnstrnt.mk_real

and cnstrnt c a =
  cnstrnt_of_monomials c (monomials a)
