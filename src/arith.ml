
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
open Term
open Sym
(*i*)


(*s Theory-specific recognizers *)

let is_interp a =
  match a with
    | App(Arith _, _) -> true
    | _ -> false


(*s Fold functional *)

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
	

(*s Destructors. *)

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

(*s Recognizers. *)

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



(*s Constants. *)

let mk_num q = App(Arith(Num(q)), [])

let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)
let mk_two = mk_num(Q.of_int 2)
(*s Some normalization functions. *)

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

let of_poly =
  let addsym = Arith(Add) in
  fun q l ->
    let m = if  Q.is_zero q then l else mk_num q :: l in
      match m with 
	| [] -> mk_zero
	| [x] -> x
	| _ -> Term.mk_app addsym m

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
      | None -> App(Arith(Multq(q)), [x])


(*s Constructors. *)

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

and mk_add a b =
  let rec  map2 f l1 l2 =      (* Add two polynomials *)
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
  in
  let (q,l) = poly_of a in
  let (p,m) = poly_of b in
  of_poly (Q.add q p) (map2 Q.add l m)

and mk_addl l =
  match l with
    | [] -> mk_zero
    | [x] -> x
    | x :: xl -> mk_add x (mk_addl xl)
 
and mk_incr a =
  let (q, l) = poly_of a in
  of_poly (Q.add q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)



(*s Apply term transformer [f] at uninterpreted positions. *)

let rec map f a =
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num _, [] -> 
	       a
	   | Multq(q), [x] ->
	       let x' = map f x in
		 if Term.eq x x' then a else mk_multq q x'
	   | Add, [x; y] -> 
	       let x' = map f x and y' = map f y in
		 if Term.eq x x' && Term.eq y y' then a
		 else mk_add x' y'
	   | Add, xl -> 
	       let xl' = Term.mapl (map f) xl in
		 if xl == xl' then a else
		   mk_addl xl'
	   | _ -> 
	       assert false)
    | _ ->
	f a

(*s Replace [x] with [a] in [b]. *)

let replace x a b =
  map (fun y -> if Term.eq x y then a else y) b

let replacel el =
  map (fun x -> try Term.assq x el with Not_found -> x)



(*s Interface for sigmatizing arithmetic terms. *)

let rec sigma op l =
  match op, l with
    | Num(q), [] -> mk_num q
    | Add, [x; y] -> mk_add x y
    | Add, _ :: _ :: _ -> mk_addl l
    | Multq(q), [x] -> mk_multq q x
    | _ ->  assert false


(*s Abstract interpretation in the domain of constraints. *)

let rec cnstrnt ctxt = function
  | (App(Arith(op), l) as a) ->
      let c = 
	match op, l with
	  | Sym.Num(q), [] -> 
	      Cnstrnt.mk_singleton q
	  | Sym.Multq(q), [x] -> 
	      Cnstrnt.multq q (cnstrnt ctxt x)
	  | Sym.Add, l -> 
	      Cnstrnt.addl (List.map (cnstrnt ctxt) l)
	  | _ -> 
	      assert false
      in
	(try 
	   Cnstrnt.inter (ctxt a) c
	 with
	     Not_found -> c)
  | a ->
      ctxt a



(*s Solving of an equality [a = b] in the rationals and the integers. 
  Solve for maximal monomial which satisfies predicate [pred]. *)

let rec solve_for pred (a,b) =
  let (q,l) = poly_of (mk_sub a b) in
  if l = [] then
    if Q.is_zero q then None else raise(Exc.Inconsistent)
  else
    try
      let ((p,x), ml) = destructure pred l in
      assert(not(Q.is_zero p));             (*s case [q + p * x + ml = 0] *)
      let b = mk_multq (Q.minus (Q.inv p)) (of_poly q ml) in
      if Term.eq x b then 
	None
      else 
	Some(orient pred x b)
    with
	Not_found -> 
	  raise Exc.Unsolved

and orient pred x b = 
  if is_interp b then
    (x, b)
  else if is_var b then
    Term.orient (x, b)
  else if x <<< b && pred b then
    (b, x)
  else 
    (x, b)

(*s Destructuring a polynomial into the monomial which satisfies predicate [f]
  and the remaining polynomial. *)

and destructure pred l =
   let rec loop acc l =   
     match l with 
       | m :: ml ->
	   let (p,x) = mono_of m in
	   if pred x then
	     ((p, x), List.rev acc @ ml)
	   else
	     loop (m :: acc) ml
       | [] -> 
	   raise Not_found
   in
   loop [] l


(*s Split a term into the part with constraints and the unconstraint part.
 Also, return the constraint for the term with a constraint. *)

let split ctxt a =
  let (constr, unconstr) = 
    List.fold_right
      (fun x (constr, unconstr) ->
	 try
	   let i = cnstrnt ctxt x in
	   let constr' =  (match constr with
	     | None -> Some([x], i)
	     | Some(yl, j) -> Some(x :: yl, Cnstrnt.add i j))
	   in
	   (constr', unconstr)
	 with
	     Not_found ->
	       let unconstr' = match unconstr with
		 | None -> Some([x])
		 | Some(yl) -> Some(x :: yl)
	       in
	       (constr, unconstr'))
      (monomials a)
      (None, None)
  in
  let constr' = match constr with
    | Some(xl, i) -> Some(mk_addl xl, i)
    | None -> None
  and unconstr' = match unconstr with
    | Some(yl) -> Some(mk_addl yl)
    | None -> None
  in
  (constr', unconstr')
