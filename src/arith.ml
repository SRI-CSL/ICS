
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
  Term.is_app a &&
  Sym.is_arith (Term.sym_of a)

let d_interp a =
  if Term.is_var a then
    None
  else 
    match Sym.destruct (Term.sym_of a) with
      | Interp(Arith(f)) -> Some(f, Term.args_of a)
      | _ -> None


(*s Fold functional *)

let rec fold f a e =        
  match d_interp a with
    | Some(op,l) ->
	(match op, l with
	   | Sym.Num _, [] -> e
	   | (Sym.Add | Sym.Mult), l -> List.fold_right (fold f) l e
	   | Sym.Expt _, [x] -> fold f x e
	   | _ -> assert false)
    | _ ->
	f a e


(*s Destructors. *)

let d_num a =
  match d_interp a with
    | Some(Sym.Num(q),[]) -> Some(q)
    | _ -> None

let d_multq a =
  match d_interp a with
    | Some(Sym.Mult, x :: xl) ->
	(match d_num x with
	   | Some(q) -> 
	       (match xl with
		  | [y] -> Some(q, y)
		  | _ :: _ -> Some(q, Term.mk_app Sym.mk_mult xl)
		  | [] -> assert false)
	   | None -> None)
    | _ -> 
	None

let d_add a =
  match d_interp a with
    | Some(Sym.Add, l) -> Some(l)
    | _ -> None

let d_mult a =
 match d_interp a with
    | Some(Sym.Mult, l) -> Some(l)
    | _ -> None

let d_expt a =
  match d_interp a with
    | Some(Sym.Expt(n), [x]) -> Some(n,x)
    | _ -> None

let monomials a =
  match d_add a with
    | Some(l) -> l
    | None -> [a]


(*s Recognizers. *)

let is_num a =
  match d_num a with Some _ -> true | None -> false

let is_zero a =
  match d_num a with
    | Some(q) -> Q.is_zero q
    | _ -> false

let is_one a =
  match d_num a with
    | Some(q) -> Q.is_one q
    | _ -> false

let is_linear a =
  match d_interp a with
    | Some(op, l) ->
	(match op, l with
	   | Sym.Num _, [] -> true
	   | Sym.Add, _ -> true
	   | Sym.Mult, [x;_] when is_num x -> true
	   | _ -> false)
    | _ ->
	false
  

(*s Constants. *)

let mk_num q = Term.mk_const (Sym.mk_num(q))

let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)


(*s Some normalization functions. *)

let poly_of a =
  match d_interp a with
    | Some(op,l)  ->
	(match op, l with
	   | Num(q), [] -> (q, [])
	   | Expt _ , [_] -> (Q.zero, [a])
	   | Mult, _ -> (Q.zero, [a])
	   | Add, ((x :: xl') as xl) ->
	       (match d_num x with
		  | Some(q) -> (q, xl')
		  | None -> (Q.zero, xl))
	   | _ -> assert false)
    | None -> 
	(Q.zero, [a])

let of_poly q l =
  let m = if  Q.is_zero q then l else mk_num q :: l in
  match m with 
    | [] -> mk_zero
    | [x] -> x
    | _ -> Term.mk_app Sym.mk_add m

let mono_of a =
  match d_multq a with
    | Some(q,x) -> (q, x)
    | None -> (Q.one, a)

let of_mono q x =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then 
    x
  else 
    match d_num x with
      | Some(p) -> mk_num (Q.mult q p) 
      | None -> Term.mk_app Sym.mk_mult [mk_num q; x]

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
  let (q,l) = poly_of a in
  of_poly (Q.add q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)

and mk_mult a b =
  let rec multl l1 l2 =             
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let n1,x1 = expt_of m1 in
	  let n2,x2 = expt_of m2 in
	  let cmp = Term.cmp x1 x2 in
	  if cmp = 0 then
	    let n = n1 + n2 in
	    if n = 0 then
	      multl l1' l2'
	    else 
	      (mk_expt n x1) :: (multl l1' l2')
	  else if cmp < 0 then
	    m1 :: multl l1' l2
	  else (* cmp > 0 *)
	    m2 :: multl l1 l2'
  and of_list l =
    match l with
      | [] -> mk_one
      | [x] -> x
      | _ -> Term.mk_app Sym.mk_mult l
  and expt_of a =
    match d_expt a with
      | Some(n,x) -> (n, x)
      | None -> (1, a)
  in
  if Term.eq a b then                                       (* [ a * a --> a^2] *)
    mk_expt 2 a 
  else
    match d_num a, d_num b with
      | Some(q), Some(p) -> mk_num (Q.mult q p)
      | None, Some(p) -> mk_multq p a
      | Some(q), None -> mk_multq q b
      | None, None ->
	  (match d_expt a, d_expt b with
	    | Some(n,x), Some(m,y) when Term.eq x y ->       (* [ x^n * x^m --> x^(n+m)] *)
		mk_expt (n + m) x
	    | Some(n,x), _ when Term.eq x b ->               (* [ x^n * x --> x^(n+1)] *)
		mk_expt (n + 1) x
	    | _, Some(m,y) when Term.eq y a ->               (* [ y * y^m --> y^(m+1)] *)
		mk_expt (m + 1) y
	    | _ ->
		(match d_add a, d_add b with                 (* [(x1+...+xn) * b = x1*b+...+xn*b] *)
		   | Some(xl), _ -> 
		       mk_addl (List.map (mk_mult b) xl)
		   | _, Some(yl) ->                          (* [(y1+...+yn) * b = y1*b+...+yn*b] *)
		       mk_addl (List.map (mk_mult a) yl)
		   | None, None ->
		       (match d_mult a, d_mult b with
			  | Some(xl), Some(yl) ->
			      of_list (multl xl yl)
			  | Some(xl), None ->
			      of_list (multl xl [b])
			  | None, Some(yl) ->
			      of_list (multl yl [a])
			  | _ ->
			      let (a,b) = Term.orient (a,b) in
			      Term.mk_app Sym.mk_mult [a;b])))

and mk_multl al =
  match al with
    | [] -> mk_one
    | [x] -> x
    | x :: y :: zl -> mk_multl (mk_mult x y :: zl)

and mk_expt n a =
  match n with
    | 0 -> mk_one
    | 1 -> a
    | _ ->
	(match d_expt a with
	   | Some(m, x) when Term.eq x a ->
	       mk_expt (n + m) a
	   | _ ->
	       (match d_mult a with
		  | Some(xl) ->
		      mk_multl (List.map (mk_expt n) xl)
		  | None ->
		      (match d_num a with
			 | Some(q) -> 
			     mk_num (Mpa.Q.expt q n)
			 | None ->
			     Term.mk_app (Sym.mk_expt n) [a])))

(*s Decomposition. *)

type decompose =
  | Const of Mpa.Q.t
  | One of Mpa.Q.t * Mpa.Q.t * Term.t
  | Many of Mpa.Q.t * Mpa.Q.t * Term.t * Term.t


let decompose a =
  let (q, al) = poly_of a in
  match al with
    | [] -> 
	Const(q)
    | [m] ->
	let (p,x) = mono_of m in
	One(q,p,x)
    | m :: ml ->
	let (p,x) = mono_of m in
	Many(q, p, x, Term.mk_app Sym.mk_add ml)

(*s Apply term transformer [f] at uninterpreted positions. *)

let rec map f a =
  match d_interp a with
    | Some(op,l)  ->
	(match op, l with
	   | Num _, [] -> a
	   | Mult, [x;y] -> 
	       (match d_num x with
		  | Some(q) -> mk_multq q (map f y)
		  | None -> mk_multl (Term.mapl (map f) l))
	   | Mult, l -> mk_multl (Term.mapl (map f) l)
	   | Add, [x;y] -> mk_add (map f x) (map f y)
	   | Add, l -> mk_addl (Term.mapl (map f) l)
	   | Expt(n), [x] -> mk_expt n (map f x)
	   | _ -> assert false)
    | _ ->
	f a

(*s Replace [x] with [a] in [b]. *)

let replace x a b =
  map (fun y -> if Term.eq x y then a else y) b

let replacel el =
  map (fun x -> try Term.assq x el with Not_found -> x)


(* Does every nonconstant monomial satisfy predicate [p]. *)

let rec for_all p a =
  match d_interp a with
    | Some(op, l) -> 
	(match op, l with
	   | Sym.Num _, [] -> 
	       true
	   | Sym.Add, l -> 
	       for_all_list p l
	   | Sym.Mult, (x :: xl) ->
	       (match d_num x with
		  | Some(q) -> p (q, mk_multl xl)
		  | None -> p (Q.one, a))
	   | Sym.Expt _, [_] ->
	       p (Q.one, a)
	   | _ -> assert false)
    | _ ->
	p (Q.one, a)

and for_all_list p l =
  match l with
    | [] -> true
    | x :: xl -> 
	for_all p x && for_all_list p xl


(*s Interface for sigmatizing arithmetic terms. *)

let rec sigma op l =
  match op, l with
    | Num(q), [] -> mk_num q
    | Add, [x; y] -> mk_add x y
    | Add, _ :: _ :: _ -> mk_addl l
    | Mult, [x;y] ->
	(match d_num x with
	   | Some(q) -> mk_multq q y
	   | None -> mk_mult x y)
    | Mult, _ -> mk_multl l
    | Expt(n), [x] -> mk_expt n x
    | _ ->  assert false


(*s Solving of an equality [a = b] in the rationals and the integers. 
  Solve for largest term. *)

let rec solve p e =
  Trace.call 7 "Solve(a)" e (Pretty.eqn Term.pp);
  let sl = solve1 p e in
  Trace.exit 7 "Solve(a)" sl pp_solved;
  sl

and solve1 p (a,b) =
  let pred x = p x                     (* solve for maximal monomial. *)
                                      (* which is linear and satisfies [p] *)
  in   
  let orient x b =
  if is_interp b then
    (x, b)
  else if x <<< b && pred b then
    (b, x)
  else 
    (x, b)
  in
  let (q,l) = poly_of (mk_sub a b) in
  if l = [] then
    if Q.is_zero q then None else raise(Exc.Inconsistent)
  else
    try
      let ((p,x), ml) = destructure pred l in
      assert(not(Q.is_zero p));             (*s case [q + p * x + ml = 0] *)
      let b = mk_multq (Q.minus (Q.inv p)) (of_poly q ml) in
      if Term.eq x b then None else Some(orient x b)
    with
	Not_found -> raise Exc.Unsolved

and pp_solved fmt sl =
  match sl with
    | None -> Pretty.solution Term.pp fmt []
    | Some(x,a) -> Pretty.solution Term.pp fmt [(x,a)]


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

