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
 *)

open Mpa
open Sym
open Term

(** {6 Symbols} *)

let num q = Arith(Num(q))

let multq q = Arith(Multq(q))

let add = Arith(Add)


(** {6 Theory-specific recognizers} *)

let is_interp a =
  match a with
    | App(Arith _, _) -> true
    | _ -> false


(** {6 Iterators} *)

(** Folding over coefficients. *)
let rec foldcoeffs f a e = 
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num(q), [] -> 
	       f q e
	   | Multq(q), [_] -> 
	       f q e
	   | Add, l -> 
	       List.fold_right (foldcoeffs f) l e
	   | _ -> assert false)
    | _ ->
	f Q.one e
	

(** {6 Destructors} *)

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

(** {6 Recognizers} *)

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


(** {6 Constants} *)

let mk_num q = App(Arith(Num(q)), [])

let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)
let mk_two = mk_num(Q.of_int 2)


(** {6 Normalizations} *)

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


(** {6 Constructors} *)

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
  if Q.is_zero q then a else
    match a with
      | App(Arith(Num(p)), []) ->
	  mk_num (Q.add q p)
      | App(Arith(Multq(_)), [_]) ->
	  mk_app add [mk_num q; a]
      | App(Arith(Add), xl) ->
	  (match xl with
	     | App(Arith(Num(p)), []) :: xl' ->
		 let q_plus_p = Q.add q p in
		   if Q.is_zero q_plus_p then
		     mk_app add xl'
		   else 
		     mk_app add (mk_num q_plus_p :: xl')
	     | _ -> 
		 mk_app add (mk_num q :: xl))
      | _ ->
	  mk_app add [mk_num q; a]

and mk_add a b =
  let rec map2 l1 l2 =      (* Add two polynomials *)
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let (q1, x1) =  mono_of m1 in
	  let (q2, x2) = mono_of m2 in
	  let cmp = Term.cmp x1 x2 in
	    if cmp = 0 then
	      let q = Q.add q1 q2 in
		if Q.is_zero q then 
		  map2 l1' l2'
		else 
		  (of_mono q x1) :: (map2 l1' l2')
	    else if cmp < 0 then
	      m2 :: map2 l1 l2'
	  else (* cmp > 0 *)
	    m1 :: map2 l1' l2
  in
  let (q, l) = poly_of a in
  let (p, m) = poly_of b in
    of_poly (Q.add q p) (map2 l m) 

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

and mk_linear (p, a) (q, b) =
  mk_add (mk_multq p a) (mk_multq q b)


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
let rec qsolve e =
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
	

(** Integer test. Incomplete. *)
let rec is_int c a = 
  let is_int_var x = 
    try 
      Cnstrnt.dom_of (c x) = Dom.Int 
    with 
	Not_found -> false
  in
    match a with
      | App(Arith(Num(q)), []) ->
	  Q.is_integer q
      | App(Arith(Multq(q)), [x]) ->
	  Q.is_integer q &&
	  is_int_var x
      | App(Arith(Add), xl) ->
	  List.for_all (is_int c) xl
      | _ ->
	  false

(** Test if all variables are interpreted in the integers. *)
let rec is_diophantine c a =
  try
    let rec loop = function
      | App(Arith(Num(_)), []) -> true
      | App(Arith(Multq(_)), [x]) -> Cnstrnt.sub (c x) Cnstrnt.mk_int
      | App(Arith(Add), xl) -> List.for_all loop xl
      | a -> Cnstrnt.sub (c a) Cnstrnt.mk_int
    in 
      loop a
  with
      Not_found -> false


(** Constraints. *)
let rec tau c op al = 
  try
    (match op, al with
       | Num(q), [] ->  
	   Cnstrnt.mk_singleton q
       | Multq(q), [x] -> 
	   Cnstrnt.multq q (c x)
       | Add, _ -> 
	   cnstrnt_of_addl c al
       | _ -> 
	   Cnstrnt.mk_real)
  with
      Not_found -> Cnstrnt.mk_real
	

(** Constraint of a list of monomials. *)
and cnstrnt_of_addl c ml =
  try
    let of_monomial = function
      | App(Arith(Num(q)), []) -> 
	  Cnstrnt.mk_singleton q
      | App(Arith(Multq(q)), [x]) -> 
	  Cnstrnt.multq q (c x)
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
	      Cnstrnt.add i (cnstrnt_of_addl c ml')
      with
	  Not_found -> Cnstrnt.mk_real

and cnstrnt c a =
  cnstrnt_of_addl c (monomials a)


(** Check if there exists a [q] such that [q * a] equals [b]. *)
let rec multiple (a, b) =
  let divnum q p =         (* return [r] s.t. [r * q = p]. *)
    if Q.is_zero q then
      if Q.is_zero p then Q.one else raise Not_found
    else 
      Q.div p q
  in   
  let al = monomials a and bl = monomials b in
    match al, bl with
      | [], [] -> Q.one
      | [], _ -> 
	  raise Not_found
      | _, [] -> 
	  raise Not_found
      | a :: al', b :: bl' ->
	  (match a, b with     (* first check for constant monomials. *)
	     | App(Arith(Num(q)), []), App(Arith(Num(p)), []) ->
		 check (divnum q p) al' bl'
	     | App(Arith(Num _), _), _ ->
		 raise Not_found
	     | _, App(Arith(Num _), _) ->
		 raise Not_found
	     | _ ->
		 let (q, x) = mono_of a 
		 and (p, y) = mono_of b in
		   if Term.eq x y then
		     check (divnum q p) al' bl'
		   else 
		     raise Not_found)

and check r al bl =   (* check if [r * al = bl] *)
  match al, bl with
    | [], [] -> r
    | [], _ -> raise Not_found
    | _, [] -> raise Not_found
    | a :: al', b :: bl' ->
	let (q, x) = mono_of a 
	and (p, y) = mono_of b in
	 if Term.eq x y && Q.equal (Q.mult r q) p then
	   check r al' bl'
	 else 
	   raise Not_found

let multiple =
  Trace.func "foo" "Multiple" (Pretty.pair Term.pp Term.pp) Q.pp
    multiple


let leading = function
  | App(Arith(Num _), _) -> 
      raise Not_found
  | App(Arith(Multq(q)), [x]) -> x 
  | App(Arith(Add), (App(Arith(Num _), [])) :: m :: _) ->
      let (_, x) = mono_of m in x
  | App(Arith(Add), m :: ml) ->
      let (_, x) = mono_of m in x
  | _ ->
      raise Not_found

let leading = 
  Trace.func "foo" "Leading" Term.pp Term.pp leading
    

(** {6 Integer solver} *)

let mk_fresh =
  let name = Name.of_string "a" in
    fun () -> Var(Var.mk_fresh name None)


module Euclid = Euclid.Make(
  struct
   type q = Q.t
   let eq = Q.equal
   let ( + ) = Q.add
   let inv = Q.minus
   let zero = Q.zero
   let ( * ) = Q.mult
   let one = Q.one
   let ( / ) = Q.div
   let floor q = Q.of_z (Q.floor q)
   let is_int = Q.is_integer
  end)


let rec zsolve e = 
  let (a, b, _) = Fact.d_equal e in
  let (q, ml) = poly_of (mk_sub a b) in   (* [q + ml = 0] *)
    if ml = [] then
      if Q.is_zero q then [] else raise(Exc.Inconsistent)
    else
      let (cl, xl) = vectorize ml in     (* [cl * xl = ml] in vector notation *)
	match Euclid.solve cl (Q.minus q) with
	  | None -> raise Exc.Inconsistent
	  | Some(d, pl) -> 
	      let gl = general cl (d, pl) in
		List.map2 (fun x a -> Fact.mk_equal x a None) xl gl
	     
and vectorize ml =
  let rec loop (ql, xl) = function
    | [] -> 
	(List.rev ql, List.rev xl) 
    | m :: ml ->
	let (q, x) = mono_of m in
	  loop (q :: ql, x :: xl) ml
  in
    loop ([], []) ml


(** Compute the general solution of a linear Diophantine
  equation with coefficients [al], the gcd [d] of [al]
  and a particular solution [pl]. In the case of four
  coeffients, compute, for example,
   [(p0 p1 p2 p3) + k0/d * (a1 -a0 0 0) + k1/d * (0 a2 -a1 0) + k2/d * (0 0 a3 -a2)]
  Here, [k0], [k1], and [k2] are fresh variables. Note that
  any basis of the vector space of solutions [xl] of the 
  equation [al * xl = 0] would be appropriate. *)
and general al (d, pl) =
  let rec loop al zl =
    match al, zl with
      | [_], [_] -> zl
      | a0 :: ((a1 :: al'') as al'),  z0 :: z1 :: zl'' ->
          let k = mk_fresh () in
          let e0 = mk_add z0 (mk_multq (Q.div a1 d) k) in
          let e1 = mk_add z1 (mk_multq (Q.div (Q.minus a0) d) k) in
            e0 :: loop al' (e1 :: zl'')
      | _ -> assert false
  in
    loop al (List.map mk_num pl)

    
(** Normalize a constraint. *)
let rec normalize (a, c) =
  match a with
    | App(Arith(Multq(q)), [x]) 
	when not(Q.is_zero q) ->
	(x, Cnstrnt.multq (Q.inv q) c)
    | App(Arith(Add), [App(Arith(Num(q)), []);  App(Arith(Multq(p)), [x])]) ->
	assert(not(Q.is_zero p));         (* [q + p*x in c] iff *)
	let pinv = Q.inv p in             (* [x in 1/ p * (c - q)] *)
	let c' = Cnstrnt.multq pinv (Cnstrnt.addq (Q.minus q) c) in
	  (x, c')
    | _ ->
	let (p, ml) = poly_of a in
	let (a', c') = (mk_addl ml, Cnstrnt.addq (Q.minus p) c) in
	let lcm =       (* least-common multiple of denominators of all rationals. *)
	  foldq 
	    (fun q -> 
	       Z.lcm (Q.denominator q)) 
	    (a', c') 
	    Z.one
	in
	let  lcm' = Q.of_z lcm in
	let gcd =       (* greatest common divisor of integerized coefficients. *)
	  foldq 
	    (fun q gcd -> 
	       let q' = Q.mult lcm' q in  (* [q'] is now an integer. *)
		 assert(Z.equal (Q.denominator q') Z.one);
		 Z.gcd (Q.numerator q') gcd)
	    (a', c') 
	    Z.one 
	in
	let gcd' = Q.of_z gcd in
	let multiplier = Q.div lcm' gcd' in
	  if Q.is_one multiplier then
	    (a', c')
	  else
	    (mk_multq multiplier a', Cnstrnt.multq multiplier c')


(** Fold over all rationals in a pair [a in c]. *)
and foldq f (a, c) e =
  foldcoeffs f a (Cnstrnt.foldq f c e)

let normalize (a, c) =
  Trace.call "arith" "Normalize" (a, c) Term.pp_in;
  let (a', c') = normalize (a, c) in
    Trace.exit "arith" "Normalize" (a', c') Term.pp_in;
    (a', c')




