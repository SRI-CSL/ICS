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


(** {6 Theory-specific recognizers} *)

let is_interp a =
  match a with
    | Term.App(Sym.Arith _, _) -> true
    | _ -> false

let is_pure = Term.is_pure Th.a


(** {6 Constants} *)

let mk_num q = Term.App.mk_const (Sym.Arith.num q)

let mk_zero = mk_num Q.zero
let mk_one = mk_num Q.one
let mk_two = mk_num (Q.of_int 2)

let mk_inf = mk_num (Q.of_int 123456789)                 (* hack *)
let mk_eps = mk_num (Q.div Q.one (Q.of_int 123456789))



(** {6 Destructors} *)

let d_interp = function
  | Term.App(Sym.Arith(op), al) -> (op, al)
  | _ -> raise Not_found

let d_num a =
  match d_interp a with
    | Sym.Num(q), [] -> q
    | _ -> raise Not_found

let d_multq a =
 match d_interp a with
    | Sym.Multq(q), [x] -> (q, x)
    | _ -> raise Not_found

let d_add a =
  match d_interp a with
    | Sym.Add, al -> al
    | _ -> raise Not_found

let monomials a =
  try d_add a with Not_found -> [a]

let destruct a =
  try
    (match d_interp a with
       | Sym.Num(q), [] -> 
	   (q, mk_zero)
       | Sym.Multq(_), [_] -> 
	   (Mpa.Q.zero, a)
       | Sym.Add, [] -> 
	   (Mpa.Q.one, mk_zero)
       | Sym.Add, [b] ->
	   (try (d_num b, mk_zero) with Not_found -> (Mpa.Q.zero, a))
       | Sym.Add, [b1; b2] ->
	   (try (d_num b1, b2) with Not_found -> (Mpa.Q.zero, a))
       | Sym.Add, b :: bl ->   (* now [bl] contains at least two elements *)
	   (try (d_num b, Term.App.mk_app Sym.Arith.add bl) 
	    with Not_found -> (Mpa.Q.zero, a))
       | _ -> 
	   (Mpa.Q.zero, a))
  with
      Not_found -> (Mpa.Q.zero, a)


(** {6 Predicates} *)

let is_q q a =
  try
    let p = d_num a in
      Q.equal q p
  with
      Not_found -> false

let is_num a =
  try let _ = d_num a in true with Not_found -> false

let is_zero = is_q Q.zero
let is_one = is_q Q.one

let rec is_diophantine a =
  try
    (match d_interp a with
       | Sym.Num _, [] -> true
       | Sym.Multq _, [a] -> is_diophantine a
       | Sym.Add, al ->  List.for_all is_diophantine al
       | _ -> false)
  with
      Not_found -> Term.Var.is_int a


let is_diophantine_equation e =
  is_diophantine (Term.Equal.lhs e) &&
  is_diophantine (Term.Equal.rhs e)



(** {6 Normalizations} *)

let poly_of a =
  try
    let (op, l) = d_interp a in
      (match op, l with
	 | Sym.Num(q), [] -> 
	     (q, [])
	 | Sym.Multq _, _ -> 
	     (Q.zero, [a])
	 | Sym.Add, ((x :: xl') as xl) ->
	     (try (d_num x, xl') with Not_found -> (Q.zero, xl))
	 | _ -> 
	     assert false)
  with
      Not_found -> (Q.zero, [a])

let constant_of a =
  let (q, _) = poly_of a in q

let monomials_of a = 
  let (_, ml) = poly_of a in ml

let of_poly q l =
  let m = if  Q.is_zero q then l else mk_num q :: l in
    match m with 
      | [] -> mk_zero
      | [x] -> x
      | _ -> Term.App.mk_app Sym.Arith.add m

let mono_of a = 
  try d_multq a with Not_found -> (Q.one, a)

let of_mono q x =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then 
    x
  else 
    try 
      let p = d_num x in
	mk_num (Q.mult q p)
    with
	Not_found -> Term.App.mk_app (Sym.Arith.multq q) [x]


(** {6 Iterators} *)

module Monomials = struct

  type t = Mpa.Q.t * Term.t

  let is_true _ = true
  let is_pos (q, _) = Mpa.Q.is_pos q
  let is_neg (q, _) = Mpa.Q.is_neg q
  let is_var y (_, x) = Term.eq y x
  let is_slack (_, x) = Term.Var.is_slack x

  let (&&&) p q m = p m && q m
  let (|||) p q m = p m || q m

  let of_term a = snd(poly_of a)

  let fold p f a e =
    List.fold_left
      (fun acc m ->
	 let qx = mono_of m in
	   if p qx then f qx acc else acc)
      e (of_term a)

  let iter p f a =
    List.iter
      (fun m ->
	 let qx = mono_of m in
	   if p qx then f qx)
	(of_term a)

  let exists p a =
    List.exists (fun m -> p (mono_of m)) (of_term a)

  let is_empty p a = not(exists p a)
	
  let for_all p a =
    List.for_all (fun m -> p (mono_of m)) (of_term a)
	
  let partition p a =
    let (n, ml) = poly_of a in
    let p' m = p (mono_of m) in
    let (ml1, ml2) = List.partition p' ml in
      (n, of_poly Q.zero ml1, of_poly Q.zero ml2)
      

  exception Found

  let choose p a =
    let (_, ml) = poly_of a in
    let result = ref (Obj.magic 1) in
      try
	List.iter
	  (fun m ->
	     let (q, x) = mono_of m in
	       if p (q, x) then
		 begin
		   result := (q, x); 
		   raise Found
		 end)
	  ml;
	raise Not_found
      with
	  Found -> !result

  module Pos = struct
    let is_empty = is_empty is_pos
    let is_empty = Trace.func "foo6" "pos_is_empty" Term.pp Pretty.bool is_empty
    let for_all p = for_all (is_neg ||| p)
    let exists p = exists (p &&& is_pos)
    let mem x = exists (is_var x)
    let choose p = choose (is_pos &&& p)
    let least = choose is_pos	
    let coefficient_of x a = fst(choose (is_pos &&& is_var x) a)
    let fold f = fold is_pos f
    let iter = iter is_pos
  end

  module Neg = struct
    let is_empty = is_empty is_neg
    let for_all p = for_all (is_pos ||| p)
    let exists p = exists (p &&& is_neg)
    let mem x = exists (is_var x)
    let choose p = choose (is_neg &&& p)
    let least = choose is_neg	
    let coefficient_of x a = fst(choose (is_neg &&& is_var x) a)
    let fold f = fold is_neg f
    let iter = iter is_neg
  end

end 

let coefficient_of x a = 
  let p (_, y) = Term.eq x y in
    fst(Monomials.choose p a)
	
let lcm_of_denominators a =
  let p = constant_of a in
  let lcm0 = if Q.is_zero p then Z.one else Q.denominator p in
    Monomials.fold Monomials.is_true
      (fun (q, _) -> 
	 Z.lcm (Q.denominator q))
      a lcm0

      (** Test if [a >= 0]. *)
let is_nonneg a =
  let q = constant_of a in
    if Mpa.Q.is_nonneg q 
      && Monomials.Neg.is_empty a 
      && Monomials.Pos.for_all Monomials.is_slack a
    then
      Three.Yes
    else if Mpa.Q.is_neg q 
      && Monomials.Pos.is_empty a
      && Monomials.Neg.for_all Monomials.is_slack a
    then
      Three.No
    else 
      Three.X

let is_pos a =
  let q = constant_of a in
    if Mpa.Q.is_pos q 
      && Monomials.Neg.is_empty a 
      && Monomials.Pos.for_all Monomials.is_slack a
    then
      Three.Yes
    else if Mpa.Q.is_neg q 
      && Monomials.Pos.is_empty a 
      && Monomials.Neg.for_all Monomials.is_slack a
    then
      Three.No
    else
      Three.X

let is_nonneg =
  Trace.func "foo6" "Arith.is_nonnneg" Term.pp Pretty.three is_nonneg

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
  let add = Term.App.mk_app Sym.Arith.add in
  if Q.is_zero q then a else
    try
      (match d_interp a with
	 | Sym.Num(p), [] ->
	     mk_num (Q.add q p)
	 | Sym.Multq(_), [_] ->
	     add [mk_num q; a]
	 | Sym.Add, [] ->
	     mk_num q
	 | Sym.Add, ((x :: xl') as xl) ->
	     (try
		let p = d_num x in
		let q_plus_p = Q.add q p in
		  if Q.is_zero q_plus_p then
		    add xl'
		  else 
		    add (mk_num q_plus_p :: xl')
	      with
		  Not_found -> add (mk_num q :: xl))
	 | _ ->
	     assert false)
    with
	Not_found -> add [mk_num q; a]

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

and mk_decr a =
  let (q, l) = poly_of a in
    of_poly (Q.sub q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)
 

(** Mapping a term transformer [f] over [a]. *)
let rec map f a =
  try
    (match d_interp a with
       | Sym.Num _, [] -> 
	   a
       | Sym.Multq(q), [x] ->
	   let x' = map f x in
	     if x == x' then a else 
	       mk_multq q x'
      | Sym.Add, [x; y] -> 
	  let x' = map f x and y' = map f y in
	    if x == x' && y == y' then a else 
	      mk_add x' y'
      | Sym.Add, xl -> 
	  let xl' = Term.mapl (map f) xl in
	    if xl == xl' then a else
	      mk_addl xl'
      | _ -> 
	  assert false)
   with
       Not_found -> f a

(** Replacing a variable with a term. *)
let apply sl = Term.Subst.apply map sl

let apply1 (x, b) =
  let lookup y = if Term.eq x y then b else y in
    map lookup


(** Interface for canonizing arithmetic terms. *)
let rec sigma op l =
  match op, l with
    | Sym.Num(q), [] -> 
	mk_num q
    | Sym.Add, [x; y] -> 
	mk_add x y
    | Sym.Add, _ :: _ :: _ -> 
	mk_addl l
    | Sym.Multq(q), [x] ->
	mk_multq q x
    | _ ->  
	assert false


(** {6 Abstract domain interpretation} *)

let dom of_term op al =
  try
    (match op, al with
      | Sym.Num(q), [] -> 
	  Dom.of_q q
      | Sym.Multq(q), [a] -> 
	  Dom.multq q (of_term a)
      | Sym.Add, [a; b] -> 
	  Dom.add (of_term a) (of_term b)
      | Sym.Add, _ -> 
	  Dom.addl (List.map of_term al)
      | _ -> 
	  Dom.Real)
  with
      Not_found -> Dom.Real

let rec dom_of a = 
  try
    let (op, al) = d_interp a in
      dom dom_of op al
  with
      Not_found -> Term.Var.dom_of a

let is_int a =
  try
    Dom.sub (dom_of a) Dom.Int
  with
      Not_found -> false


(** {6 Rational Solvers} *)

let qsolve ((a, b) as e) =
  let p, ml = poly_of (mk_sub a b) in
    match ml with
      | [] -> 
	  if Q.is_zero p then None else raise Exc.Inconsistent
      | m :: ml -> 
	  let (q, x) = mono_of m in         (* [p + q * x + ml = 0] *)
	  let b = mk_addq (Q.minus (Q.div p q))
		    (mk_multq (Q.minus (Q.inv q))
		       (mk_addl ml))
	  in
	    Some(x, b)
	

(** {6 Integer solver} *)

let mk_fresh =
  let d = Some(Dom.Int) in
    fun () -> Term.Var.mk_fresh Th.a None d


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


let rec zsolve (a, b) =    
  if Term.eq a b then [] else
    if Term.is_var a && Term.is_var b then
      [Term.orient(a, b)]
    else 
      let (q, ml) = poly_of (mk_sub a b) in   (* [q + ml = 0] *)
	if ml = [] then
	  if Q.is_zero q then [] else raise(Exc.Inconsistent)
	else
	  let (cl, xl) = vectorize ml in     (* [cl * xl = ml] in vector notation *)
	    match Euclid.solve cl (Q.minus q) with
	      | None -> raise Exc.Inconsistent
	      | Some(d, pl) -> 
		  let (kl, gl) = general cl (d, pl) in
		    List.combine xl gl
	     
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
  let fl = ref [] in
  let rec loop al zl =
    match al, zl with
      | [_], [_] -> zl
      | a0 :: ((a1 :: al'') as al'),  z0 :: z1 :: zl'' ->
          let k = mk_fresh () in
	    fl := k :: !fl;
            let e0 = mk_add z0 (mk_multq (Q.div a1 d) k) in
            let e1 = mk_add z1 (mk_multq (Q.div (Q.minus a0) d) k) in
              e0 :: loop al' (e1 :: zl'')
      | _ -> assert false
  in
    (!fl, loop al (List.map mk_num pl))


let integer_solve = ref true

let solve e =
  if !integer_solve && 
    is_diophantine_equation e 
  then
    zsolve e
  else 
    match qsolve e with
      | None -> []
      | Some(c, d) -> [(c, d)]


(** Decompose [a] into [pre + q * y + post]. *)
let destructure y a =
  let rec loop pre post =     (* [pre + post = 0]. *)
    match post with
      | [m] ->
	  let (q, y') = mono_of m in
	    if Term.eq y y' then
	      (pre, q, y, [])
	    else 
	      raise Not_found
      | m :: post' ->
	  let (q, y') = mono_of m in
	    if Term.eq y y' then
	      (pre, q, y, post')
	    else 
	      loop (m :: pre) post'
      | [] ->
	  raise Not_found
  in      
  let p, ml = poly_of a in
  let (pre, q, y, post) = loop [] ml in
    (p, pre, (q, y), post)


let rec isolate y (a, b) =
  assert(Term.subterm y (mk_sub a b));
  if Term.is_var a then
    if Term.eq y a then
      if Term.subterm y b then
	isolate_in_unsolved y (a, b)
      else 
	(a, b)
    else 
      if Term.subterm y b then
	isolate_in_unsolved y (a, b)
      else 
	isolate_in_solved y (a, b)
  else 
    isolate_in_unsolved y (a, b)
  
(** Isolate [y] in a solved equality [x = a]. *)
and isolate_in_solved y (x, a) = 
  assert (Term.subterm y a && not(Term.subterm x a));    
  let (p, pre, (q, y), post) = destructure y a in 
    assert(not(Q.is_zero q)); (* [p + pre + q * y + post = x]. *)
    let b = mk_multq (Q.inv q) (mk_sub x (mk_addq p (mk_addl (pre @ post)))) in
      (y, b)             

and isolate_in_unsolved x ((a, b) as e) =
  assert(Term.subterm x a || Term.subterm x b);
  let a_sub_b = mk_sub a b in
    assert(not(is_num a_sub_b));
    let (p, pre, (q, x), post) = destructure x a_sub_b in
      assert(not(Q.is_zero q));    (* [p + pre + q*x + post = 0] *)
      let c = mk_multq (Q.minus (Q.inv q)) 
		(mk_addq p (mk_addl (pre @ post))) 
      in
	(x, c)
    
   
