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

let is_interp a =
  match a with
    | Term.App(sym, _, _) -> Sym.theory_of sym = Th.la
    | _ -> false

let is_pure = Term.is_pure Th.la

(** Hashconsing creation of numeric constants. *)
let mk_num = 
 let table = Mpa.Q.Hash.create 53 in
 let _ =  Tools.add_at_reset (fun () -> Mpa.Q.Hash.clear table) in
   fun q ->
     try
       Mpa.Q.Hash.find table q 
     with
	 Not_found ->
	   let c = Term.App.mk_const (Sym.Arith.mk_num q) in
	     Mpa.Q.Hash.add table q c; c
	
	
(** Following constants need a parameter, since hashtables in
  module {!Sym} are resettable. *)
let mk_zero () = mk_num Q.zero
let mk_one () = mk_num Q.one
let mk_two () = mk_num (Q.of_int 2)


let d_interp = function
  | Term.App((Sym.Arith(op), _), al, _) -> (op, al)
  | _ -> raise Not_found

let d_num = function
  | Term.App((Sym.Arith(Sym.Num(q)), _), [], _) -> q
  | _ -> raise Not_found

let d_add = function
  | Term.App((Sym.Arith(Sym.Add), _), al, _) -> al
  | _ -> raise Not_found

let is_multq = function
  | Term.App((Sym.Arith(Sym.Multq(_)), _), [_], _) -> true
  | _ -> false


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

let is_nonneg_num a =
  try let q = d_num a in Mpa.Q.is_nonneg q with Not_found -> false

let is_pos_num a =
  try let q = d_num a in Mpa.Q.is_pos q with Not_found -> false

let is_nonpos_num a =
  try let q = d_num a in Mpa.Q.is_nonpos q with Not_found -> false


(** Test if all variables in [a] are interpreted over the integers. *)
let rec is_diophantine a =
  try
    match a with
      | Term.App((Sym.Arith(op), _), al, _) ->
	  (match op, al with
	     | Sym.Num _, [] -> true
	     | Sym.Multq _, [a] -> is_diophantine a
	     | Sym.Add, al ->  List.for_all is_diophantine al
	     | _ -> false)
      | _ ->
	  Term.Var.is_int a
  with
      Not_found -> Term.Var.is_int a

let is_diophantine_equation e =
  is_diophantine (Term.Equal.lhs e) &&
  is_diophantine (Term.Equal.rhs e)


(** The constant part [q] of a polynomial [q + a]. *)
let constant_of = function
  | Term.App((Sym.Arith(op), _), l, _) ->
      (match op, l with
	 | Sym.Num(q), [] -> q
	 | Sym.Add, (Term.App((Sym.Arith(Sym.Num(q)), _), [], _) :: _) -> q
	 | _ -> Q.zero)
    | _ ->
	Q.zero

(** The nonconstant part [a1 + ... + an] of a polynomial [q + a1 + ... + an]
  as a list of monomials *)
let nonconstant_monomials_of a =
  match a with
    | Term.App((Sym.Arith(op), _), al, _) ->
	(match op, al with
	   | Sym.Num(_), [] -> []
	   | Sym.Add, (Term.App((Sym.Arith(Sym.Num(_)), _), [], _) :: al') -> al'
	   | Sym.Add, _ -> al
	   | _ -> [a])
    | _ ->
	[a]

let nonconstant_of a =
  match a with
    | Term.App((Sym.Arith(op), _), l, _) ->
	(match op, l with
	   | Sym.Num(_), [] -> 
	       mk_zero()
	   | Sym.Add, (Term.App((Sym.Arith(Sym.Num(_)), _), [], _) :: al) ->
	       Term.App.mk_app Sym.Arith.mk_add al
	   | _ -> 
	       a)
    | _ ->
	a

(** Create a polynomial with constant part [q] and 
  monomials [ml]. Assumes [ml] is canonical. *)
let mk_polynomial q ml =
  let m = if  Q.is_zero q then ml else mk_num q :: ml in
    match m with 
      | [] -> mk_zero()
      | [x] -> x
      | _ -> Term.App.mk_app Sym.Arith.mk_add m

let mk_monomial q x =
  if Q.is_zero q then
    mk_zero()
  else if Q.is_one q then 
    x
  else 
    try 
      let p = d_num x in
	mk_num (Q.mult q p)
    with
	Not_found -> 
	  Term.App.mk_app (Sym.Arith.mk_multq q) [x]


let coefficient_of_mono = function
  | Term.App((Sym.Arith(Sym.Multq(q)), _), _, _) -> q
  | _ -> Q.one

let variable_of_mono = function
  | Term.App((Sym.Arith(Sym.Multq(_)), _), [x], _) -> x
  | a -> a


let rec iter f a =
  match a with
    | Term.App((Sym.Arith(op), _), al, _) ->
	(match op, al with
	   | Sym.Num _, [] -> ()
	   | Sym.Multq _, [b] -> iter f b
	   | Sym.Add, bl -> List.iter (iter f) bl
	   | _ -> f a)
    | _ ->
	f a

let rec fold f a acc = 
  match a with 
    | Term.App((Sym.Arith(op), _), al, _) ->
	(match op, al with
	   | Sym.Num _, [] -> acc
	   | Sym.Multq _, [b] -> fold f b acc
	   | Sym.Add, bl -> List.fold_right (fold f) bl acc
	   | _ -> f a acc)
    | _ ->
	f a acc

exception Not_holds

let for_all f a =
  try
    iter
      (fun x ->
	 if not(f(x)) then raise Not_holds)
      a;
    true
  with
      Not_holds -> false

(** Iterations over monomials. *)
module Monomials = struct

  type pred = Mpa.Q.t -> Term.t -> bool

  let is_true _ _ = true
  let is_pos q _ = Mpa.Q.is_pos q
  let is_neg q _ = Mpa.Q.is_neg q
  let is_var y _ x = Term.eq y x
  let is_slack _ x = Term.Var.is_slack x

  let (&&&) p1 p2 q x = p1 q x && p2 q x
  let (|||) p1 p2 q x = p1 q x || p2 q x


  let mapq f a =
    let p = constant_of a
    and ml = nonconstant_monomials_of a in
    let ml' = 
      List.fold_right
	(fun m acc ->
	   let q = coefficient_of_mono m
	   and x = variable_of_mono m in
	   let q' = f q in
	     if Mpa.Q.is_zero q' then acc else
	       mk_monomial q' x :: acc)
	ml [] 
    in
      mk_polynomial p ml'

  let fold p f a e =
    List.fold_left
      (fun acc m ->
	 let q = coefficient_of_mono m
	 and x = variable_of_mono m in
	   if p q x then f q x acc else acc)
      e (nonconstant_monomials_of a)

  let iter p f a =
    List.iter
      (fun m ->
	 let q = coefficient_of_mono m
	 and x = variable_of_mono m in
	   if p q x then f q x)
	(nonconstant_monomials_of a)

  let exists p a =
    List.exists 
      (fun m -> 
	 p (coefficient_of_mono m) (variable_of_mono m))
      (nonconstant_monomials_of a)

  let is_empty p a = not(exists p a)
	
  let for_all p a =
    List.for_all 
      (fun m -> 
	 p (coefficient_of_mono m) (variable_of_mono m))
      (nonconstant_monomials_of a)
	

  exception Found

  let choose p a =
    let ml = nonconstant_monomials_of a in
    let result = ref (Obj.magic 1) in
      try
	List.iter
	  (fun m ->
	     let q = coefficient_of_mono m
	     and x = variable_of_mono m in
	       if p q x then
		 begin
		   result := (q, x);
		   raise Found
		 end)
	  ml;
	raise Not_found
      with
	  Found -> !result

  exception FoundVariable of Term.t

  let variable_choose p a =
    try
      List.iter
	(fun m ->
	   let q = coefficient_of_mono m in
	   let x = variable_of_mono m in
	     if p q x then raise(FoundVariable(x)))
	(nonconstant_monomials_of a);
      raise Not_found
    with
	FoundVariable(x) -> x

  exception FoundQ of Mpa.Q.t
    
  let coefficient_choose p a =
    try
      List.iter
	(fun m ->
	   let q = coefficient_of_mono m in
	   let x = variable_of_mono m in
	     if p q x then raise(FoundQ(q)))
	(nonconstant_monomials_of a);
      raise Not_found
    with
	FoundQ(q) -> q

  module Pos = struct
    let is_empty = is_empty is_pos
    let for_all p = for_all (is_neg ||| p)
    let exists p = exists (p &&& is_pos)
    let mem x = exists (is_var x)
    let choose p = choose (is_pos &&& p)
    let variable_choose p = variable_choose (is_pos &&& p)
    let variable_least_of = variable_choose is_pos
    let coefficient_of x = 
      let is_pos_and_eq_var_x = is_pos &&& is_var x in
	coefficient_choose is_pos_and_eq_var_x
    let fold f = fold is_pos f
    let iter = iter is_pos
  end

  module Neg = struct
    let is_empty = is_empty is_neg
    let for_all p = for_all (is_pos ||| p)
    let exists p = exists (p &&& is_neg)
    let mem x = exists (is_var x)
    let choose p = choose (is_neg &&& p)  
    let variable_choose p = variable_choose (is_neg &&& p)
    let variable_least_of = variable_choose is_neg
    let coefficient_of x = 
      let is_neg_and_eq_var_x = is_neg &&& is_var x in
	coefficient_choose is_neg_and_eq_var_x
    let fold f = fold is_neg f
    let iter = iter is_neg
  end

end 

let coefficient_of x a = 
  let eqx _ y = Term.eq x y in
    Monomials.coefficient_choose eqx a
	

let lcm_of_denominators a =
  let p = constant_of a in
  let lcm0 = if Q.is_zero p then Z.one else Q.denominator p in
    Monomials.fold Monomials.is_true
      (fun q _ -> 
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

(** Test if [a > 0]. *)
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


let rec mk_multq q a =
  let rec multq q = function
    | [] -> 
	[]
    | m :: ml ->
	let p = coefficient_of_mono m
	and x = variable_of_mono m in
	  (mk_monomial (Q.mult q p) x) :: (multq q ml)
  in
    if Q.is_zero q then 
      mk_zero()
    else if Q.is_one q then 
      a
    else 
      let p = constant_of a
      and ml = nonconstant_monomials_of a in
	mk_polynomial (Q.mult q p) (multq q ml)

and mk_addq q a =
  let add = Term.App.mk_app Sym.Arith.mk_add in
  if Q.is_zero q then a else
    match a with
      | Term.App((Sym.Arith(op), _), al, _) ->
	  (match op, al with
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
      | _ -> 
	  add [mk_num q; a]

and mk_add a b =
  let rec map2 l1 l2 =      (* Add two polynomials *)
    match l1, l2 with
      | [], _ -> l2
      | _ , [] -> l1
      | m1 :: l1', m2 :: l2' ->
	  let q1 = coefficient_of_mono m1
	  and x1 = variable_of_mono m1
	  and q2 = coefficient_of_mono m2
	  and x2 = variable_of_mono m2 in
	  let cmp = Term.cmp x1 x2 in
	    if cmp = 0 then
	      let q = Q.add q1 q2 in
		if Q.is_zero q then 
		  map2 l1' l2'
		else 
		  (mk_monomial q x1) :: (map2 l1' l2')
	    else if cmp < 0 then
	      m2 :: map2 l1 l2'
	    else (* cmp > 0 *)
	      m1 :: map2 l1' l2
  in
  let q = constant_of a
  and l = nonconstant_monomials_of a
  and p = constant_of b
  and m = nonconstant_monomials_of b in
    mk_polynomial (Q.add q p) (map2 l m) 

and mk_addl l =
  match l with
    | [] -> mk_zero()
    | [x] -> x
    | [x; y] -> mk_add x y
    | [x; y; z] -> mk_add x (mk_add y z)
    | x :: xl -> mk_add x (mk_addl xl)
 
and mk_incr a =
  let q = constant_of a
  and l = nonconstant_monomials_of a in
    mk_polynomial (Q.add q Q.one) l

and mk_decr a =
  let q = constant_of a
  and l = nonconstant_monomials_of a in
    mk_polynomial (Q.sub q Q.one) l

and mk_neg a =
  mk_multq (Q.minus (Q.one)) a

and mk_sub a b =
  mk_add a (mk_neg b)

(** Mapping a term transformer [f] over [a]. *)
let rec map f a =
  match a with
    | Term.App((Sym.Arith(op), _), al, _) ->
	(match op, al with
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
    | _ ->
	f a

(** Replacing a variable with a term. *)
let apply (x, b) =
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

let rec qsolve ((a, b) as e) =
  let a_sub_b = mk_sub a b in
  let p = constant_of a_sub_b
  and ml = nonconstant_monomials_of a_sub_b in
    match ml with
      | [] -> 
	  raise (if Q.is_zero p then Exc.Valid else Exc.Inconsistent)
      | [m] ->                             (* [p + q * x = 0] *)
	  let q = coefficient_of_mono m
	  and x = variable_of_mono m in
	    (x, mk_num (Q.div (Q.minus p) q))
      | [m1; m2] ->                        (* [p + q1 * x1 + m2 = 0] *)
	  let q1 = coefficient_of_mono m1
	  and x1 = variable_of_mono m1 in
	    (x1, mk_addq (Q.div (Q.minus p) q1) (mk_multq (Q.minus (Q.inv q1)) m2))
      | m :: ml ->                         (* [p + q * x + ml = 0] *)
	  let q = coefficient_of_mono m
	  and x = variable_of_mono m in   
	  let b = mk_addq (Q.minus (Q.div p q))
		    (mk_multq (Q.minus (Q.inv q))
		       (mk_addl ml))
	  in
	    if Term.is_var b then
	      Term.orient (x, b)
	    else 
	      (x, b)


(** {6 Integer solver} *)

let fresh = ref []

let mk_fresh =
  let d = Var.Cnstrnt.mk_real(Dom.Int) in
    fun () -> 
      let x = Term.Var.mk_fresh Th.la None d in
	fresh := x :: !fresh;
	x

let is_fresh x =
  let eqx = Term.eq x in
    List.exists eqx !fresh


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
  fresh := [];           (* reinitialize fresh variable generation. *)
  if Term.eq a b then [] else
    if Term.is_var a && Term.is_var b then
      [Term.orient(a, b)]
    else 
      let a_sub_b = mk_sub a b in
      let q = constant_of a_sub_b 
      and ml = nonconstant_monomials_of a_sub_b in     (* [q + ml = 0] *)
	match ml with
	  | [] -> 
	      if Q.is_zero q then [] else raise(Exc.Inconsistent)
	  | [m] ->                           (* [q + p*x = 0] *)
	      let p = coefficient_of_mono m
	      and x = variable_of_mono m in
	      let q_div_p = Q.div q p in
		if Q.is_integer q_div_p then 
		  [(x, mk_num (Q.minus q_div_p))]
		else 
		  raise Exc.Inconsistent
	  | _ ->
	      let (cl, xl) = vectorize ml in     (* [cl * xl = ml] in vector notation *)
		(match Euclid.solve cl (Q.minus q) with
		   | None -> 
		       raise Exc.Inconsistent
		   | Some(d, pl) -> 
		       let gl = general cl (d, pl) in
			 combine xl gl)
	     
and vectorize ml =
  let rec loop (ql, xl) = function
    | [] -> 
	(ql, xl)
    | m :: ml -> 
	let q = coefficient_of_mono m
	and x = variable_of_mono m in
	  loop (q :: ql, x :: xl) ml
  in
    loop ([], []) ml

and combine xl bl =
  List.combine xl bl


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
	  let sl' =  loop al' (e1 :: zl'') in
            e0 :: sl'
      | _ -> assert false
  in
    loop al (List.map mk_num pl)


let integer_solve = ref true

let rec solve e =
  if !integer_solve && is_diophantine_equation e then
    zsolve e
  else 
    try
      let e' = qsolve e in
	[e']
    with
	Exc.Valid -> []


(** Isolate [y] in an equality [a = b]. May raise [Not_found] if [y] does 
  not occur in [a - b]. *)
let rec isolate y ((a, b) as e) =
  assert(Term.subterm y (mk_sub a b));
  if Term.is_var a then
    if Term.eq y a then
      if Term.subterm y b then
	isolate_in_unsolved y e
      else 
	e
    else 
      if Term.subterm y b then
	isolate_in_unsolved y e
      else 
	isolate_in_solved y e
  else 
    isolate_in_unsolved y e
  
(** Isolate [y] in a solved equality [x = a]. *)
and isolate_in_solved y (x, a) = 
  assert (Term.subterm y a && not(Term.subterm x a));    
  let (p, pre, q, y, post) = destructure y a in 
    assert(not(Q.is_zero q)); (* [p + pre + q * y + post = x]. *)
    let b = mk_multq (Q.inv q) (mk_sub x (mk_addq p (mk_addl (pre @ post)))) in
      (y, b)             

and isolate_in_unsolved x ((a, b) as e) =
  assert(Term.subterm x a || Term.subterm x b);
  let a_sub_b = mk_sub a b in
    assert(not(is_num a_sub_b));
    let (p, pre, q, x, post) = destructure x a_sub_b in
      assert(not(Q.is_zero q));    (* [p + pre + q*x + post = 0] *)
      let c = mk_multq (Q.minus (Q.inv q)) 
		(mk_addq p (mk_addl (pre @ post))) 
      in
	(x, c)
    
   
(** Decompose [a] into [pre + q * y + post]. *)
and destructure y a =
  let rec loop pre post =     (* [pre + post = 0]. *)
    match post with
      | [m] ->
	  let q = coefficient_of_mono m
	  and y' = variable_of_mono m in
	    if Term.eq y y' then
	      (pre, q, y, [])
	    else 
	      raise Not_found
      | m :: post' ->
	  let q = coefficient_of_mono m
	  and y' = variable_of_mono m in
	    if Term.eq y y' then
	      (pre, q, y, post')
	    else 
	      loop (m :: pre) post'
      | [] ->
	  raise Not_found
  in      
  let p = constant_of a
  and ml = nonconstant_monomials_of a in
  let (pre, q, y, post) = loop [] ml in
    (p, pre, q, y, post)

