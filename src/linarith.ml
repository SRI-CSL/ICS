(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

open Mpa

let theory = Theory.create "la"

let is_theory = Theory.eq theory

let _ = 
  Theory.Description.add theory
    "Theory of linear arithmetic (mixed rational and integer)."

module Sig = struct

  let th = theory

  type t =
    | Num of Q.t
    | Multq of Q.t
    | Add

  let to_string = function
    | Num(q) -> Q.to_string q
    | Multq(q) -> Format.sprintf "%s*" (Q.to_string q)
    | Add -> " + "

  let name op = 
    Name.of_string (to_string op)
end

module Op = Funsym.Make(Sig)

(** Returns interpreted operation symbol or raises [Not_found]. *)
let op a = Op.out (Term.sym_of a)

let args a = 
  Term.Args.to_list (Term.args_of a)

let pp fmt f al =
  assert(Op.is_interp f);
  match Op.out f, Term.Args.to_list al with
    | Sig.Num(q), [] -> Q.pp fmt q
    | Sig.Multq(q), [a] -> Q.pp fmt q; Format.fprintf fmt "*"; Term.pp fmt a
    | Sig.Add, al -> Pretty.infixl Term.pp " + " fmt al
    | _ -> invalid_arg "Linarith.pp: invalid term"

let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false

let rec is_pure t =
  try
    (match op t with
       | Sig.Num(q) -> true
       | Sig.Multq(q) -> Term.is_var (Term.arg1 t)
       | Sig.Add -> Term.Args.for_all is_pure (Term.args_of t))
  with
      Not_found -> Term.is_var t

let is_monomial m = 
  try
    (match op m with
      | Sig.Multq _ -> not(is_interp (Term.arg1 m))
      | _ -> false)
  with
      Not_found -> true

let coeff_of m =
  assert(is_monomial m);
  try 
    (match op m with Sig.Multq(q) -> q | _ -> Q.one)
  with
      Not_found -> Q.one

let var_of m =
  assert(is_monomial m);
  try
    (match op m with Sig.Multq _ -> Term.arg1 m | _ -> m)
  with
      Not_found -> m
 
(** Hashconsing of numeric constants. *)
let mk_num =
  let table = Q.Hash.create 17 in
  let _ =  Tools.add_at_reset (fun () -> Q.Hash.clear table) in
    fun q ->
      try Q.Hash.find table q with Not_found ->
	let c = Term.mk_const (Op.inj(Sig.Num(q))) in
	  Q.Hash.add table q c; c


(** Following constants need a parameter. *)
let mk_zero () = mk_num Q.zero
let mk_one () = mk_num Q.one
let mk_two () = mk_num (Q.of_int 2)

let d_num t =
  match op t with
    | Sig.Num(q) -> q
    | _ -> raise Not_found
	
let d_add t =
  match op t with
    | Sig.Add -> args t
    | _ -> raise Not_found
	
let d_multq t =
  match op t with
    | Sig.Multq(q) -> (q, Term.arg1 t)
    | _ -> raise Not_found

let is_multq t =
  try
    (match op t with
       | Sig.Multq _ -> true
       | _ -> false)
  with
      Not_found -> false
	
let is_q q t =
  try Q.equal (d_num t) q with Not_found -> false
	
let is_zero = is_q Q.zero
let is_one = is_q Q.one
	       
let is_num t =
  try let _ = d_num t in true with Not_found -> false

let rec is_canonical a =
  try
    (match op a, args a with
       | Sig.Num _, [] -> true
       | Sig.Multq _, [x] -> not(is_interp x)
       | Sig.Add, m :: ml when is_num m -> is_ordered_monomials ml
       | Sig.Add, ((_::_::_) as ml) -> is_ordered_monomials ml
       | _ -> false)
  with
      Not_found -> true

and is_ordered_monomials ml = 
  match ml with
    | [] -> true
    | [m] -> is_monomial m
    | m1 :: ((m2 :: _) as ml') -> 
	is_monomial m1 &&
	(Term.compare (var_of m1) (var_of m2) > 0) && 
	is_ordered_monomials ml'

(** The constant part [q] of a polynomial [q + t]. *)
let constant_of t =
  try
    (match op t with
       | Sig.Num(q) -> q
       | Sig.Add -> d_num (Term.arg1 t)
       | Sig.Multq _ -> Q.zero)
  with
      Not_found -> Q.zero
	
	
(** The nonconstant part [a1 + ... + an] of a 
  polynomial [q + a1 + ... + an] as a list of monomials *)
let monomials_of a =
  try
    (match op a, args a with
       | Sig.Num(_), [] -> []
       | Sig.Add, a' :: al' when is_num a' -> al'
       | Sig.Add, al -> al
       | _ -> [a])
  with
      Not_found -> [a]

let nonconstant_of a =
  try
    (match op a, args a with
       | Sig.Num(_), [] -> 
	   mk_zero() 
       | Sig.Add, [a'; a''] when is_num a' ->
	   a''
       | Sig.Add, (a' :: al') when is_num a' -> 
	   assert(List.length al' >= 2);
	   Term.mk_app (Op.inj(Sig.Add)) (Term.Args.of_list al')
       | _ -> a)
  with
      Not_found -> a

(** Cheap test for disequality holds if [a] is of the form [q + a']
  and [b] is of the form [p + b'] and [q <> p] and [a'] is equal to b']. *)
let rec is_diseq a b =
  not (Q.equal (constant_of a) (constant_of b)) &&
  monomials_eq a b
    
and monomials_eq t1 t2 =
  try List.for_all2 Term.eq (monomials_of t1) (monomials_of t2) with _ -> false
    
(** - [q + a' <= p + b'] iff [q <= p] and [a' = b']
    - [q + a' > p + b'] iff [q > p] and [a' = b']. *)
let is_le a b =
  let q = constant_of a and p = constant_of b in
    if Q.le q p then
      if monomials_eq a b then Three.Yes else Three.X
    else if Q.gt q p then
      if monomials_eq a b then Three.No else Three.X
    else
      Three.X

let is_ge a b = is_le b a


(** [a >= 0] iff [a] is a nonnegative number. *)
let is_nonneg a =
  try
    let q = d_num a in
      if Q.is_nonneg q then Three.Yes else Three.No
  with
      Not_found -> Three.X


(** [a > 0] iff [a] is a positive number. *)
let is_pos a =
  try
    let q = d_num a in
      if Q.is_pos q then Three.Yes else Three.No
  with
      Not_found -> Three.X


(** Create a polynomial with constant part [q] and 
  monomials [ml]. Assumes [ml] is canonical. *)
let mk_polynomial =
  let add = Term.mk_app (Op.inj(Sig.Add)) in
    fun q ml ->
      assert(is_ordered_monomials ml);
      let m = if  Q.is_zero q then ml else mk_num q :: ml in
	match m with 
	  | [] -> mk_zero()
	  | [x] -> x
	  | _ -> add (Term.Args.of_list m)

let mk_monomial q x =
  assert(not(is_interp x));
  if Q.is_zero q then mk_zero() else
    if Q.is_one q then x else 
      try 
	let p = d_num x in
	  mk_num (Q.mult q p)
      with
	  Not_found -> 
	    Term.mk_unary (Op.inj(Sig.Multq(q))) x

let rec iter f a =
  if is_interp a then
    List.iter (iter f) (args a)
  else 
    f a

let rec fold f a = 
  if is_interp a then
    List.fold_right (fold f) (args a)
  else 
    f a

let rec for_all p a =
  if is_interp a then
    List.for_all (for_all p) (args a)
  else
    p a

let coefficient_of x a = 
  let rec search = function
    | [] -> raise Not_found
    | m :: ml -> if Term.eq x (var_of m) then coeff_of m else search ml
  in
    search (monomials_of a)
	
let foldq f a e =
  let init = f (constant_of a) e in
  let step acc m = f (coeff_of m) acc in
    List.fold_left step init (monomials_of a)

let lcm_of_denominators a =
  let lcm q acc =
    if Q.is_zero q then acc else Z.lcm (Q.denominator q) acc
  in
    foldq lcm a Z.one
 
let gcd_of_denominators a =
  let gcd q acc = 
    if Q.is_zero q then acc else Z.gcd (Q.denominator q) acc
  in
    foldq gcd a Z.one

let rec mk_apply1 op l =
  assert(is_ordered_monomials l);
  List.fold_right
    (fun m acc ->
       let p' = op (coeff_of m) in
	 if Q.is_zero p' then acc 
	 else if Q.is_one p' then m :: acc
	 else mk_monomial p' (var_of m) :: acc)
    l [] 

and multq_monomials q ml =
  assert(List.for_all is_monomial ml);
  if Q.is_zero q then [] 
  else if Q.is_one q then ml else
    List.map
      (fun m ->
	 mk_monomial (Q.mult q (coeff_of m)) (var_of m))
      ml

and mk_multq q a =
  assert(is_canonical a);
  if Q.is_zero q then mk_zero()
  else if Q.is_one q then a else
    mk_polynomial
      (Q.mult q (constant_of a))
      (multq_monomials q (monomials_of a))

and mk_addq q a =
  assert(is_canonical a);
  if Q.is_zero q then a else
    mk_polynomial 
      (Q.add q (constant_of a))
      (monomials_of a)

and add_monomials l1 l2 =
  assert(is_ordered_monomials l1);
  assert(is_ordered_monomials l2);
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' ->
	let cmp = Term.compare (var_of m1) (var_of m2) in
	  if cmp = 0 then
	    let q = Q.add (coeff_of m1) (coeff_of m2) in
	      if Q.is_zero q then add_monomials l1' l2' else 
		mk_monomial q (var_of m1) :: (add_monomials l1' l2')
	  else if cmp < 0 then
	    m2 :: add_monomials l1 l2'
	  else (* cmp > 0 *)
	    m1 :: add_monomials l1' l2
		
and mk_add t1 t2 =
  assert(is_canonical t1 && is_canonical t2);
  let t =
    mk_polynomial 
      (Q.add (constant_of t1) (constant_of t2))
      (add_monomials (monomials_of t1) (monomials_of t2))
  in
    assert(is_canonical t);
    t
  
and sub_monomials l1 l2 =
  assert(is_ordered_monomials l1);
  assert(is_ordered_monomials l2);
  match l1, l2 with
    | [], _ -> List.map neg_monomial l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' ->
	let cmp = Term.compare (var_of m1) (var_of m2) in
	  if cmp = 0 then
	    let q = Q.sub (coeff_of m1) (coeff_of m2) in
	      if Q.is_zero q then sub_monomials l1' l2' else 
		mk_monomial q (var_of m1) :: (sub_monomials l1' l2')
	  else if cmp < 0 then
	    neg_monomial m2 :: sub_monomials l1 l2'
	  else (* cmp > 0 *)
	    m1 :: sub_monomials l1' l2

and neg_monomial m =
  assert(is_monomial m);
  let q = coeff_of m and x = var_of m in
    mk_monomial (Q.minus q) x

and mk_sub a b =
  assert(is_canonical a && is_canonical b);
  if Term.eq a b then mk_zero() else
    if is_zero b then a else
      let c = 
	mk_polynomial 
	  (Q.sub (constant_of a) (constant_of b))
	  (sub_monomials (monomials_of a) (monomials_of b))
      in
	assert(is_canonical c);
	c

and mk_addl l =
  match l with
    | [] -> mk_zero()
    | [x] -> x
    | [x; y] -> mk_add x y
    | [x; y; z] -> mk_add x (mk_add y z)
    | x :: xl -> mk_add x (mk_addl xl)
 
and mk_incr a =
  assert(is_canonical a);
  mk_polynomial 
    (Q.add (constant_of a) Q.one)
    (monomials_of a)

and mk_decr a =
  assert(is_canonical a);
  mk_polynomial 
    (Q.sub (constant_of a) Q.one)
    (monomials_of a)

and mk_neg a =
  assert(is_canonical a);
  mk_multq Q.negone a

let mapq f a =
  assert(is_canonical a);
  let q' = f (constant_of a) in
  let ml' = mk_apply1 f (monomials_of a) in
    mk_polynomial q' ml'

let mk_add a b =
  Format.eprintf "\nA.add <- %s %s@." (Term.to_string a) (Term.to_string b);
  let c = mk_add a b in
    Format.eprintf "\nA.add -> %s@." (Term.to_string c);
    c

let map f =
  let rec mapf a = 
    try
      (match op a, args a with
	 | Sig.Num _, [] -> 
	     a
	 | Sig.Multq(q), [x] ->
	     let x' = mapf x in
	       if x == x' then a else mk_multq q x'
	 | Sig.Add, [x; y] -> 
	     let x' = mapf x and y' = mapf y in
	       if x == x' && y == y' then a else mk_add x' y'
	 | Sig.Add, xl -> 
	     let xl' = List.map mapf xl in
	       if xl == xl' then a else mk_addl xl'
	 | _ -> 
	     assert false)
    with
	Not_found -> f a
  in
    mapf 

(** Replacing a variable with a term. *)
let apply x b =
  let lookup y = if Term.eq x y then b else y in
    map lookup

(** Interpretation. *)
let eval f a =
  let of_add ml =
    List.fold_left
      (fun acc m -> 
	 try 
	   d_num m 
	 with 
	     Not_found -> 
	       assert(is_monomial m);
	       let v = Q.mult (coeff_of m) (f (var_of m)) in
		 Q.add acc v)
      Q.zero ml
  in
    try
      (match op a, args a with
	 | Sig.Num(q), [] -> q
	 | Sig.Multq(q), [x] -> Q.mult q (f x)
	 | Sig.Add, ml -> of_add ml
	 | _ -> invalid_arg "Linarith.interp")
    with
	Not_found -> f a

(** Interface for canonizing arithmetic terms. *)
let rec sigma f a =
  assert(Op.is_interp f);
  match Op.out f with
    | Sig.Num(q) -> mk_num q
    | Sig.Add when Term.Args.length a = 2 -> mk_add (Term.Args.get a 0) (Term.Args.get a 1)
    | Sig.Add -> mk_addl (Term.Args.to_list a)
    | Sig.Multq(q) -> mk_multq q (Term.Args.get a 0)

(** Multiply with a constant so that all coefficients become integer.*)
let integerize a =
  let lcm = Q.of_z (lcm_of_denominators a) in
  let a' = if Q.is_one lcm then a else mk_multq lcm a in
  let gcd = Q.of_z (gcd_of_denominators a') in
  let a'' = if Q.is_one gcd then a' else mk_multq gcd a' in
    (a'', Q.mult lcm gcd)

(** Test if [a] is interpreted over the integers. Might throw [Not_found]. *)
let is_int of_term = 
  let rec is_integer t =
    match op t with
      | Sig.Num(q) -> Mpa.Q.is_integer q 
      | Sig.Multq(q) -> Mpa.Q.is_integer q && is_integer (Term.arg1 t)
      | Sig.Add -> Term.Args.for_all is_integer (Term.args_of t)
  in
    is_integer

(** Constraint of a term. *)
let rec cnstrnt of_term a =
  try
    let f = Term.sym_of a and al = Term.args_of a in
      if Op.is_interp f then
	tau of_term f al
      else
	Cnstrnt.Real
  with
      Not_found -> Cnstrnt.Real

and tau of_term f al =
  assert(Op.is_interp f);
  let of_q q =
    if Mpa.Q.is_integer q then Cnstrnt.Int else Cnstrnt.Nonint
  in
    try
      (match Op.out f, Term.Args.to_list al with
	 | Sig.Num(q), [] -> 
	     of_q q
	 | Sig.Multq(q), [a] -> 
	     (match of_q q, of_term a with
		| Cnstrnt.Int, Cnstrnt.Int -> Cnstrnt.Int
		| _ -> Cnstrnt.Real)    
	 | Sig.Add, [a; b] -> 
	     (match of_term a, of_term b with
		| Cnstrnt.Int, Cnstrnt.Int -> Cnstrnt.Int
		| _ -> Cnstrnt.Real)
	 | Sig.Add, al ->
	     if List.for_all (is_int of_term) al then Cnstrnt.Int else Cnstrnt.Real
	 | _ -> 
	     Cnstrnt.Real)
    with
	Not_found -> Cnstrnt.Real


(** Decompose [a] into [p + pre + q * y + post], where
  [pre] and [post] are ordered lists of monomials with
  all monomials in [pre] larger than any of [post]. *)
let destructure y a =
  let rec loop pre post =     (* [pre + post = 0]. *)
    match post with
      | [m] ->
	  if Term.eq y (var_of m) then 
	    (pre, coeff_of m, y, []) 
	  else 
	    raise Not_found
      | m :: post' ->
	  if Term.eq y (var_of m) then 
	    (pre, coeff_of m, y, post') 
	  else 
	    loop (m :: pre) post'
      | [] ->
	  raise Not_found
  in      
  let p = constant_of a and ml = monomials_of a in
  let (pre, q, y, post) = loop [] ml in
    (p, List.rev pre, q, y, post)

(** Solve [a = 0] in the rationals. *)
let rec qsolve a =
  let p = constant_of a in
    match monomials_of a with
      | [] -> 
	  raise (if Q.is_zero p then Exc.Valid else Exc.Inconsistent)
      | [m] ->                             (* [p + q * x = 0] *)
	  let q = coeff_of m and x = var_of m in
	    (x, mk_num (Q.div (Q.minus p) q))
      | m :: ml ->                         (* [p + q * x + ml = 0] *)
	  let q = coeff_of m and x = var_of m in   
	  let b = 
	    mk_polynomial
	      (Q.minus (Q.div p q))
	      (multq_monomials (Q.minus (Q.inv q)) ml)
	  in
	    if Term.is_var b then Term.orient (x, b) else (x, b)


let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.printer <- Some(pp);
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- Some(is_diseq);
    m.Term.Methods.is_nonneg <- Some(is_nonneg);
    (let solve a b =
       let x, c = qsolve (mk_sub a b) in
	 Term.Subst.singleton x c
     in
       m.Term.Methods.solve <- Some(solve));
    Term.Methods.register theory m

	      
(** Solve [a = 0] for [x]. *)
let rec qsolve_for x a =               (* [p + pre + q * x + post = 0]. *)
  let (p, pre, q, x', post) = destructure x a in 
    assert(not(Q.is_zero q)); 
    assert(Term.eq x x');
    let neginvq = Q.minus (Q.inv q) in (* ==> [x = -p/q + -1/q(pre + post)]   *)
    let pre' = multq_monomials neginvq pre
    and post' = multq_monomials neginvq post in
      mk_polynomial (Q.mult p neginvq) (pre' @ post')
 
let rec qsolve_solved_for x (y, a) =            
  assert(not(is_interp y));
  assert(not(Term.occurs y a));
  assert(Term.occurs x a);
  let (p, pre, q, x', post) = destructure x a in 
    assert(not(Q.is_zero q));          (* [y = p + pre + q * x + post]. *)
    assert(Term.eq x x');
    let invq = Q.inv q in
    let neginvq = Q.minus invq in     (* ==> [x = -p/q + -1/q*y -1/q(pre + post)] *)
    let pre' = multq_monomials neginvq pre
    and post' = multq_monomials neginvq post in
    let m' = mk_multq invq y in
    let ml' = add_monomials [m'] (pre' @ post') in
      mk_polynomial (Q.mult p neginvq) ml'

let fresh = ref (Term.Set.empty ())

let mk_fresh () =
  let x = Term.mk_fresh_var "k" in
    Term.Set.add x !fresh;
    x

let is_fresh x =
  Term.Set.mem x !fresh

(** Solve [a = 0] over the integers if there is a variable with unitary coefficient. *)
exception Found of Term.t
let choose_unitary a =
  let choose m = 
    let q = coeff_of m in
      if Q.is_one q || Q.is_negone q then
	raise(Found(var_of m))
  in
    try
      List.iter choose (monomials_of a);
      raise Not_found
    with
	Found(x) -> x

let zsolve_unitary x a =
  let (p, pre, q, x, post) = destructure x a in  (* [a = p + pre + q*x + post = 0]. *)
  let b = mk_multq (Q.minus (Q.inv q))           (* ==> [x = -1/q(p + pre + post)].*)
            (mk_polynomial p (pre @ post))
  in
    (x, b)
  
(** Euclidean solver for particular solutions. *)
let euclid = 
  let module Euclid = Euclid.Make(
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
  in
    Euclid.solve
    

(** Solve [a = 0] in the integers. *)
let rec zsolve a = 
  fresh := Term.Set.empty ();       (* reinitialize fresh variable generation. *)
  if is_interp a then
    try
      let x = choose_unitary a in
      let y, b = zsolve_unitary x a in
	Term.Set.empty(), Term.Subst.singleton y b
    with
	Not_found -> 
	  let sl = zsolve0 a in
	    !fresh, Term.Subst.of_list sl
  else 
    Term.Set.empty(), Term.Subst.singleton a (mk_zero())

and zsolve0 a =
  let q = constant_of a in
    match monomials_of a with
      | [] -> 
	  if Q.is_zero q then [] else raise Exc.Inconsistent
      | [m] ->                           (* [q + p*x = 0] *)
	  let q_div_p = Q.div q (coeff_of m) in
	    if Q.is_integer q_div_p then 
	      [(var_of m, mk_num (Q.minus q_div_p))]
	    else 
	      raise Exc.Inconsistent
      | ml ->
	  let (cl, xl) = vectorize ml in     (* [cl * xl = ml] in vector notation *)
	    (match euclid cl (Q.minus q) with
	       | None -> 
		   raise Exc.Inconsistent
	       | Some(d, pl) -> 
		   let gl = general cl (d, pl) in
		     combine xl gl)
	    
and vectorize ml =
  let rec loop (ql, xl) = function
    | [] -> (ql, xl)
    | m :: ml -> loop (coeff_of m :: ql, var_of m :: xl) ml
  in
    loop ([], []) ml

and combine xl bl =
  let rec loop acc xl bl =
    match xl, bl with
      | [], [] -> 
	  acc
      | x :: xl', b :: bl' -> 
	  if is_fresh b then
	    let acc' = fuse acc (b, x)           (* Rename. *)
	    and bl'' = List.map (apply b x) bl' in
	      loop acc' xl' bl''
	  else 
	    loop ((x, b) :: acc) xl' bl'
      | _ ->
	  invalid_arg "Linarith.combine"
  in
    loop [] xl bl

(** Fuse variable equalities [la = x] with [la] a fresh variable into rhs. *)
and fuse sl (x, y) =
  assert(is_fresh x);
  let rec loop acc = function
    | [] -> acc
    | ((y, a) as e) :: el -> 
	let a' = apply x y a in
	let acc' = if a == a' then e :: acc else (y, a') :: acc in
	  loop acc' el
  in
    loop [] sl

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


module Atom = struct

  (* [a <= b] iff [b - a >= 0] *)
  let mk_le a b = Atom.mk_nonneg (mk_sub b a)

  let mk_ge a b = mk_le b a

  (* [a<b] iff [b-a > 0] *)
  let mk_lt a b = Atom.mk_pos (mk_sub b a)
	
  let mk_gt a b = mk_lt b a

  (* [a == b mod m] iff [a = b + k*m] with [k] a integer. *)
  let mk_modeq a b n = 
    let k = Term.mk_fresh_var "k" in
    let m = Mpa.Q.of_z n in
      [Atom.mk_cnstrnt k Cnstrnt.Int;
       Atom.mk_equal a (mk_add b (mk_multq m k))]

end


(** {6 Monomials} *)

(** For arithmetic terms [a] we use the notation 
  - [|a|] for the constant monomial, 
  - [a+] for the positive monomials, and 
  - [a-] for the negative monomials in [a]. *)
module Monomials = struct

  type sign = Pos | Neg | All

 (** [is_empty Pos a] iff [a+] is empty. *)
  let is_empty tag a =
    let p = match tag with Pos -> Q.is_neg | Neg -> Q.is_pos | All -> fun _ -> false in
    let holds m = p (coeff_of m) in
      List.for_all holds (monomials_of a)

  let qtest = function
    | Pos -> Q.is_pos 
    | Neg -> Q.is_neg
    | All -> fun _ -> true

 (** [is_neg_mem x a] iff [x] is in [vars(a-)]. *)
  let is_mem tag x a =
    let p = qtest tag in
    let holds m = Term.eq x (var_of m) && p (coeff_of m) in
      List.for_all holds (monomials_of a)
   
  (** [iter Pos f a] applies [f] for all [x] in [vars(a+)] *)
  let iter tag f a =
    let p = qtest tag in
    let f' m = 
      assert(is_monomial m);
      if p (coeff_of m) then f (var_of m)
    in
      List.iter f' (monomials_of a)
	
  (** Choose least variable in arithmetic term [a] or raise [Not_found. *)
  let choose a = 
    match monomials_of a with
      | [] -> raise Not_found
      | m :: _ -> 
	  assert(Term.is_var (var_of m));
	  var_of m

  let exists tag p a =
    let qtest = qtest tag in
    let p' m = qtest (coeff_of m) && p (var_of m) in
      List.exists p' (monomials_of a)

  let for_all tag p a =
    let qtest = qtest tag in
    let p' m = qtest (coeff_of m) && p (var_of m) in
      List.for_all p' (monomials_of a)

  (** Minimal variable in [a] with respect to [cmp]. *)
  let choose_min cmp a =
    match monomials_of a with
      | [] -> 
	  raise Not_found
      | m :: ml -> 
	  assert(is_monomial m);
	  let min = ref (var_of m) in
	    List.iter
	      (fun m -> 
		 assert(is_monomial m);
		 let x = var_of m in
		   if cmp x !min < 0 then
		     min := x)
	      ml;
	    !min
end
