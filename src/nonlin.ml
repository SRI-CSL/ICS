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

module A = Linarith

let nl = Theory.create "nl" 
let theory = nl

let is_theory = Theory.eq nl

let _ = 
  Theory.Description.add nl
    "Nonlinear arithmetic
     Signature:
        a*b   : nonlinear multiplication
        inv(a): inverse
     Canonical forms:
        inv(a1*(a2*(...*an)...)) * (b1*(b2*(...*bm)...))
    "

type signature = Mult | Inv

(** Interpreted operations. *)
module Sig = struct
  let th = nl
  type t = signature
  let name =
    let mult = Name.of_string "*"
    and inv = Name.of_string "inv" in
      function
	| Mult -> mult
	| Inv -> inv
end 

module Op = Funsym.Make(Sig)

let mult = Op.inj Mult
let inv = Op.inj Inv
  
let mk_inv_zero () =
  let error = Term.mk_fresh_var "inv(0)" in
  let str = Name.to_string (Term.name_of error) in
    Format.eprintf "\nICS Warning: division by zero. Introducing variable %s\n@." str;
    error

let is_interp t =
  try Op.is_interp (Term.sym_of t) with Not_found -> false
      
let op a = Op.out (Term.sym_of a)
let args = Term.args_of

let is_mult t = 
  try (match op t with Mult -> true | _ -> false) with Not_found -> false

let is_inv t = 
  try (match op t with Inv -> true | _ -> false) with Not_found -> false

let lhs t =
  assert(is_mult t);
  Term.Args.get (Term.args_of t) 0

let rhs t =
  assert(is_mult t);
  Term.Args.get (Term.args_of t) 1

let denominator t =
  assert(is_inv t);
  Term.Args.get (Term.args_of t) 0

      
(** A {i power product} is of the form
  - [1] or
  - [x1*(x2*(...*xn)...)]
  with [xi] variables or other nonarithmetic terms. *)
module Pprod = struct

  module P = Ac.T(
    struct 
      let th = nl 
      let f = mult
    end)

  let is_interp = P.is_interp

  let mk_one = A.mk_one()
		 
  let is_one = Term.eq mk_one

  let is_pure a = is_one a || P.is_pure a

  let lhs t = 
    match op t with
      | Mult -> Term.Args.get (Term.args_of t) 0
      | Inv -> raise Not_found

  let rhs t = 
    match op t with
      | Mult -> Term.Args.get (Term.args_of t) 1
      | Inv -> raise Not_found

  let rec is_canonical t =
    is_one t ||
    (try
       (match op t with
	  | Mult -> is_canonical (lhs t) && is_canonical (rhs t)
	  | Inv -> false)
     with
	 Not_found -> not(A.is_interp t))

  let pp = P.pp

  let mk_mult t1 t2 =
    if is_one t1 then t2
    else if is_one t2 then t1 
    else P.make t1 t2

  (** Invariant: [hd t * tl t = t]. *)
  let hd t = try lhs t with Not_found -> t
  let tl t = try rhs t with Not_found -> mk_one
   
  let gcd = 
    let rec scan d s t =
      if is_one s || is_one t then d else
	let x = hd s and y = hd t in
	let cmp = Term.compare x y in
	  if cmp = 0 then scan (mk_mult x d) (tl s) (tl t) else 
	    if cmp < 0 then scan d (tl s) t else 
	      scan d s (tl t)
    in
      scan mk_one

  (** Least common multiple:
    - [lcm 1 t = t]
    - [lcm s 1 = s]
    - [lcm (x*s) (x*t) = x * lcm s t]
    - [lcm (x*s) (y*t) = x * lcm s (y*t)] if [x << y]
    - [lcm (x*s) (y*t) = y * lcm (x*s) t] if [x >> y]. *)
  let lcm =
    let rec scan m s t =
      if is_one s then mk_mult m t else 
	if is_one t then mk_mult m s else 
	  if Term.eq s t then mk_mult m s else 
	    let x = hd s and y = hd t in
	    let cmp = Term.compare x y in
	      if cmp = 0 then scan (mk_mult x m) (tl s) (tl t) else 
		if cmp < 0 then scan (mk_mult x m) (tl s) t else 
		  scan (mk_mult y m) s (tl t)
    in
      scan mk_one

  (** Cancel in [s] all occurrences of variables in [t].
    Only defined if [t] divides [s]. *)
  let mk_div =
    let rec scan d s t =
      if Term.eq s t then d else
	let y = hd t in
	  if is_one y then mk_mult d s else
	    (assert(not(is_one s));
	     let x = hd s in  (* drop [x]. *)
	       if Term.eq x y then scan d (tl s) (tl t) else
		 let d' = mk_mult x d in
		   scan d' (tl s) t)
    in
      scan mk_one
		
  let cancel ((s, t) as e) =
  let d = gcd s t in
    if is_one d then e else (mk_div s d, mk_div t d)

end 

(** A {i monomial} is of the form 
  - 1 or
  - [x1*...*xn] or
  - [inv(y1*...*ym)*(x1*...*xn)]
  with [xi], [yj] variables or other nonarithmetic terms. *)
module Monomial = struct

  let is_interp = is_interp

  let is_one = Pprod.is_one

  let lhs = Pprod.lhs
  let rhs = Pprod.rhs
  let denominator t =
    match op t with 
      | Inv -> Term.Args.get (Term.args_of t) 0
      | Mult -> raise Not_found

  let is_canonical t =
    try
      (match op t with
	 | Inv -> Pprod.is_canonical (denominator t)
	 | Mult -> 
	     let t1 = lhs t and t2 = rhs t in
	       (match op t1 with
		  | Inv -> Pprod.is_canonical (denominator t1)
		  | Mult -> Pprod.is_canonical t1) 
	       && Pprod.is_canonical t2)
    with
	Not_found -> is_one t || not(A.is_interp t)

  let rec is_pure t =
    try
      (match op t with
	 | Inv -> is_pure (denominator t)
	 | Mult -> is_pure (lhs t) && is_pure (rhs t))
    with
	Not_found -> Term.is_var t

  (** If [a] is of the form 
    - [inv(b) * c], then [destruct a = (c, b)],
    - [inv(b)], then [destruct a = (1, b)], and
    - [c], then [destruct a = (c, 1)]. *)
  let destruct t = 
    assert(is_canonical t);
    try
      (match op t with
	 | Inv  -> 
	     (Pprod.mk_one, denominator t)
	 | Mult ->
	     (match op (lhs t) with
		| Inv -> (rhs t, denominator (lhs t))
		| Mult -> (t, Pprod.mk_one)))
    with
	Not_found ->
	  (t, Pprod.mk_one)

  let is_pprod t =
    assert(is_canonical t);
    try
      (match op t with
	 | Inv  -> false
	 | Mult -> (match op (lhs t) with Inv -> false | Mult -> true))
    with
	Not_found -> true

  let crossmultiply ((a, b) as arg) = 
    assert(is_canonical a && is_canonical b);
    if is_pprod a && is_pprod b then arg else 
      let (na, da) = destruct a and (nb, db) = destruct b in
      let lcm = Pprod.lcm da db in
      let a' = Pprod.mk_mult na (Pprod.mk_div lcm da) in
      let b' = Pprod.mk_mult nb (Pprod.mk_div lcm db) in
	(a', b')
	    
  let pp fmt f a =
    assert(Op.is_interp f);
    match Op.out f with 
      | Mult -> 
	  Term.pp fmt (Term.Args.get a 0); 
	  Funsym.pp fmt f; 
	  Term.pp fmt (Term.Args.get a 1)
      | Inv -> 
	  Funsym.pp fmt f; 
	  Format.fprintf fmt "(";
	  Term.pp fmt (Term.Args.get a 0);
	  Format.fprintf fmt ")"

  let make n d =
    assert(Pprod.is_canonical n);
    assert(Pprod.is_canonical d);
    if Pprod.is_one d then n else 
      if Pprod.is_one n then Term.mk_unary inv d else 
	Term.mk_binary mult (Term.mk_unary inv d) n
	  
  let mk_inv t = 
    assert(is_canonical t);
    let n, d = destruct t in (* [t = n/d]. *)
      make d n
	    
  let mk_mult s t = 
    assert(is_canonical s && is_canonical t);
    if Pprod.is_one s then t else
      if Pprod.is_one t then s else
	let (ns, ds) = destruct s in
	let (nt, dt) = destruct t in   (* [(ns/ds) * (nt*dt)]. *)
	let n = Pprod.mk_mult ns nt in
	let d = Pprod.mk_mult ds dt in
	  if Term.eq n d then Pprod.mk_one else
	    let n, d = Pprod.cancel (n, d) in
	      make n d
      
  let map f =
    let rec mapf t =
      try
	(match op t with
	   | Mult  -> 
	       let t1 = lhs t and t2 = rhs t in
	       let t1' = mapf t1 and t2' = mapf t2 in
		 if t1 == t1' && t2 == t2' then t else mk_mult t1' t2'
	   | Inv -> 
	       let d = denominator t in
	       let d' = mapf d in
		 if d == d' then t else mk_inv d')
      with
	  Not_found -> f t
    in
      mapf
	
  let sigma f a = 
    assert(Op.is_interp f);
    match Op.out f with
      | Mult -> mk_mult (Term.Args.get a 0) (Term.Args.get a 1)
      | Inv -> mk_inv (Term.Args.get a 0)

end
		  
(** A {i nonlinear} term is built up from function symbols in [la] and [nl]. *) 
let is_interp t = 
  is_interp t || A.is_interp t
    
let is_pure t = 
  let is_monomial_pure m =
    assert(A.is_monomial m);
    Monomial.is_pure (A.var_of m)
  in
    A.for_all is_monomial_pure t
      
let is_canonical t =
  A.is_canonical t &&
  A.for_all Monomial.is_canonical t
    
let rec mk_mult s t =
  assert(is_canonical s);
  assert(is_canonical t);
  try mk_linarith_mult s t with Not_found -> 
    assert(not(A.is_interp s));
    (try mk_linarith_mult t s with Not_found -> 
       assert(not(A.is_interp t));
       Monomial.mk_mult s t)
    
and mk_linarith_mult s t = 
  match A.op s with
    | A.Sig.Num(q) -> 
	A.mk_multq q t
    | A.Sig.Multq(q) ->
	let x = Term.Args.get (Term.args_of s) 0 in
	  A.mk_multq q (mk_mult x t)
    | A.Sig.Add ->
	let a' = Term.Args.map (mk_mult t) (Term.args_of s) in
	  A.mk_addl (Term.Args.to_list a')
	    
and mk_multl tl =
  assert(List.for_all is_canonical tl);
  List.fold_left mk_mult (A.mk_one()) tl
    
and mk_expt t n =
  assert(is_canonical t);
  assert(n >= 0);
  if n = 0 then A.mk_one() else mk_mult t (mk_expt t (n - 1))
    
and mk_inv t =
  try
    (match A.op t with
       | A.Sig.Num(q) when Mpa.Q.is_zero q ->
	   mk_inv_zero()          (* division by zero *)
       | A.Sig.Num(q) -> 
	   A.mk_num (Mpa.Q.inv q)
       | A.Sig.Multq(q) ->
	   assert(not(Mpa.Q.is_zero q));
	   let x = Term.Args.get (Term.args_of t) 0 in
	     A.mk_multq (Mpa.Q.inv q) (mk_inv x)
       | A.Sig.Add ->
	   Term.mk_unary inv t)
  with
      Not_found ->
	assert(not(A.is_interp t));
	Monomial.mk_inv t
	  
and mk_div s t =
  assert(is_canonical s);
  assert(is_canonical t);
  if A.is_one t then s else
    let c = mk_mult s (mk_inv t) in
      assert(is_canonical c);
      c
	
(** Mapping a term transformer [f] over [a]. *)
let map = A.map
	    
(** Replace [x] by [b] in [a]. *)
let apply (x, b) = 
  let lookup y = if Term.eq x y then b else y in
    map lookup
      
let sigma f a = 
  assert(Op.is_interp f);
  match Op.out f with
    | Mult -> mk_multl (Term.Args.to_list a)
    | Inv -> mk_inv (Term.Args.get a 0)
	
let monomial_of m =
  assert(A.is_monomial m);
  A.var_of m
    
let coeff_of m =
  assert(A.is_monomial m);
  A.coeff_of m
    
let is_nonneg t = Three.X

let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.printer <- Some(Monomial.pp);
    m.Term.Methods.can <- Some(sigma);
    m.Term.Methods.is_diseq <- None;
    m.Term.Methods.is_nonneg <- Some(is_nonneg);
    Term.Methods.register nl m
