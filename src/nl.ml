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

(** Interpreted operations. *)
module Sig = struct
  let th = nl
  type t = Mult | Inv
  let name =
    let mult = Name.of_string "*"
    and inv = Name.of_string "inv" in
      function
	| Mult -> mult
	| Inv -> inv
end 

module Op = Funsym.Make(Sig)

let mult = Op.inj Sig.Mult
let inv = Op.inj Sig.Inv
  
let mk_inv_zero () =
  let error = Term.mk_fresh_var "inv(0)" in
  let str = Name.to_string (Term.name_of error) in
    Format.eprintf "\nICS Warning: division by zero. Introducing variable %s\n@." str;
    error

let is_interp a =
  try Op.is_interp (Term.sym_of a) with Not_found -> false
      
let op a = Op.out (Term.sym_of a)
let args = Term.args_of
      
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
      | Sig.Mult -> Term.Args.get (Term.args_of t) 0
      | Sig.Inv -> raise Not_found

  let rhs t = 
    match op t with
      | Sig.Mult -> Term.Args.get (Term.args_of t) 1
      | Sig.Inv -> raise Not_found

  let rec is_canonical t =
    is_one t ||
    (try
       (match op t with
	  | Sig.Mult -> is_canonical (lhs t) && is_canonical (rhs t)
	  | Sig.Inv -> false)
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
      | Sig.Inv -> Term.Args.get (Term.args_of t) 0
      | Sig.Mult -> raise Not_found

  let is_canonical t =
    try
      (match op t with
	 | Sig.Inv -> Pprod.is_canonical (denominator t)
	 | Sig.Mult -> 
	     let t1 = lhs t and t2 = rhs t in
	       (match op t1 with
		  | Sig.Inv -> Pprod.is_canonical (denominator t1)
		  | Sig.Mult -> Pprod.is_canonical t1) 
	       && Pprod.is_canonical t2)
    with
	Not_found -> is_one t || not(A.is_interp t)

  let rec is_pure t =
    try
      (match op t with
	 | Sig.Inv -> is_pure (denominator t)
	 | Sig.Mult -> is_pure (lhs t) && is_pure (rhs t))
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
	 | Sig.Inv  -> 
	     (Pprod.mk_one, denominator t)
	 | Sig.Mult ->
	     (match op (lhs t) with
		| Sig. Inv -> (rhs t, denominator (lhs t))
		| Sig.Mult -> (t, Pprod.mk_one)))
    with
	Not_found ->
	  (t, Pprod.mk_one)

  let is_pprod t =
    assert(is_canonical t);
    try
      (match op t with
	 | Sig.Inv  -> false
	 | Sig.Mult -> (match op (lhs t) with Sig.Inv -> false | Sig.Mult -> true))
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
      | Sig.Mult -> 
	  Term.pp fmt (Term.Args.get a 0); 
	  Funsym.pp fmt f; 
	  Term.pp fmt (Term.Args.get a 1)
      | Sig.Inv -> 
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
	   | Sig.Mult  -> 
	       let t1 = lhs t and t2 = rhs t in
	       let t1' = mapf t1 and t2' = mapf t2 in
		 if t1 == t1' && t2 == t2' then t else mk_mult t1' t2'
	   | Sig.Inv -> 
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
      | Sig.Mult -> mk_mult (Term.Args.get a 0) (Term.Args.get a 1)
      | Sig.Inv -> mk_inv (Term.Args.get a 0)

end
		  

(** A {i nonlinear} term is built up from function symbols 
  in [la] and [nl]. *)
module Nonlin = struct  

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
      | Sig.Mult -> mk_multl (Term.Args.to_list a)
      | Sig.Inv -> mk_inv (Term.Args.get a 0)

  let monomial_of m =
    assert(A.is_monomial m);
    A.var_of m

  let coeff_of m =
    assert(A.is_monomial m);
    A.coeff_of m

  let is_nonneg t = Three.X

end

let _ = 
  let m = Term.Methods.empty() in
    m.Term.Methods.printer <- Some(Monomial.pp);
    m.Term.Methods.can <- Some(Nonlin.sigma);
    m.Term.Methods.is_diseq <- None;
    m.Term.Methods.is_nonneg <- Some(Nonlin.is_nonneg);
    Term.Methods.register nl m


(** Flat terms are of the form [x*y] with [x], [y] variables. *)
module Flat = struct

  let is t = 
    try
      (match op t with
	 | Sig.Mult -> 
	     let a = Term.args_of t in
	       Term.is_var (Term.Args.get a 0) && 
	       Term.is_var (Term.Args.get a 1)
	 | _ -> false)
    with
	Not_found -> false

  let lhs t =
    assert(is t);
    match op t with Sig.Mult -> Term.Args.get (Term.args_of t) 0 | _ -> assert false

  let rhs t =
    assert(is t);
    match op t with Sig.Mult -> Term.Args.get (Term.args_of t) 01| _ -> assert false

  let can f a =
    assert(Op.is_interp f);
    Pprod.mk_mult (Term.Args.get a 0) (Term.Args.get a 1)

  let map f t =
    assert(is t);
    let x = lhs t and y = rhs t in
    let x' = f x and y' = f y in
      if x == x' && y == y' then t else Pprod.mk_mult x' y'

end


(** Inference system for AC terms with cancellation laws. *)
module Ac = Can.Infsys(
  struct

    let th = nl
    let can = Flat.can
    let map = Flat.map

    open Axioms  

    let mk_lapp a b = Lterm.mk_app  (Op.inj Sig.Mult) [a; b]

    (** [x * (y * z) = (x * y) * z]. *)
    let assoc = 
      let x = Lterm.mk_var "x" in
      let y = Lterm.mk_var "y" in
      let z = Lterm.mk_var "z" in
	Chain.mk_equal 
	  (Name.of_string "nl.assoc")
	  [] 
	  (mk_lapp x (mk_lapp y z)) 
	  (mk_lapp (mk_lapp x y) z)

    (** [x * y1 = x * y2 ==> y1 = y2]. *)
    let lcancel =
      let x = Lterm.mk_var "x" in
      let y1 = Lterm.mk_var "y1" in
      let y2 = Lterm.mk_var "y2" in
	Chain.mk_equal  
	  (Name.of_string "nl.lcancel")
	  [Atom.mk_equal (mk_lapp x y1) (mk_lapp x y2)]
	  y1 y2

    (** [y1 * x = y2 * x ==> y1 = y2]. *)
    let rcancel =
      let x = Lterm.mk_var "x" in
      let y1 = Lterm.mk_var "y1" in
      let y2 = Lterm.mk_var "y2" in
	Chain.mk_equal 
          (Name.of_string "nl.rcancel")
	  [Atom.mk_equal (mk_lapp y1 x) (mk_lapp y2 x)]
	  y1 y2
	
    let chains = 
      [assoc; lcancel; rcancel]
      
    let disjunction _ = raise Not_found
      
  end)

module J = Judgement


(** Various deductions from equalities [x = y*z]. *)
module Deduce = struct

  let linearize t = failwith "to do"

  let all e = failwith "to do"

end 

(*
module Deduce = struct

  open Cnstrnt

  let dom e =
    assert(Term.is_var e#lhs);
    e#lhs

  let cod e =
    assert(Flat.is e#rhs);
    e#rhs
      
  (** Deducing new constraints from an equality [x = y * z] using
    abstract constraint interpretation; for example:
    - [int = int * X] ==> [X = int]
    - [real = real * X] ==> [X = real]. *)
  let cnstrnts e =
    let x = dom e and a = cod e in
    let cnstrnt z =
      assert(Term.is_var z);
      match V.Infsys.cnstrnt z with
	| Some(c) -> 
	    if Cnstrnt.is_real c#cnstrnt then c else
	      raise(Jst.Inconsistent(failwith "to do"))
	| None ->
	    V.Infsys.process_cnstrnt (x, Real, rho);
	    Real, rho
    in  
    let y = Flat.lhs a  and z = Flat.rhs a in   (* [rho |- x = y*z] *)
    let c, gamma = cnstrnt x          (* [gamma |- x in c]. *)               
    and d, delta = cnstrnt y          (* [delta |- y in d]. *)
    and e, eps = cnstrnt z in         (* [eps |- z in e]. *)
      match c, d, e with
	| Int, Int, Nonint -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Int, Nonint, Int -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Nonint, Int, Int -> 
	    raise(Jst.Inconsistent(Jst.dep4 rho gamma delta eps))
	| Int, Int, Real -> 
	    V.Infsys.process_cnstrnt (z, Int, Jst.dep3 rho gamma delta)
	| Int, Real, Int -> 
	    V.Infsys.process_cnstrnt (y, Int, Jst.dep3 rho gamma eps)
	| Real, Int, Int -> 
	    V.Infsys.process_cnstrnt (x, Int, Jst.dep3 rho delta eps)
	| _  ->
	    ()
	    
  (** Deducing new nonnegativity constraints from an equality [x = y * z]
    - [nonneg = nonneg * Z] implies [Z = nonneg]
    -  [nonneg = Y * nonneg] implies [Y = nonneg]
    -  [X = nonneg * nonneg] implies [X = nonneg]. *)
  let nonneg x (a, rho) =
    assert(Term.is_var x && Flat.is a);     
    let is_nonneg z = 
      failwith "to do"
      (* try Some(Simplex.jst_of_nonneg_var z) with Not_found -> None  *)
    in  
    let y = Flat.lhs a and z = Flat.rhs a in       (* [rho |- x = y*z]. *)
      match is_nonneg x, is_nonneg y, is_nonneg z with
	| None, Some(tau2), Some(tau3) ->
	    Simplex.Infsys.process_nonneg (x, Jst.dep3 rho tau2 tau3)
	| Some(tau1), None, Some(tau3) ->
	    Simplex.Infsys.process_nonneg (y, Jst.dep3 rho tau1 tau3)
	| Some(tau1), Some(tau2), None ->
	    Simplex.Infsys.process_nonneg (z, Jst.dep3 rho tau1 tau2)
	| _ ->
	    ()

  let rec linarith x (a, rho) = 
    assert(Term.is_var x && Flat.is a);
    Trace.call 5 "Nl.deduce(la)" (x, a, rho) Jst.Equal.pp;
    (let y = Flat.lhs a and z = Flat.rhs a in  (* [rho |- x = y*z]. *)
     let theta = ref rho in
     let x' = replace theta x 
     and y' = replace theta y
     and z' = replace theta z in
       if x == x' && y == y' && z == z' then () else
	 let a' = Nonlin.mk_mult y' z' in
	   if not(Term.eq x' a') then 
	     Ac.remove (x, a, rho);
	   let a'' = linearize theta a' in
	     assert(A.is_pure a'');
	     Ac.remove (x, a, rho);
	     Simplex.Infsys.process_equal (x', a'', !theta));
    Trace.exit0 5 "Nl.deduce(la)"

  and replace theta x =
    try
      let (a, rho) = Simplex.Eqs.apply (Simplex.Infsys.current()) x in
	theta := Jst.dep2 rho !theta; a
    with
	Not_found -> x
    
		
  (** Returns a linear arithmetic term obtained by renaming nonlinear subterms. *)
  and linearize rho a =
    assert(Nonlin.is_pure a);
    let varify a =
      assert(Monomial.is_canonical a);
      assert(not(Monomial.is_one a));
      let m = Nonlin.monomial_of a in
	if Term.is_var m then a else
	  let q = Nonlin.coeff_of a in
	  let x = Ac.var_of_pure rho m in
	    assert(Term.is_var x);
	    A.mk_multq q x 
    in
    let a' = Nonlin.map varify a in
      assert(A.is_pure a');
      a'

  let linearize rho =
    Trace.func 7 "Linearize" Term.pp Term.pp (linearize rho)
      
  (** Propagation of disjunctions [x = 0 || y = 1] as 
    obtained from [x = y * z] if [y == x] or [z == x]. *)
  let rec disj x (a, rho) =    
    assert(Term.is_var x && Flat.is a); 
    Trace.call 5 "Nl.disj" (x, a, rho) Jst.Equal.pp;
    (let y = Flat.lhs a and z = Flat.rhs a in
      if y == x then                  (* [rho |- x = x*z]. *)
	begin
	  Ac.remove (x, a, rho);
	  process_disj (x, A.mk_decr z, rho); 
	end 
      else if z == x then             (* [rho |- x = y*x]. *)
	begin
	  Ac.remove (x, a, rho);
	  process_disj (x, A.mk_decr y, rho)
	end);
    Trace.exit0 5 "Nl.disj"
	  
  (** Process [a = 0 || b = 0] by introducing a new boolean variable [z]
    and processing [a = a * z] and [b = b * (1 - z) = b - b * z]. *)
  and process_disj (a, b, rho) =
    assert(A.is_pure a && A.is_pure b);
    let is_zero a =
      try if Mpa.Q.is_zero (A.d_num a) then Three.Yes else Three.No
      with Not_found -> Three.X
    in
    let theta = ref rho in
    let a = replace theta a in
    let b = replace theta b in
      match is_zero a, is_zero b with
	| Three.Yes, _ -> ()
	| _, Three.Yes -> ()
	| Three.No, Three.No -> 
	    raise(Jst.Inconsistent(!theta))
	| Three.No, _ -> 
	    Simplex.Infsys.process_equal_zero (b, !theta)
	| _, Three.No -> 
	    Simplex.Infsys.process_equal_zero (a, !theta)
	| Three.X, Three.X ->
	    let z = Term.mk_fresh_var "nl" in 
	      Simplex.Infsys.process_nonneg (z, Jst.dep0);              (* [0 <= z <= 1]. *)
	      Simplex.Infsys.process_nonneg (A.mk_incr (A.mk_neg z), Jst.dep0);
	      let e' = linearize theta (A.mk_sub a (Nonlin.mk_mult z a)) in
	      let e'' = linearize theta (Nonlin.mk_mult z b) in
		Simplex.Infsys.process_equal_zero (e', !theta);         (* [a - z*a = 0]. *)
		Simplex.Infsys.process_equal_zero (e'', !theta)         (* [z * b = 0]. *)
		  
  let all x ((a, _) as cod) =
    assert(Term.is_var x && Flat.is a);
    Trace.call 4 "Nl.deduce" x Term.pp;
    cnstrnts x cod;
    nonneg x cod;
    disj x cod;
    linarith x cod;
    Trace.exit0 4 "Nl.deduce"
    
      
end 
*)


(** Inference System for nonlinear monomials. The configuration consists
 of equalities [x = y * z]. *)
module Infsys = struct

  let current = Ac.current
  let initialize = Ac.initialize
  let is_unchanged = Ac.is_unchanged
  let finalize = Ac.finalize
  let abstract = Ac.abstract
  let normalize = Ac.normalize

  let crossmultiply_equal e =
    let a, b =  Monomial.crossmultiply (e#lhs, e#rhs) in
      failwith "to do"

  let crossmultiply_diseq e =
    let a, b =  Monomial.crossmultiply (e#lhs, e#rhs) in
      failwith "to do"
	
  (** Processing an equality [a = b]. Denumerators are eliminated
    by means of crossmultiplication. *)
  let process_equal e =
    assert(Monomial.is_pure e#lhs && Monomial.is_pure e#rhs);
    let e = crossmultiply_equal e in
    let s = e#lhs and t = e#rhs in
      match Monomial.is_one s, Monomial.is_one t with
	| true, true -> ()
	| true, false ->                     (* [e1 |- x = 1], [s=1] *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x); 
	      Ac.process_equal (J.mk_trans e1 e)
	| false, true ->                      (* [e1 |- x = 1], [t=1] *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x);
	      Ac.process_equal (J.mk_trans e1 (J.mk_sym e))
	| false, false ->
	    Ac.process_equal e
	      
  (** Processing a disequality [a <> b]. Denumerators are 
    eliminated by means of crossmultiplication. *)
  let process_diseq d =
    assert(Monomial.is_pure d#lhs && Monomial.is_pure d#rhs);  
    let d = crossmultiply_diseq d in         (* [d |- a <> b]. *)
    let a = d#lhs and b = d#rhs in
      match Monomial.is_one a, Monomial.is_one b with
	| true, true -> 
	    raise(Judgement.Unsat(J.mk_bot d))
	| true, false ->                     (* [e1 |- x = 1], [a == 1]. *)
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x);  
	      Ac.process_diseq (J.mk_replace_in_diseq e1 (J.mk_refl b) d)
	| false, true -> 
	    let x, e1 = Simplex.Infsys.alias Mpa.Q.one in
	      assert(Term.is_var x); 
	      Ac.process_diseq (J.mk_replace_in_diseq (J.mk_refl a) e1 d)
	| false, false ->
	    Ac.process_diseq d
	      
  (** Processing [a >= b]. *)
  let process_nonneg n =
    assert(Monomial.is_pure n#arg);
    let e = Deduce.linearize n#arg in
    let n' = J.mk_replace_in_nonneg e n in
      Simplex.Infsys.process_nonneg n'
  
  let propagate_equal x =
    assert(Term.is_var x);
    Ac.propagate_equal x
        
  let propagate_cnstrnt x =
    assert(Term.is_var x);
    ()
 (*   if not(is_empty()) then 
      begin 
	Trace.call 3 "Nl.prop(c)" x Term.pp;
	Ac.iter_on x Deduce.cnstrnts;
	Trace.exit0 3 "Nl.prop(c)"
      end 
 *)

  let propagate_restricted x =
    assert(Term.is_var x);
    ()
(*
    if not(is_empty()) then
      begin 
	Trace.call 3 "Nl.prop(>=0)" x Term.pp;
	Ac.iter_on x Deduce.nonneg;
	Trace.exit0 3 "Nl.prop(>=0)"
      end 
*)
	
  let propagate_linarith x = 
    assert(Term.is_var x);
    ()
(*
    if not(is_empty()) then
      begin 
	Trace.call 3 "Nl.prop(la)" x Term.pp;  
	Ac.iter_on x Deduce.linarith;
	Trace.exit0 3 "Nl.prop(la)"
      end
*)
	
  let _ = 
    Simplex.Effect.register propagate_linarith

(*
  let _ = 
    Ac.register Deduce.all
*)

  let branch () = raise Not_found
end 

(*
module Unit = 
  Eqs.Register(Infsys)
*)

