
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*i*)
open Tools
open Hashcons
open Mpa
open Term
open Eqn
(*i*)

(*s Recognizers. *)

let rec is_arith a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num _, [] -> 
	       true
	   | Multq(q), [x] ->
	       not(Q.is_zero q) && not(is_arith x)
	   | Add, _::_::_ ->
	       true
	   | Mult, _::_::_ ->
	       true
	   | Div, [_;_] ->
	       true
	   | _ ->
	       false)
    | _ ->
	false

let rec is_linarith a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num _, [] -> 
	       true
	   | Multq(_), [_] ->
	       true
	   | Add, _::_::_ ->
	       true
	   | _ ->
	       false)
    | _ ->
	false

let is_fresh a =
  match a.node with
    | Var(_,Fresh(IntDom)) -> true
    | _ -> false 

 
(*s Apply [f] at uninterpreted positions. *)

let rec iter f a =          
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num _, [] -> ()
	   | Multq _, [x] -> f x
	   | Add, _::_::_ -> List.iter (iter f) l
	   | Mult, _::_::_ -> List.iter (iter f) l
	   | Div, [x;y] -> iter f x; iter f y
	   | _ -> assert false)
    | _ ->
	f a

		  
(*s Constants. *)

let num = mk_num

let zero = num(Q.zero)

let one = num(Q.one)


(*s Some normalization functions. [poly_of a] constructs
  a list of monomials, where a monomial is every term except
  an addition. [to_monomial a] normalizes a monomial into the
  coefficient and the variable part, if any. *)

let poly_of a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num(q), [] -> if Q.is_zero q then [] else [a]
	   | Multq _, [_] -> [a]
	   | Mult, _::_::_ -> [a]
	   | Add, _::_::_ -> l
	   | Div, [_;_] -> [a]
	   | _ -> 
	       let str = Tools.pp_to_string Pretty.term a in
	       failwith ("Arith.poly_of: Ill-formed monomial" ^ str))
    | _ -> 
	[a]

let of_poly = mk_add

let monomial_of a =
  match a.node with
    | App({node=Interp(Arith(op))},l) ->
	(match op,l with
	   | Num(q), [] ->
	       (q, None)
	   | Multq(q), [x] ->
	       (q, Some(x))
	   | Mult, l ->
	       (Q.one, Some(a))
	   | Div, [_;_] ->
	       (Q.one, Some(a))
	   | _ ->
	       let str = Tools.pp_to_string Pretty.term a in
	       failwith("Arith.monomial_of: Ill-formed monomial" ^ str))
    | _ -> 
	(Q.one ,Some(a))

let of_monomial (q,x) =
  if Q.is_zero q then 
    zero
  else 
    match x with
      | None -> num q
      | Some(x) -> mk_multq q x



(*s Addition *)
		  
let rec addp l1 l2 =                          (* Add two polynomials *)
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' -> 
	match monomial_of m1, monomial_of m2 with
	  | (q1,None), (q2,None) ->
	      let q = Q.add q1 q2 in
	      if Q.is_zero q then
		addp l1' l2'
	      else 
		num(Q.add q1 q2) :: addp l1' l2'
	  | (_,None), _ ->
	      m1 :: addp l1' l2
	  | _, (_, None) ->
	      m2 :: addp l1 l2'
	  | (q1,Some(x1)), (q2,Some(x2)) ->
	      let cmp = cmp x1 x2 in
	      if cmp = 0 then
		let q = Q.add q1 q2 in
		if Q.is_zero q then 
		  addp l1' l2'
		else 
		  let m' = mk_multq (Q.add q1 q2) x1 in
		  m' :: addp l1' l2'
	      else if cmp < 0 then
		m1 :: addp l1' l2
	      else
		m2 :: addp l1 l2'

let add2 (a,b) =
  mk_add (addp (poly_of a) (poly_of b))

let add l =
  List.fold_right (fun x acc -> add2(x,acc)) l zero
  
let incr a =
  add2(a, one)


(*s Multiplication. *)

let multpp a b =                      (* Multiply two power products. *)
  Pproduct.to_term
    (Pproduct.mult (Pproduct.of_term a, Pproduct.of_term b))

let multmm (q,x) (p,y) =              (* Multiply two monomials. *)
  let qp = Q.mult q p in
  let xy = match x, y with
    | None, None -> None
    | None, Some _ -> y
    | Some _, None -> x
    | Some(a), Some(b) ->
	Some(multpp a b)
  in
  (qp,xy)

let multm (q,x) l =          (* multiply all monomials in [l] with a monomial [q*x] *)
  let rec loop l = 
    match l with
      | [] -> []
      | m :: ml -> 
	  let a = of_monomial (multmm (q,x) (monomial_of m)) in
	  if a === zero then loop ml else a :: loop ml
  in
  Sort.list (<<<) (loop l)

let rec multp l1 l2 =
  match l1 with
    | [] -> []
    | m1 :: l1' -> 
	 addp (multm (monomial_of m1) l2) (multp l1' l2)
	  
let rec mult2 (a,b) =
  if is_zero a || is_zero b then
    zero
  else if is_one a then
    b
  else if is_one b then
    a
  else if is_div a then
    let (a1,a2) = d_div a in
    div (mult2 (a1,b), a2)
  else if is_div b then
    let (b1,b2) = d_div b in
    div (mult2 (a,b1), b2)
  else 
    mk_mult (multp (poly_of a) (poly_of b))

and mult l =
  match l with
    | [] -> one
    | [x] -> x
    | x :: xl -> mult2 (x, mult xl)


(*s Operations on the coefficients of a polynomial *)

and mapq f a =
  mk_add (List.fold_right
	    (fun m acc ->
	       let (q,x) = monomial_of m in
	       let q' = f q in
	       if Q.is_zero q' then 
		 acc 
	       else
		 let m' = match x with
		   | None -> num q'
		   | Some(x') -> mk_multq q' x'
		 in
		 m' :: acc)
	    (poly_of a)
	    [])

and neg a = 
  mapq Q.minus a

and divq q a =
  mapq (Q.div q) a

and addq q a  = 
  mapq (Q.add q) a

and sub (a,b) =
  add2(a,neg b)

and multq q a =
  if Q.is_zero q then zero
  else if Q.is_one q then a
  else if is_div a then
    let (a1,a2) = d_div a in
    if is_num a1 then
      mk_multq (Q.mult q (d_num a1)) (inv a2)
    else if is_num a2 then
      mk_multq (Q.div q (d_num a2)) a1
    else 
      mapq (Q.mult q) a
  else 
    mapq (Q.mult q) a

and inv a =
  if is_num a then
    num (Q.inv (d_num a))
  else if is_div a then
    let (a1,a2) = d_div a in
    mk_div (a2,a1)
  else 
    mk_div (one,a)
   
(*s Division. *)

and div (a,b) =
  if is_div a then
    let (a1,a2) = d_div a in
    div (a1, mult2 (a2,b))
  else if is_div b then
    let (b1,b2) = d_div b in
    mult2 (a, div (b2,b1))
  else
  add (List.map (fun m -> divm(m,b)) (poly_of a))

and divm (m,b) =
  if m === b then
    one
  else if is_num b then 
    multq (Q.inv(d_num b)) m
  else 
    let (q,x) = monomial_of m in             
    if Mpa.Q.is_zero q then zero else
    match x with
      | None ->                            (*s Case: [q * 1/b] *)
	  multq q (inv b)
      | Some(x) ->                         (*s Case: [(q * x) / b] *)
	  multq q (cancelled_div (x,b))

and cancelled_div (a,b) =
  let (l1,l2) = 
    Pproduct.cancel 
      (Pproduct.of_term a, 
       Pproduct.of_term b) 
  in
  let a' = Pproduct.to_term l1 in
  let b' = Pproduct.to_term l2 in
  if is_one b' then a' 
  else if is_zero a' then zero
  else mk_div (a',b')



(*s Interface for sigmatizing arithmetic terms. *)

let sigma op l =
  match op, l with
    | Num(q), [] ->
	num q
    | Add, _:: _:: _ -> 
	add l
    | Multq(q), [x] ->
	multq q x
    | Mult, _::_::_ ->
	mult l
    | Div, [x;y] ->
	div (x,y)
    | _ ->
	failwith("Arith.sigma: Ill-formed arithmetic expression")

(*s Decompose a list of monomials into the list of all but last
  monomials and the last monomial in decomposed form. *)

let rec decompose l =       
  match l with            
    | [] -> ([], Q.zero, None)
    | [m] -> 
        let (q,x) = monomial_of m in
        ([], q,x)
    | m :: ml -> 
        let (pre, q,x) = decompose ml in
        (m :: pre, q, x)

(*s Decompose [l] into coefficient of power product [x] and the rest of [l]. *)

let choose x =
  let rec loop = 
    function
      | [] ->
	  raise Not_found
      | m :: ml ->
	  match monomial_of m with
	    | _, None -> 
		let (q1,ml1) = loop ml in
		(q1, m :: ml1)
	    | q, Some(y) ->
		assert(not(Q.is_zero q));
		let cmp = Term.cmp x y in
		if cmp = 0 then
		  (q,ml)
		else if cmp > 0 then
		  let (q1,ml1) = loop ml in
		  (q1, m :: ml1)
		else 
		  raise Not_found
  in
  loop    

(*s Compute the least-common multiple of the coefficients of 
  the term [a] in polynomial-normal form. *)
	
let lcm a =
  List.fold_right 
    (fun m acc ->
       let (q,x) = monomial_of m in
       assert(not(Q.is_zero q));
       Z.lcm (Q.denominator q) acc)
    (poly_of a)
    (Z.of_int 1)


(*s Split polynomial in leading constant and rest. *)

let leading a =     
  let p = poly_of a in
  match p with
    | [] -> (Q.zero, [])
    | m :: ml ->
	let (q,x) = monomial_of m in
	match x with
	  | None -> (q, ml)
	  | Some _ -> (Q.zero, p)


(*s Constructing inequalities. *)

let lt (a,b) =
  let (q, ml) = leading (sub(a,b)) in          (*s [q + ml < 0] *)
  let q' = Q.minus q in
  match ml with
    | [] ->                                    (*s Case [0 < q'] *)
	if Q.lt Q.zero q' then Bool.tt() else Bool.ff()
    | m :: ml ->                               (*s Case [p * x + ml < q'] *)
	(match monomial_of m with
	   | p, Some(x) ->
	       assert(not(Q.is_zero p));
	       let c = 
		 if Q.gt p Q.zero then 
		   Cnstrnt.lt Interval.Real (Q.div q' p)
		 else 
		   Cnstrnt.gt Interval.Real (Q.div q' p)
	       in
	       Atom.cnstrnt c (add2(x,(multq (Q.inv p) (add ml))))
	   | _ ->
	       failwith "Arith.lt: fatal error")


let le (a,b) = 
 let (q, ml) = leading (sub(a,b)) in          (*s [q + ml < 0] *)
  let q' = Q.minus q in
  match ml with
    | [] ->                                    (*s Case [0 < q'] *)
	if Q.le Q.zero q' then Bool.tt() else Bool.ff()
    | m :: ml ->                               (*s Case [p * x + ml < q'] *)
	(match monomial_of m with
	   | p, Some(x) ->
	       assert(not(Q.is_zero p));
	       let c = 
		 if Q.gt p Q.zero then 
		   Cnstrnt.le Interval.Real (Q.div q' p)
		 else 
		   Cnstrnt.ge Interval.Real (Q.div q' p)
	       in
	       Atom.cnstrnt c (add2(x,(multq (Q.inv p) (add ml))))
	   | _ ->
	       failwith "Arith.lt: fatal error")

(*s Solving of an equality [a = b], represented by the pair [a,b)]
  in the rationals and the integers. *)

let qsolve e =
  match decompose (poly_of (sub e)) with
    | [], q, None ->                                          (*s case [q = 0] *)
	if Q.is_zero q then None else raise(Exc.Inconsistent)
    | l, q, Some(x) ->                                        (*s case [l + q * x = 0 *)
	assert(not(Q.is_zero q));
        let b = multq (Q.minus (Q.inv q)) (add l) in
	let cmp =  cmp x b in
	if cmp = 0 then
	  None
	else if cmp = 1 || is_linarith b then
	  Some(x,b)
	else 
	  Some(b,x)
    | _::_, _, None ->                                        (*s case [l + q = 0] *)
	failwith "Arith.qsolve: fatal error"

let qsolve_for x e =
  try
    let (q,ml) = choose x (poly_of (sub e)) in
    assert(not(Q.is_zero q));
    let a = multq (Q.inv (Q.minus q)) (of_poly ml) in
    if x === a then None else Some(a) 
  with
      Not_found -> failwith "Arith.qsolve_for: not found."

(*s Solving of an equality [a = b] like qsolve, but possible
  solutions such as [x * y |-> c] are further processed to obtain
  [y |-> c / x] whenever it is known that [x] is nonzero in the constraint
  context [c]. *)

let rec qsolve1 ctxt e =
  match qsolve e with
    | None -> None
    | Some(xy,c) as e' ->
	match Pproduct.partition (Pproduct.of_term xy) with
	  | None -> e'
	  | Some(xl, yl) ->
	      let x' = Pproduct.to_term xl in
	      if Cnstrnt.is_nonzero (Cnstrnt.of_term ctxt x') then
		let y' = Pproduct.to_term yl in
		if is_num y' then
		  Some(mk_div(c,x'),y')
		else 
		  Some(y', mk_div(c,x'))
	else
	  e'


(*s Solving an arithmetic equality in the integers. *)

module Euclid = Euclid.Make(
  struct
  type q = Q.t
  let eq = Q.equal
  let ( + ) = Q.add
  let inv = Q.minus
  let zero = Q.zero
  let ( * ) = Q.mult
  let one = Q.one
  let ( / ) a b = Q.div a b
  let floor q = Q.of_z (Q.floor q)
   let is_int = Q.is_integer
  end)    

(*s Compute the general solution of a linear Diophantine
  equation with coefficients [al], the gcd [d] of [al]
  and a particular solution [pl]. In the case of four
  coeffients, compute, for example,
  \begin{verbatim}
  (p0 p1 p2 p3) + k0/d * (a1 -a0 0 0) + k1/d * (0 a2 -a1 0) + k2/d * (0 0 a3 -a2)
  \end{verbatim}
  Here, [k0], [k1], and [k2] are fresh variables. Note that
  any basis of the vector space of solutions [xl]
  of the equation [al * xl = 0] would be appropriate. *)

let diophantine_solve al b =
  match Euclid.solve al b with
    | None -> 
        raise(Exc.Inconsistent)
    | Some (d,pl) ->
        let rec loop al zl =
          match al, zl with
            | [_], [_] -> zl
            | a0 :: ((a1 :: al'') as al'),  z0 :: z1 :: zl'' ->
                let k = mk_fresh IntDom in
                let e0 = add2 (z0, multq (Q.div a1 d) k) in
                let e1 = add2 (z1, multq (Q.div (Q.minus a0) d) k) in
                e0 :: loop al' (e1 :: zl'')
            | _ -> assert false
        in
        loop al (List.map num pl)

let vectorize a =
  let (q, p) = leading a in
  let (ql,xl) = 
    List.fold_right
      (fun m (ql,xl) ->
	 match monomial_of m with
	   | (q, Some(x)) -> (q :: ql, x :: xl)
	   | _ -> failwith "Arith.vectorize: fatal error")
      p ([],[])
  in
  (ql,xl,Q.minus q)

let zsolve (a,b) =
  let p = sub (a,b) in
  if is_num p then
    if is_zero p then [] else raise(Exc.Inconsistent)
  else
    let (cl,xl,b) = vectorize p in
    List.combine xl (diophantine_solve cl b)


(*s Normalize a term [a] with respect to a given substitution [s].
    A substitution is represented as a list of pairs. *)

let norm rho a =
  let rec loop a =
    match a.node with
      | App({node=Interp(Arith(op))},l) ->
	(match op, l with
	   | Num _, [] -> 
	       a
	   | Multq(q), [x] ->
	       hom1 a (multq q) loop x
	   | Add, l ->
	       homl a add loop l
	   | Mult, l ->
	       homl a mult loop l
	   | Div, [x;y] ->
	       hom2 a div loop (x,y) 
	   | _ ->
	       failwith("Arith.norm: invalid argument" ^ pp_to_string Pretty.term a))
      | _ ->
	  try
	    Subst.apply rho a with Not_found -> a
  in
  loop a


(*s Check if [a] is Diophantine. *)

let is_diophantine ctxt a =    (*s tests if all power product are known to be integer. *)
  List.for_all
    (fun m ->
       match monomial_of m with
	 | (_, Some(x)) -> Cnstrnt.sub (ctxt x) Cnstrnt.int
	 | _ -> true)
    (poly_of a)


(*s Solving with respect to information in the data base. *)

let rec solve ctxt (a,b) = 
  let sl = 
    if is_diophantine ctxt a && is_diophantine ctxt b then
      zsolve (a,b)
    else 
      match qsolve1 ctxt (a,b) with
	| None -> []
	| Some(x,y) -> [(x,y)]
  in
(*  assert(check (a,b) sl); *)
  sl

and check (a,b) sl =
  let rho = Subst.of_list sl in
  norm rho a === norm rho b

	  
(*s Database of arithmetic facts. *)

module A = Th.Make(
  struct
    let name = "a"
    let is_th = is_linarith
    let iter = iter
  end)

type t = {
  mutable find : A.t;               (*s find structures for arithmetic equalities. *)
}

let empty () = {
  find = A.empty()
}

let copy s = {
  find = A.copy s.find;
}

let subst_of s = A.subst_of s.find
let use_of s = A.use_of s.find

let apply s a = A.apply s.find a
let find s a = A.find s.find a 
let inv s a = A.inv s.find a
let use s a = A.use s.find a


(*s Merging in an arithmetic equation. *)

let union s ((a,_) as e) =
  A.union s.find e

let remove s a =
  A.restrict s.find a

(*s Extending the domain of the find structure. *)

let extend s ((a,_) as e) = 
  Trace.call 7 "Ext(a)" e Pretty.eqn;
  A.extend s.find e

(*s External equations. *)

let is_external (a,b) =
  not(is_linarith a) &&
  not(is_fresh a) &&
  not(is_arith b || is_fresh b)


(*s Equalities between fresh variables just need to be propagated. 
  Install equality [x = e], where [x] is uninterpreted, and [e] is interpreted.
  Equalities between uninterpreted terms are added to set of generated equalities.
  Now, propagate the equality [a = b] for all bindings [inv s u |-> u] such that
  [a] occurs interpreted in [u]. *)

let nrm s a =
  let rho = A.subst_of s.find in
  norm rho a

let merge ctxt s e =
  Trace.call 7 "Merge(a)" e Pretty.eqn;
  let eqs = ref []                            (* newly discovered equations. *)
  and cnstrnts = ref []                       (* newly discovered constraints. *)
  in
  let rec loop ((a,b) as e) =
    if not(a === b) then                      
      begin
	if is_arith b && not(is_fresh a) then
	  begin
	    try 
	      let a' = inv s b in             (* [a' |-> b] already known. Thus, *)     
	      eqs := Eqn.cons (a, a') !eqs    (* infer new equality [a = a']. *)
	    with
		Not_found ->
		  let ca = ctxt a in
		  let ca' = Cnstrnt.inter ca (ctxt b) in
		  match Cnstrnt.analyze ca' with
		    | Cnstrnt.Empty ->
			raise Exc.Inconsistent
		    | Cnstrnt.Singleton(q) ->
			union s (a, mk_num q)
		    | Cnstrnt.Full ->
			union s e
		    | Cnstrnt.Other ->
			cnstrnts := (ca',a) :: !cnstrnts; (* propagate constraints. *)
			union s e
	  end;
	Term.Set.iter                    
	  (fun u ->                     
	     let b' = nrm s u in
	     let a' = inv s u in
	     if is_external (a',b') then           (* New uninterpreted equality. *)
	       begin
		 remove s a';
		 eqs := Eqn.cons (a',b') !eqs
	       end;
	     loop (a', b'))
	  (use s a)
      end
  in
  loop e;
  (!eqs, !cnstrnts)

let compose ctxt s =                           (*s Composing a set of equalities. *)
  List.fold_left 
    (fun (el,cl) e ->
       let (el',cl') = merge ctxt s e in
       (el' @@ el, cl' @ cl))
    ([],[])

let process ctxt s ((a,b) as e) =
  Trace.call 7 "Add(a)" e Pretty.eqn;
  let sl = solve ctxt (nrm s a, nrm s b) in
  let el1 = List.filter is_external sl in     (* newly infered equalities. *)
  let (el2,cl2) = compose ctxt s sl in
  Trace.exit 7 "Add(a)" (el1 @ el2) (Pretty.list Pretty.eqn);
  (el1 @@ el2,cl2)

let processn ctxt s =
  List.fold_left
    (fun (el,cl) ((x,y) as e) ->
       if find s x === find s y then
	 (el,cl)
       else 
	 let (el',cl') = process ctxt s e in
	 (el' @@ el, cl' @ cl))
    ([],[])

let propagate1 ctxt s ((a,b) as e) =
  Trace.call 7 "Prop(a)" e Pretty.eqn;
  let sl = solve ctxt (nrm s a, nrm s b) in
  let el1 = Eqn.remove e (List.filter is_external sl) in
  let (el2,cl2) = compose ctxt s sl in
  Trace.exit 7 "Prop(a)" (el1 @ el2) (Pretty.list Pretty.eqn);
  (el1 @@ el2,cl2)

let propagate ctxt s =
  List.fold_left 
    (fun (el,cl) e ->
       let (el1,cl1) = propagate1 ctxt s e in
       (el1 @@ el, cl1 @ cl))
    ([],[])

let rec ext s a =
  try
    let b = apply s a in
    Term.Set.singleton b
  with
      Not_found ->
	let solve = qsolve_for a in
	Term.Set.fold 
	  (fun b acc ->
	     match solve (b, inv s b) with
	       | None -> acc
	       | Some(y) -> Term.Set.add y acc)
	  (use s a)
	  Term.Set.empty

let cnstrnt ctxt s a =
  let bs = ext s a in
  Term.Set.fold 
    (fun x -> 
       Cnstrnt.inter (Cnstrnt.of_term ctxt x))
    bs 
    Cnstrnt.top

(*s Critical pair computation for two power products. *)

let cp a b =
  match Pproduct.cp (Pproduct.of_term a, Pproduct.of_term b) with
    | None -> None
    | Some(x,y) -> Some(Pproduct.to_term x, Pproduct.to_term y)
	
let groebner1 s =
  Subst.fold2 
    (fun (a1,b1) (a2,b2) acc ->
       assert(not(a1 === a2));
       match cp a1 a2 with
	 | None -> acc
	 | Some(x1,x2) ->
	     let lhs = mult2 (b1,x1) in
	     let rhs = mult2 (b2,x2) in
	     if find s lhs === find s rhs then
	       acc
	     else 
	       let e = (lhs, rhs) in
               Trace.call 9 "Cp(a)" e Pretty.eqn;
	       e :: acc)
    (A.subst_of s.find)
    []

let groebner ctxt s =
  let rec loop (el,cl) =
    let nl = groebner1 s in
    Trace.call 8 "Groebner" nl (Pretty.list Pretty.eqn);
    if nl = [] then 
      (el,cl)
    else 
      let (el',cl') = processn ctxt s nl in
      loop (el' @@ el, cl' @ cl)
  in
  loop ([],[])







