
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
open Hashcons
(*i*)

module type ITE = sig
  type bdd_node
  type bdd = bdd_node hashed
  type tag
  val compare : bdd -> bdd -> int
  val high : tag -> bdd
  val low : tag -> bdd
  val ite : tag -> bdd -> bdd -> bdd -> bdd
  val is_high : bdd -> bool
  val is_low : bdd -> bool
  val is_ite : bdd -> bool
  val destructure_ite : bdd -> (bdd * bdd * bdd) option
  val fresh : tag -> bdd
end

module Make(Ite : ITE) = struct

  open Ite

(*s Atomic terms are interpred as a positive literal *)

  let simplify b =
    match destructure_ite b with
      | Some(x,p,n) when is_high p && is_low n -> x
      | _ -> b

  let d_ite tg b =
    if is_low b || is_high b then None
    else let b3 = destructure_ite b in
      match b3 with
	| Some _ -> b3
	| None -> Some(b, high tg, low tg)

(*s Building up BDDs *)

  let cofactors tg x s =
    match d_ite tg s with
      | Some (y,s1,s2) when x === y -> (s1,s2)
      | _ -> (s,s)
      
  let maxvar x y =
    if (compare x y) <= 0 then x else y
      
  let topvar tg x s2 s3 =
    match d_ite tg s2, d_ite tg s3 with
      | Some (y,_,_), Some (z,_,_) -> maxvar x (maxvar y z)
      | Some (y,_,_), None -> maxvar x y
      | None, Some(z,_,_) -> maxvar x z
      | None, None -> x

  module H3 = Hasht.Make(
    struct
      type t = bdd * bdd * bdd
      let equal (a1,a2,a3) (b1,b2,b3) = a1 === b1 & a2 === b2 & a3 === b3
      let hash (b1,b2,b3) = b1.tag + b2.tag + b3.tag

    end)
 
  let ht = H3.create 10007

  let _ = Tools.add_at_exit (fun () -> print_string "Bdd. : "; H3.stat ht)

  let rec build tg s3 =
    try
      H3.find ht s3
    with Not_found ->
      let b = simplify (build_fun tg s3) in
      H3.add ht s3 b; b 

  and build_fun tg (s1,s2,s3) =
    if s2 === s3 then s2
    else if is_high s2 && is_low s3 then s1
    else if is_high s1 then s2
    else if is_low s1 then s3
    else match d_ite tg s1 with
      | Some(y,_,_) ->
	  let x = topvar tg y s2 s3 in
	  let (p1,n1) = cofactors tg x s1 in
	  let (p2,n2) = cofactors tg x s2 in
	  let (p3,n3) = cofactors tg x s3 in
	  let p = build tg (p1,p2,p3) in
	  let n = build tg (n1,n2,n3) in
	  if p === n then p else ite tg x p n
      | _ -> assert false

  (*s Derived constructors. *)

  let neg tg s      = build tg (s,low tg,high tg)
  let conj tg s1 s2 = build tg (s1,s2,low tg)
  let disj tg s1 s2 = build tg (s1,high tg,s2)
  let xor tg s1 s2  = build tg (s1,neg tg s2,s2)
  let imp tg s1 s2  = build tg (s1,s2,high tg)
  let iff tg s1 s2  = build tg (s1,s2,neg tg s2)

  (*s Derived recognizers. *)
			
  let is_neg b =
    match destructure_ite b with
      | Some(_,p,n) -> is_low p && is_high n
      | None -> false
   
  let is_conj b =
    match destructure_ite b with
      | Some(_,_,n) -> is_low n
      | None -> false

  let is_disj b =
    match destructure_ite b with
      | Some(_,p,_) -> is_high p
      | None -> false

  let is_xor b =
    match destructure_ite b with
      | Some(_,p,n) ->
	  (match destructure_ite p with
	     | Some(x',p',n') -> is_low p' && is_high n' && x' === n
	     | None -> false)
      | None -> false

  let is_imp b =
    match destructure_ite b with
      | Some(_,_,n) -> is_high n
      | None -> false

  let is_iff b =
    match destructure_ite b with
      | Some(_,p,n) ->
	  (match destructure_ite n with
	     | Some(x',p',n') -> is_low p' && is_high n' && p === x'
	     | None -> false)
      | None -> false

 (*s Derived deconstructors. *)
			
  let d_neg b =
    match destructure_ite b with
      | Some(x,p,n) when is_low p && is_high n -> x
      | _ -> raise (Invalid_argument "Bdd.d_neg: not a negation")
   
  let d_conj b =
    match destructure_ite b with
      | Some(x,p,n) when is_low n -> (x,p)
      | _ -> raise (Invalid_argument "Bdd.d_conj: not a conjunction")

  let d_disj b =
    match destructure_ite b with
      | Some(x,p,n) when is_high p -> (x,n)
      | _ -> raise (Invalid_argument "Bdd.d_conj: not a disjunction")

  let d_xor b =
    match destructure_ite b with
      | Some(x,p,n) ->
	  (match destructure_ite p with
	     | Some(x',p',n') when is_low p' && is_high n' && x' === n -> (x,n)
	     | _ -> raise (Invalid_argument "Bdd.d_xor: not an exclusive or"))
      | _ -> raise (Invalid_argument "Bdd.d_xor: not an exclusive or")
	    
  let d_imp b =
    match destructure_ite b with
      | Some(x,p,n) when is_high n -> (x,p)
      | _ -> raise (Invalid_argument "Bdd.d_imp: not an implication")

  let d_iff b =
    match destructure_ite b with
      | Some(x,p,n) ->
	  (match destructure_ite n with
	     | Some(x',p',n') when is_low p' && is_high n' && p === x' -> (x,p)
	     | _ -> raise (Invalid_argument "Bdd.d_imp: not an equivalence"))
      | _ -> raise (Invalid_argument "Bdd.d_imp: not an equivalence")      

      
  (* Based on the equation:
       [ite(x,p,n) = (p or n) and exists delta. x = (p and (n => delta))]
   *)

  let solve1 tg s =
    if is_low s then
      None
    else if is_high s then
      Some []
    else
      match destructure_ite s with
	| Some(x,p,n) ->
	    let e = conj tg p (imp tg n (fresh tg)) in
	    if x === e then
	      Some [disj tg p n, high tg]
	    else
	      Some [disj tg p n, high tg; x, e]
	| _ -> assert false

	      
	      (*s Returns either an equivalent solved form
		or raises the Inconsistent exception if not solvable.
		Based on the equation:
		[ite(x,p,n) = (p or n) and exists delta. x = (p and (n => delta))]
	      *)

  let subst tg s x t =
    let rec sub s = 
      if is_high s or is_low s then s
      else match d_ite tg s with
	| Some(y,p,n) ->
	    if x === y then build tg (t,sub p,sub n) else s
	| None ->
	    if s === x then t else s
    in
    sub s

  let add tg x t e =
    if x === t then e else
      (x,t) :: (List.map (fun (y,s) -> (y, subst tg s x t)) e)
      
  let solve tg s =
    let rec triangular_solve s e =
      match d_ite tg s with
	| Some (x,p,n) ->
	    if is_high p & is_low n then          (* poslit *)
	      add tg x p e
	    else if is_low p & is_high n then     (* neglit *)
	      add tg x p e
	    else
	      let t' = conj tg p (imp tg n (fresh tg)) in
	      let e' = add tg x t' e in
	      let s' = disj tg p n in
	      if is_low s' then
		raise (Exc.Inconsistent "Bdd solver")
	      else if is_high s' then
		e'
	      else if is_ite s' then
		triangular_solve s' e'
	      else
		add tg s' (high tg) e'
	| None ->
	    add tg s (high tg) e
    in
    if is_low s then
      None
    else if is_high s then
      Some []
    else
      try
	Some(triangular_solve s [])
      with
	  Exc.Inconsistent _ -> None

end











