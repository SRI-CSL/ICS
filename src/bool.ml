
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
 *
 * Author: Harald Ruess
 i*)


(*i*)
open Term
open Hashcons
open Binrel
(*i*)

(*s Test if a term is considered to be boolean. *)

let is_bool a =
  match a.node with
    | App({node=Interp(Bool _)},_) -> true
    | _ -> false


(* apply [f] at uninterpreted positions. *)

let rec iter f a =        
  match a.node with
    | App({node=Interp(Bool(op))},l) ->
	(match op, l with
	   | (True | False), [] -> ()
	   | Ite, [x;y;z] -> iter f x; iter f y; iter f z
	   | _ -> assert false)
    | App({node=Pred(p)},l) ->
	(match p, l with
	   | Equal, [x;y] -> iter f x; iter f y
	   | Cnstrnt(_), [x] -> iter f x
	   | _ -> assert false)
    | _ ->
	f a

(* Propositional constants *)

let tt () = mk_tt ()
let ff () = mk_ff ()

let rec ite a b c =
  if is_diseq a then
    ite (Atom.equal (d_diseq a)) c b
  else 
    mk_ite (a,b,c)

(* Building up BDDs *)

module BDD = Bdd.Make(
  struct
    type bdd_node = Term.tnode
    type bdd = Term.t
    type tag = unit
    let compare = fast_cmp
    let high _ = tt()
    let low _ = ff()
    let ite _ = ite
    let is_high = is_tt
    let is_low = is_ff
    let is_ite = is_ite
    let destructure_ite a =
      match a.node with
	| App({node=Interp(Bool(Ite))},[x;y;z]) -> Some(x,y,z)
	| _ -> None
    let fresh _ = mk_fresh BoolDom
  end)
    
let ite = BDD.build ()
     
let neg a =
  if is_equal a then
    Atom.diseq (d_equal a)
  else if is_diseq a then
    Atom.equal (d_diseq a)
  else if is_cnstrnt a then
    let (c,x) = d_cnstrnt a in
    if Cnstrnt.is_arith c then
      match domain_of x with
	| Some(IntDom | RatDom) ->
	    Atom.cnstrnt (Cnstrnt.compl c) x       (* ??? *)    
	| _ ->
	    BDD.neg () a
    else 
      BDD.neg () a
  else
    BDD.neg () a

let conj = BDD.conj ()
let disj = BDD.disj ()
let xor = BDD.xor ()
let imp = BDD.imp ()
let iff = BDD.iff ()


let rec conjl = function
  | [] -> tt()
  | [x] -> x
  | x :: l -> conj x (conjl l)

let rec disjl = function
  | [] -> ff()
  | [x] -> x
  | x :: l -> conj x (disjl l)

let is_neg  = BDD.is_neg
let is_conj = BDD.is_conj
let is_disj = BDD.is_disj
let is_xor  = BDD.is_xor
let is_imp = BDD.is_imp
let is_iff = BDD.is_iff

let d_neg = BDD.d_neg 
let d_conj = BDD.d_conj 
let d_disj = BDD.d_disj
let d_xor = BDD.d_xor
let d_imp = BDD.d_imp
let d_iff = BDD.d_iff


(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | True, [] -> tt()
    | False, [] -> ff()
    | Ite, [x;y;z] -> ite(x,y,z)
    | _ -> assert false

	      
(*s Solving propositional equations and disequalities (xor) *)

let solve_eqn (s1,s2) = BDD.solve () (iff s1 s2)
let solve_deq (s1,s2) = BDD.solve () (xor s1 s2)

let solve (a,b) =
  BDD.solve () (iff a b)

 (*s Lifting conditionals. *)

let rec unary_lift_ite f a =
  match a.node with
    | App({node=Interp(Bool(Ite))},[x;y;z]) ->
	ite(x, unary_lift_ite f y, unary_lift_ite f z)
    | _ ->
	f a

let rec binary_lift_ite f (a,b) =
  match a.node, b.node with
    | App({node=Interp(Bool(Ite))},[x1;y1;z1]), _ ->
	ite(x1, binary_lift_ite f (y1,b), binary_lift_ite f (z1,b))
    | _, App({node=Interp(Bool(Ite))},[x2;y2;z2]) ->
	ite(x2,binary_lift_ite f (a,y2),binary_lift_ite f (a,z2)) 
    | _ ->
	f (a,b)  

let rec ternary_lift_ite f (a,b,c) =
  match a.node, b.node, c.node with
    | App({node=Interp(Bool(Ite))},[x1;y1;z1]), _, _ ->
	ite(x1,ternary_lift_ite f (y1,b,c),ternary_lift_ite f (z1,b,c))
    | _, App({node=Interp(Bool(Ite))},[x2;y2;z2]), _ ->
	ite(x2,ternary_lift_ite f (a,y2,c),ternary_lift_ite f (a,z2,c))
    | _, _, App({node=Interp(Bool(Ite))},[x3;y3;z3])  ->
	ite(x3,ternary_lift_ite f (a,b,y3),ternary_lift_ite f (a,b,z3))
    | _ ->
	f (a,b,c)  

let d_list_ite l =
  let rec loop acc = function
    | [] -> None
    | a :: l ->
	(match a.node with
	   | App({node=Interp(Bool(Ite))},[x;y;z]) ->
	       Some(List.rev acc, (x,y,z), l)
	   | _ ->
	       loop (a :: acc) l)
  in
  loop [] l

let rec nary_lift_ite f l =
  match d_list_ite l with
    | Some(l1,(x,y,z),l2) ->
	ite(x,nary_lift_ite f (l1 @ [y] @ l2),nary_lift_ite f (l1 @ [z] @ l2))
    | None ->
	f l

(*s Database of boolean facts. *)

let is_fresh a = 
  match a.node with
    | Var(_,Fresh(BoolDom)) -> true
    | _ -> false

module B = Th.Make(
  struct
    let name = "b"
    let is_th = is_bool
    let iter = iter
  end)

type t = { find: B.t }

let empty () = { find = B.empty () }

let copy s = { find = B.copy s.find }

let subst_of s = B.subst_of s.find
let use_of s = B.use_of s.find

let apply s = B.apply s.find
let find s = B.find s.find
let inv s = B.inv s.find
let use s = B.use s.find

let is_mem s = B.mem s.find

let extend s = 
  B.extend s.find

(*s Normalize a term with respect to a given substitution. *)

let norm s a =
  let rho = B.subst_of s.find in
  let rec loop a =
    match a.node with
      | App({node=Interp(Bool(op))},l) ->
	  (match op, l with
	     | (True | False), [] -> a
	     | Ite, [x;y;z] ->
		 hom3 a ite loop (x,y,z)
	     | _ ->
		 failwith "Bool.norm: not a Boolean expression")
      | App({node=Pred(p)},l) ->                        (* also normalize atoms on rhs. *)
	  (match p, l with
	     | Equal, [x;y] ->
		 hom2 a Atom.equal loop (x,y)
	     | Cnstrnt(c), [x] ->
		 hom1 a (Atom.cnstrnt c) loop x
	     | _ ->
		 failwith "Bool.norm: not an atomic expression")
      | _ ->
	  try Subst.apply rho a with Not_found -> a
  in
  loop a

(*s Interpreted and uninterpreted equations. *)

let interp (a,b) = 
  assert(not(is_bool(a)));
  is_bool b || (is_fresh b && not(is_fresh a))

let uninterp (a,b) =
  assert(not(is_bool(a)));
  not(is_bool b) && not(is_fresh b)

(*s Merge. *)

type derived = Eqn.t list * (Term.t * Term.t) list * (Cnstrnt.t * Term.t) list

let merge s e =
  Trace.call 7 "Merge(b)" e Pretty.eqn;
  let eqs = ref [] 
  and diseqs = ref []
  and cnstrnts = ref []
  in
  let  new_eq a =
    B.restrict s.find a;
    eqs := d_equal a :: !eqs
  and new_diseq a = 
    B.restrict s.find a;
    diseqs := d_equal a :: !diseqs
  and new_cnstrnt a =
    B.restrict s.find a;
    cnstrnts := d_cnstrnt a :: !cnstrnts
  in
  let derive (a,b) = 
    if is_tt b then
      (if is_equal a then new_eq a
       else if is_cnstrnt a then new_cnstrnt a)
    else if is_ff b then
      (if is_equal a then new_diseq a)
    else if uninterp (a,b) then
      begin
	B.restrict s.find a;
	eqs := (a,b) :: !eqs
      end
  in
  let rec loop ((a,b) as e) =
    if not(a === b) then
      begin
	if is_tt b && is_equal a then
	  eqs := d_equal a :: !eqs
	else if is_tt b && is_cnstrnt a then
	  cnstrnts := d_cnstrnt a :: !cnstrnts
	else if is_ff b && is_equal a then
	  diseqs := d_equal a :: !diseqs
	else if is_ff b && is_cnstrnt a then
	  let (c,x) = d_cnstrnt a in
	  cnstrnts := (Cnstrnt.compl c, x) :: !cnstrnts
	else if interp e then
	  if B.invmem s.find b then             (* [b] is already a find. *)
	    eqs := (a, B.inv s.find b) :: !eqs
	  else 
	    B.union s.find e;
	Term.Set.iter                        
	  (fun u ->    
	     let a' = inv s u in
             let b' = norm s u in
	     derive (a',b');
	     if Atom.inconsistent a' b' then  (*s 'Early' detection of inconsistencies. *)
	       raise Exc.Inconsistent;
	     loop (a', b'))
	  (use s a)
      end
  in
  loop e;
  (!eqs, !diseqs, !cnstrnts)

let ( *** ) s =                           (*s Composing a set of equalities. *)
  List.fold_left 
    (fun (el,dl,cl) e ->
       let (el',dl',cl') = merge s e in
       (el @ el', dl @ dl', cl @ cl'))
    ([],[],[])

let add_eqn s ((a,b) as e) =
 Trace.call 6 "Add(b)" e Pretty.eqn;
  s *** solve (norm s a, norm s b)

let add_eqns s = 
  List.fold_left 
    (fun (el,dl,cl) e ->
       let (el',dl',cl') =  add_eqn s e in
       (el @ el', dl @ dl', cl @ cl'))
    ([],[],[])


(*s Compute the [ite] Form corresponding to a state. *)

let term_of s =
  let rho = B.subst_of s.find in
  Subst.fold (fun (a,b) -> conj (iff a b)) rho (tt())
