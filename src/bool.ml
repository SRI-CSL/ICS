
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
open Term
open Hashcons
(*i*)

let is_bool a =
  match a.node with
    | Bool _ -> true
    | App({node=Set _}, _) -> true
    | _ -> false

(* Propositional constants *)

let tt () = hc(Bool(True))
let ff () = hc(Bool(False))

	      
(*s Exactly one of the recognizers [is_tt a], [is_ff a], [is_ite a]
  holds for any given non-quantified Boolean connective. *)

let is_tt t =
  match t.node with Bool(True) -> true | _ -> false

let is_ff t =
  match t.node with Bool(False) -> true | _ -> false
    
let is_ite t = 
  match t.node with 
    | Bool Ite _ -> true 
    | _ -> false
 
(*s Destructuring of conditional terms. *)

let d_ite a =
  match a.node with
    | Bool(Ite(x,y,z)) -> (x,y,z)
    | _ -> assert false  



let ite a b c =                      (*s ite(a,b,c) *)
  match Atom.conj a b, Atom.neg_conj a c with
    | Some(x), Some(y) ->
	(match Atom.disj x y with
	   | Some(z) -> z
	   | None -> hc(Bool(Ite(x,tt(),y))))
    | _ ->
	hc(Bool(Ite(a,b,c)))



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
    let destructure_ite p =
      match p.node with
	| Bool(Ite(x,y,z)) -> Some(x,y,z)
	| _ -> None
    let fresh _ = (Var.fresh "z" [])    
  end)

	

	    
let ite = BDD.build ()     
let neg = BDD.neg ()
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


let forall xl p = hc (Bool(Forall (xl, p)))
let exists xl p = hc (Bool(Exists (xl, p)))


(*s Constructor for conditionals. *)

let rec cond (a,b,c) =
  if b === c then b
  else
    match a.node with
      | Bool(True) ->
	  b
      | Bool(False) ->
	  c
      | Bool(Ite(x,y,z)) ->
	  ite(x, cond(y,b,c), cond(z,b,c))
      | _ ->
	  if is_bool b || is_bool c then
	    ite(a,b,c)
	  else
	    hc(Cond(a,b,c))
	      
(*s Solving propositional equations and disequalities (xor) *)

let solve_eqn (s1,s2) = BDD.solve () (iff s1 s2)
let solve_deq (s1,s2) = BDD.solve () (xor s1 s2)

let solve a =
  BDD.solve () a

    
(*s infer new boolean equalities based on the fact that
    [ite(x,p,n) = tt()] implies [(disj p  n) = tt()]. *) 

let rec infer ((a,b) as e) =
  match a.node, b.node with
     | Bool(Ite(_,y,z)), Bool(True) ->
	 e :: infer (disj y z , tt ())
     | _ ->
	 [e]
 
(*s Equalities *)

let rec equal (a,b) =
  if a === b then
    tt()
  else if is_const a && is_const b then
    ff()
  else
    match a.node, b.node with
      | Bool(Ite(x,y,z)), _ ->
	  ite(x, equal(y,b), equal(z,b))
      | _, Bool(Ite(x,y,z)) ->
	  ite(x, equal(a,y), equal(a,z))
      | Bool(Equal(x,y)), Bool(True) ->
	  equal(x,y)
      | Bool(True), Bool(Equal(x,y)) ->
	  equal(x,y)
      | _ ->
	  if Term.cmp a b <= 0 then
	    hc(Bool(Equal(a,b)))
	  else
	    hc(Bool(Equal(b,a)))

let is_equal a =
  match a.node with
    | Bool(Equal _ ) -> true
    | _ -> false

let d_equal a =
  match a.node with
    | Bool(Equal(a,b)) -> (a,b)
    | _ -> raise(Invalid_argument "Bool.d_diseq: Not an equality.")

	  
(*s Disequalities [a <> b] are encoded as [~(a = b)]. *)

let diseq a b =
  neg(equal(a,b))
 
let is_diseq a =
  match a.node with
    | Bool(Ite({node=Bool(Equal _)}, {node=Bool False}, {node=Bool True})) -> true
    | _ -> false

let d_diseq a =
  match a.node with
    | Bool(Ite({node=Bool(Equal(x,y))},
	       {node=Bool False},
	       {node=Bool True})) ->
	(x,y)
    | _ ->
	raise (Invalid_argument "Bool.d_diseq: Not a disequality.")



 (*s Lifting conditionals. *)

let rec unary_lift_ite f a =
  match a.node with
    | Bool(Ite(x,y,z)) ->
	cond(x, unary_lift_ite f y, unary_lift_ite f z)
    | _ ->
	f a

let rec binary_lift_ite f (a,b) =
  match a.node, b.node with
    | Bool(Ite(x1,y1,z1)), _ ->
	cond(x1, binary_lift_ite f (y1,b), binary_lift_ite f (z1,b))
    | _, Bool(Ite(x2,y2,z2)) ->
	cond(x2,binary_lift_ite f (a,y2),binary_lift_ite f (a,z2)) 
    | _ ->
	f (a,b)  

let rec ternary_lift_ite f (a,b,c) =
  match a.node, b.node, c.node with
    | Bool(Ite(x1,y1,z1)), _, _ ->
	cond(x1,ternary_lift_ite f (y1,b,c),ternary_lift_ite f (z1,b,c))
    | _, Bool(Ite(x2,y2,z2)), _ ->
	cond(x2,ternary_lift_ite f (a,y2,c),ternary_lift_ite f (a,z2,c))
    | _, _, Bool(Ite(x3,y3,z3)) ->
	cond(x3,ternary_lift_ite f (a,b,y3),ternary_lift_ite f (a,b,z3))
    | _ ->
	f (a,b,c)  

let d_list_ite l =
  let rec loop acc = function
    | [] -> None
    | a :: l ->
	(match a.node with
	   | Bool(Ite(x,y,z)) ->
	       Some(List.rev acc, (x,y,z), l)
	   | _ ->
	       loop (a :: acc) l)
  in
  loop [] l

let rec nary_lift_ite f l =
  match d_list_ite l with
    | Some(l1,(x,y,z),l2) ->
	cond(x,nary_lift_ite f (l1 @ [y] @ l2),nary_lift_ite f (l1 @ [z] @ l2))
    | None ->
	f l




