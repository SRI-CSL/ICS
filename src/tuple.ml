
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
open Hashcons
open Term
open Eqn
(*i*)

let is_tuple a =
  match a.node with
    | App({node=Interp(Tuple _)},_) -> true
    | _ -> false

(* apply [f] at uninterpreted positions. *)

let rec iter f a = 
  match a.node with
    | App({node=Interp(Tuple(op))},l) ->
	(match op, l with
	   | Product, _::_::_ -> List.iter (iter f) l
	   | Proj _, [x] -> f x
	   | _ -> assert false)
    | _ ->
	f a
 

(*s Smart constructors for tuples and projections. *)

let tuple = mk_tuple

let rec proj i n a =
  match a.node with
    | App({node=Interp(Tuple(Product))},l) ->
	List.nth l i
    | _ ->
	mk_proj i n a


(*s Sigmatizing. *)

let sigma op l =
  match op, l with
    | Product, _::_::_ -> tuple l
    | Proj(i,n), [x] -> proj i n x
    | _ -> assert false

(*s Normalize a term with respect to a given substitution. *)

let norm s a =
  let rec loop a =
    match a.node with
      | App({node=Interp(Tuple(op))},l) ->
	  (match op, l with
	     | Product, _::_::_ -> 
		 homl a tuple loop l 
	     | Proj(i,n), [x] ->
		 hom1 a (proj i n) loop x
	     | _ ->
		 failwith "Tuple.norm: not a tuple expression")
      | _ ->
	  try
	    Subst.apply s a
	  with
	      Not_found -> a
  in
  loop a

(*s Solving tuples. *) 

let add ((a,b) as e) el =
  if a === b then 
    el
  else
    match b.node with
      | App({node=Interp(Tuple(Product))},l)
	  when List.exists (fun y -> a === y) l ->
	    raise Exc.Inconsistent
      | _ -> e :: el

(*s [solve (s, (t0,...,tn)) = \list{(proj s 0, t0),...,(proj s n, tn)}] *)

let tuple_solve s l =
  let n = List.length l in
  let (eqs, _) = 
    List.fold_right
      (fun t (acc, i) -> (add (proj i n s, t) acc, i + 1)) l ([], 0)
  in
  eqs

(*s [solve ((s0,...,sn), (t0,...,tn)) = [(s0,t0),...(sn,tn)] *)  

let tuple_tuple_solve al bl = 
   List.fold_right2 (fun a b acc -> add (a, b) acc) al bl []

(*s [solve (proj i n s, t) = (s, \list{c0,...,t,...cn-1})]
     where [ci] are fresh, [s] at [i]-th position. *)

let proj_solve i n s t =
  let rec args j acc =
    if j = -1 then acc
    else
      let a =
	if i = j
	then t
	else
	  mk_rename_var "_p" (proj j n s)
      in
      args (j - 1) (a :: acc)
  in
  add (s, tuple (args (n - 1) [])) []

let solve ((a,b) as e) =
  match a.node, b.node with
    | App({node=Interp(Tuple(Product))},al),
      App({node=Interp(Tuple(Product))},bl) ->
	tuple_tuple_solve al bl
    | App({node=Interp(Tuple(Proj(i,n)))},[x]),_ ->
	proj_solve i n x b
    | _, App({node=Interp(Tuple(Proj(i,n)))},[y]) ->
	proj_solve i n y a
    | App({node=Interp(Tuple(Product))},al), _ ->
	tuple_solve b al
    | _, App({node=Interp(Tuple(Product))},bl) ->
	tuple_solve a bl
    | _ ->
	if a <<< b then [(b,a)] else [e]


module T = Th.Make(
  struct
    let name = "t"
    let is_th = is_tuple
    let iter = iter
  end)


(*s Database of arithmetic facts. *)

type t = { find: T.t }

let empty () = { find = T.empty () }

let copy s = { find = T.copy s.find }

let subst_of s = T.subst_of s.find
let use_of s = T.use_of s.find

let apply s = T.apply s.find
let find s = T.find s.find
let inv s = T.inv s.find
let use s = T.use s.find

let nrm s = norm (T.subst_of s.find)

let is_external (a,b) =
  not(is_tuple a) && not(is_tuple b)

(*s Interpreted equations are the ones that are installed into the database.
 Notice, that equalities between two fresh variables are not considered to be
 interpreted, since they only need propagation, but they are not
 installed into the database. An uninterpreted equation is one that
 is propagated. Equalities between fresh variables just need to be propagated.
 Install equality [x = e], where [x] is uninterpreted, and [e] is interpreted.
 Equalities between uninterpreted term are added to set of generated equalities.
 Now, propagate the equality [a = b] for all bindings [inv s u |-> u] such that
 [a] occurs interpreted in [u]. *)

let merge s e = 
  Trace.call 7 "Merge(t)" e Pretty.eqn;
  let eqs = ref [] in
  let rec loop ((a,b) as e) =
    if not(a === b) then
      begin
	if not(is_tuple a) && is_tuple b then
	  (try 
	     let a' = T.inv s.find b in   
	     assert(not(is_tuple a'));
	     eqs := cons (a,a') !eqs
	   with
	      Not_found ->
		T.union s.find e);         
	Term.Set.iter                        
	  (fun u ->   
	     let a' = inv s u in 
	     assert(not(is_tuple a'));
             let b' = nrm s u in
	     if not(is_tuple b') then
	       begin
		 T.restrict s.find a';
		 eqs := cons e !eqs
	       end; 
	     loop (a',b'))
	  (use s a)
      end
  in
  loop e;
  !eqs

let ( *** ) s =      
  List.fold_left (fun acc e -> merge s e @@ acc) []

let extend s ((a,b) as e) = 
  Trace.call 7 "Ext(t)" e Pretty.eqn;
  T.extend s.find e

let process s ((a,b) as e) =
  Trace.call 7 "Add(t)" e Pretty.eqn;
  let sl = solve (nrm s a, nrm s b) in
  let el1 = List.filter is_external sl in
  let el2 = s *** sl in
  Trace.exit 7 "Add(t)" (el1 @@ el2) (Pretty.list Pretty.eqn);
  el1 @@ el2

(*s Propagating  a set of uninterpreted equalities. *)

let propagate1 s ((a,b) as e) =                      
  Trace.call 7 "Prop(t)" e Pretty.eqn;
  let sl = solve (nrm s a, nrm s b) in
  let el1 = Eqn.remove e (List.filter is_external sl) in
  let el2 = s *** sl in
  Trace.exit 7 "Prop(t)" (el1 @@ el2) (Pretty.list Pretty.eqn);
  el1 @@ el2

let propagate s =
  List.fold_left (fun acc e -> propagate1 s e @@ acc) []
