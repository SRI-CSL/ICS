
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
open Mpa
open Term
(*i*)

let deriv_diseq i j =
  Atom.diseq(i,j) === Bool.tt()



   (*s Function application. *)

let rec appl dom props a l =
  if l = [] then 
      a
  else
    match a.node with
      | App({node=Builtin(Update)},[b;j;v]) ->
	  let i = Tuple.tuple l in
	  update_app dom (b,j,v) i
      | App({node=Uninterp(b,None,None)},m) ->
	  appl None None b (m @ l)
      | _ ->
	  let l' = normalize props a l in
	  mk_uninterp dom props a l'


and normalize props a l =
  let flatten a l =
    List.fold_right 
      (fun x acc ->
	 match x.node with
	   | App({node=Uninterp(y,_,_)}, l) when a === y -> 
		 l @ acc
	   | _ -> 
	       x :: acc)
      l
      []  
  in
  match props with
    | None -> l
    | Some(A) ->
	flatten a l
    | Some(AC) ->
	Sort.list (<<<) (flatten a l)
    | Some(C) ->
	Sort.list  (<<<) l

and update_app dom (b,j,v) i =
  if i === j then
    v
  else if deriv_diseq i j then
    appl dom None b [i]
  else    
    Bool.ite(Atom.equal(i,j), v, appl dom None b [i])


let uninterp dom props a =
  Bool.nary_lift_ite (appl dom props a)

let sigma f l =
  match f.node with
    | Uninterp(a,d,p) ->
	uninterp d p a l
    | Interp(sym) ->
	(match sym with
	   | Arith(op) -> Arith.sigma op l
	   | Tuple(op) -> Tuple.sigma op l
           | Bool(op) -> Bool.sigma op l)
    | Pred(p) ->
	(match p, l with
	   | Equal, [x;y] -> Atom.equal (x,y)
	   | Cnstrnt(c), [x] -> Atom.cnstrnt c x
	   | _ -> assert false)
    | Builtin(f) -> 
	Builtin.sigma f l






