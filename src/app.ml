
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
  Bool.diseq i j === Bool.tt()

let finite s x =
  if Term.Set.mem x s then
    Bool.tt()
  else if Term.Set.for_all (deriv_diseq x) s  then
    Bool.ff()
  else
    hc(App(Sets.finite s,[x]))



   (*s Function application. *)

let rec appl a l =
  if l = [] then a
  else
    match a.node with
      | App(b,m) ->                  (* Uncurrying *)
	  appl b (m @ l)
      | Update(b,j,v) ->
	  let i = Tuple.tuple l in
	  if i === j then
	    v
	  else if deriv_diseq i j then
	    appl b l
	  else    
	    Bool.cond(Bool.equal(i,j), v, appl b l)
      | Set s ->
	  (match s with
	     | Empty _ ->
		 Bool.ff()
	     | Full _ ->
		 Bool.tt()
	     | Cnstrnt(c) ->
		 Cnstrnt.app c (Tuple.tuple l)
	     | Finite(s) ->
		 finite s (Tuple.tuple l)
	     | SetIte(_,s1,s2,s3) ->
		 Bool.ite(appl s1 l, appl s2 l, appl s3 l))
      | _ ->
	  hc(App(a,l))

let app a l =
  Bool.nary_lift_ite (appl a) l

let update =
  let rec upd (a,i,u) =
    match a.node with
      | Update(b,j,v) when i === j  ->
	  upd (b,i,u)
      | _ ->
	  hc(Update(a,i,u))
    in
    Bool.ternary_lift_ite upd
	 













