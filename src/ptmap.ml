
(*
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
 *)

(*s Maps of integers implemented as Patricia trees. *)

(*i*)
open Hashcons
(*i*)

type ('a,'b) t =
  | Empty
  | Leaf of 'a hashed * 'b
  | Branch of int * int * ('a,'b) t * ('a,'b) t

let empty = Empty

let zero_bit k m = (k land m) == 0

let mem k t = 
  let ktag = k.tag in
  let rec lookup = function
    | Empty -> false
    | Leaf (j,_) -> ktag == j.tag
    | Branch (_, m, l, r) -> lookup (if zero_bit ktag m then l else r)
  in
  lookup t

let find k t = 
  let ktag = k.tag in
  let rec lookup = function
    | Empty -> raise Not_found
    | Leaf (j,x) -> if ktag == j.tag then x else raise Not_found
    | Branch (_, m, l, r) -> lookup (if zero_bit ktag m then l else r)
  in
  lookup t

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then 
    Branch (mask p0 m, m, t0, t1)
  else 
    Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let add k x t =
  let ktag = k.tag in
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j,_) as t -> 
	if j.tag == ktag then Leaf (k,x) else join (ktag, Leaf (k,x), j.tag, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix ktag p m then
	  if zero_bit ktag m then 
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (ktag, Leaf (k,x), p, t)
  in
  ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let ktag = k.tag in
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t -> if ktag == j.tag then Empty else t
    | Branch (p,m,t0,t1) as t -> 
	if match_prefix ktag p m then
	  if zero_bit ktag m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let rec merge = function
  | Empty, t  -> t
  | t, Empty  -> t
  | Leaf (k,x), t -> add k x t
  | t, Leaf (k,x) -> add k x t
  | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
      if m == n && match_prefix q p m then
	(* The trees have the same prefix. Merge the subtrees. *)
	Branch (p, m, merge (s0,s1), merge (t0,t1))
      else if m < n && match_prefix q p m then
	(* [q] contains [p]. Merge [t] with a subtree of [s]. *)
	if zero_bit q m then 
	  Branch (p, m, merge (s0,t), s1)
        else 
	  Branch (p, m, s0, merge (s1,t))
      else if m > n && match_prefix p q n then
	(* [p] contains [q]. Merge [s] with a subtree of [t]. *)
	if zero_bit p n then
	  Branch (q, n, merge (s,t0), t1)
	else
	  Branch (q, n, t0, merge (s,t1))
      else
	(* The prefixes disagree. *)
	join (p, s, q, t)

let union s t = merge (s,t)

let rec cardinal = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)
      
let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)
