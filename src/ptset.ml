
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Jean-Christophe Filliatre
 i*)

(*i*)
open Hashcons
(*i*)

type 'a t =
  | Empty
  | Leaf of 'a hashed
  | Branch of int * int * 'a t * 'a t

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton k = Leaf k

let zero_bit k m = (k land m) == 0

let mem k t = 
  let ktag = k.tag in
  let rec lookup = function
    | Empty -> false
    | Leaf j -> ktag == j.tag
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

let add k t =
  let ktag = k.tag in
  let rec ins = function
    | Empty -> Leaf k
    | Leaf j as t -> 
	if j.tag == ktag then t else join (ktag, Leaf k, j.tag, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix ktag p m then
	  if zero_bit ktag m then 
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (ktag, Leaf k, p, t)
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
    | Leaf j as t -> if ktag == j.tag then Empty else t
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
  | Leaf k, t -> add k t
  | t, Leaf k -> add k t
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
  | Leaf k -> f k
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1
      
let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf k -> f k accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

let map f s =
  fold (fun x acc -> add (f x) acc) s empty
	
exception Found

let filter p s =
  fold (fun x acc -> if p x then add x acc else acc) s empty
    
let inter s1 s2 = filter (fun x -> mem x s2) s1
		    
let to_list s = fold (fun x acc -> x :: acc) s []
	  
let exists p s =
  try
    iter (fun a -> if p a then raise Found) s;
    false
  with
      Found -> true

let for_all p s =
  not (exists (fun x -> not (p x)) s)
	  
let sub s1 s2 =
  for_all (fun x -> mem x s2) s1

let equal s1 s2 =
  sub s1 s2 && sub s2 s1
    
let pp p fmt s =
  let rec loop = function
    | [] -> ()
    | [a] -> p fmt a
    | a :: l -> p fmt a; Format.fprintf fmt "@ , @ "; loop l
  in
  Format.fprintf fmt "@[{"; loop (to_list s); Format.fprintf fmt "}@]"

let iter2 f s = iter (fun x -> iter (fun y -> f x y) s) s

let choose p s =
  let res = ref None in
  try
    iter (fun a -> 
	    if p a then 
	      begin 
		res := Some(a); 
		raise Found end) 
      s;
    raise Not_found
  with
      Found -> 
	(match !res with 
	   | Some(a) -> a 
	   | _ -> assert false)

let destructure s =
  let x = choose (fun _ -> true) s in
  x, remove x s

let cmp s1 s2 =
  if sub s1 s2 then Binrel.Sub
  else if sub s2 s1 then Binrel.Super
  else if equal s1 s2 then Binrel.Same
  else if is_empty (inter s1 s2) then Binrel.Disjoint
  else Binrel.Overlap 

let is_disjoint s1 s2 =
  is_empty (inter s1 s2)




