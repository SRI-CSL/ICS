
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
 * Author: Harald Ruess, N. Shankar
i*)

(*i*)
open Term
open Three
open Partition
(*i*)

let is_cod a =
  not(is_var a) &&
  List.for_all is_var (args_of a)

let rec map f a =
  if is_var a then
    f(a)
  else 
    let g, l = Term.destruct a in
    Term.mk_app g (Term.mapl f l)


type t = {
  eqs: Solution.t;          (* Equalities for uninterpreted terms. *)
  labels : Term.t Map.t     (* Set of generated variables for labeling. *)
}

(*s Accessors. *)

let solutions s = s.eqs
let labels s = Map.fold (fun x _ -> Set.add x) s.labels Set.empty


(*s [is_label s x] tests if [x] is a label in [s]. *)

let is_label s x = 
  Map.mem x s.labels
 
(*s Applications. *)

let find s = Solution.find s.eqs
let apply s = Solution.apply s.eqs
let use s = Solution.use s.eqs
let inv s = Solution.inv s.eqs


(*s [sigma s f l] encodes the following array axioms:
 [select(update(a,i,x),j)] reduces to [x] if [i = j] 
 and to  [select(a,j)] if [i <> j]. *)

exception Found of Term.t

let rec sigma (v,d,u) f l =
  let a = mk_app f l in
  try
    sigma_select_update (v,d,u) a
  with
      Not_found ->  a

and sigma_select_update (v,d,u) a =  (* throws [Not_found] if no pattern matches. *)
  let (upd, j) = d_select a in       (* otherwise returns simplified term. *)
  try 
    V.iter v
      (fun upd' ->
	 try
	   let (a, i, x) = d_update (apply u upd') in
	   match is_equal (v, d) i j with
	     | Three.Yes -> raise (Found x)
	     | Three.No -> raise (Found(mk_app Sym.mk_select [a; j]))
	     | Three.X -> ()
	 with
	     Not_found -> ())
      (V.find v upd);
    raise Not_found
  with
      Found(b) -> b

and is_equal (v, d) x y =
  let x' = V.find v x in
  let y' = V.find v y in
  if Term.eq x' y' then Three.Yes
  else if D.is_diseq d x' y' then Three.No
  else X


(*s Canonization *)

let rec can ((v,_,u) as s) a =
  if is_var a then
    V.find v a
  else  
    let f, l = Term.destruct a in
    let b = sigma s f (Term.mapl (can s) l) in
    try V.find v (inv u b) with Not_found -> b


(*s Empty state. *)

let empty = {
  eqs = Solution.empty;
  labels = Map.empty
}


(*s Adding an entry [c |-> f(c1,...,cn)] for [c] a fresh label. *)

let extend a s =
  let (x, eqs') = Solution.extend a s.eqs in
  let labels' = Map.add x a s.labels in
  (x, {s with eqs = eqs'; labels = labels'})


(*s Applying congruence axiom [x = y] implies [f(x) = f(y)]. *)

let rec merge (x, y) (v, s, focus) =
  assert(V.eq v x y);
  if Set.is_empty (use s x) || 
    Term.eq (find s x) (find s y) 
  then
    (v, s, focus)
  else 
    let (v', eqs', focus', _) = Solution.fuse map (v, s.eqs, [(x, y)], focus) in
    (v', {s with eqs = eqs'}, focus')


(*s From the equality [x = y] and the facts
 [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
 and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
 that [z1 = k2]. *)

let rec deduce (x, y) s =
  deduce_select_update_eq (x, y)
    (deduce_select_update_eq (y, x) s)

and deduce_select_update_eq (x, y) ((_,u,_) as s) =
  Set.fold
    (fun z1 ((v1,u1,_) as s1) -> 
       try
	 let (upd1, j1) = d_select (apply u1 z1) in
	 if not(V.eq v1 y j1) then
	   s1
	 else 
	   V.fold v1
	     (fun z2 ((v2,u2,_) as s2)  -> 
		try
		  let (a2, i2, k2) = d_update (apply u2 z2) in
		  if V.eq v2 x i2 then
		    let e' = (V.find v2 z1, V.find v2 k2) in
		    merge e' s2
		  else
		    s2
		with
		    Not_found -> s2)
	     (V.find v1 upd1)
	     s1
       with
	   Not_found -> s1)
    (use u x)
    s

(*s Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [v1 = select(u,j',)], [v2 = update(a,i',x)],
 [i = i'], [j = j'], [u = v2], it follows that
 [v2 = v3], wherre [v3 = select(a,j)]. *)

let diseq (i, j) (v, d, u, focus) =
  (v, d, u, focus)
(*
 Set.fold
    (fun v1 ((v, d, u, focus) as acc) -> 
       try
	 let (u, j) = d_select (apply v1 z1) in
	 if not(V.eq v1 y j1) then
	   s1
	 else 
	   V.fold v1
	     (fun z2 ((v2,u2,_) as s2)  -> 
		try
		  let (a2, i2, k2) = d_update (apply u2 z2) in
		  if V.eq v2 x i2 then
		    let e' = (V.find v2 z1, V.find v2 k2) in
		    merge e' s2
		  else
		    s2
		with
		    Not_found -> s2)
	     (V.find v1 upd1)
	     s1
       with
	   Not_found -> s1)
    (use u x)
   s
 *)

 

let close (v, d, s, focus) = 
  let (v', s', vfocus') = 
    Focus.fold_v 
      (fun x ((v, _,_) as acc) ->
	 let y = V.find v x in
	 deduce (x, y) (merge (x, y) acc))
      focus 
      (v, s, V.Focus.empty)
  in
  let (v'', d'', s'', dfocus'') = 
    Focus.fold_d diseq focus (v', d, s', D.Focus.empty)
  in
  (v'', d'', s'', Focus.make vfocus' dfocus'')


(*s Suggested splits on ['a[x:=y][z]'], when 
 [x = z] is not already known. *)

let rec split (v, d, u) =
  Solution.fold
    (fun _ b acc1 ->
       try
	 let (upd1, j1) = d_select b in
	 V.fold v
	   (fun upd2 acc2 ->
	      try
		let (_,i2,_) = d_update (apply u upd2) in
		match is_equal (v, d) i2 j1 with
		  | X -> Atom.Set.add (Atom.mk_equal i2 j1) acc2
		  | Yes -> acc2
		  | No -> acc2
	      with
		  Not_found -> acc1)
	   upd1
	   acc1
       with
	   Not_found -> acc1)
    u.eqs
    Atom.Set.empty

(*s Pretty-printer. *)

let pp fmt s =
  if not(Solution.is_empty s.eqs) then
    begin
      Format.fprintf fmt "\nu:"; 
      Solution.pp fmt s.eqs
    end

(*s Instantiation. *)

let inst v s =
  {s with eqs = Solution.inst v s.eqs}
