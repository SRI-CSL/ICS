
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
(*i*)

let is_cod a =
  not(is_var a) &&
  List.for_all is_var (args_of a)

(*s Equality contexts *)

type t = {
  v : Term.t Map.t;
  u : Term.t Map.t;
  use : Set.t Map.t
}

let v_of s = s.v
let u_of s = s.u
let use_of s = s.use


(*s Use list of a variable *)

let use s x =
  try 
    Map.find x s.use 
  with 
      Not_found -> Set.empty


(*s Lookup of a variable. *)

let rec v s x = 
  try 
    let y = Map.find x s.v in
    if eq x y then y else v s y
  with 
      Not_found -> x


(*s Lookup of a variable with dynamic path compression. *)

let v' s x =
  let rec loop acc x =
    try
      let y = Map.find x s.v in
      if eq x y then
	(acc, y)
      else 
	loop (x :: acc)  y
    with
	Not_found -> (acc,x)
  in
  let (xl,y) = loop [] x in
  let s' = {s with v = List.fold_right 
			 (fun x -> Map.add x y) 
			 xl s.v} 
  in
  (s', y)

(*s Instantiation of variables [x] or terms [f(x1,...,xn)]. *)

let inst s a = 
  if is_var a then 
    a 
  else 
    let f,l = destruct a in
    mk_app f (List.map (v s) l)

(*s Solution set. *)

let solution s =
  Map.fold
    (fun b x acc ->
       let x' = v s x in
       let b' = inst s b in
       (x',b') :: acc)                (* variable on lhs. *)
    s.u
    []



(*s Variable equality modulo [s]. *)

let veq s x y = 
  eq (v s x) (v s y)


(*s Choose an element in set [s] which satisfies [p] *)

exception Found of Term.t
exception Unreachable

let choose p s =
  try
    Set.iter (fun x -> if p x then raise (Found x)) s;
    raise Not_found
  with   
     Found(x) -> x

(*s Lookup. *)

let u s a = 
  if is_var a then 
    v s a
  else if is_const a then
    v s (Map.find a s.u)
  else 
    let f, l = destruct a in
    let b =
      choose
	(fun y ->
	   assert(is_cod y);
	   let g, m = destruct y in
	   (Sym.eq f g) && 
	   try List.for_all2 (veq s) l m with Invalid_argument _ -> false)
	(use s (List.hd l))
    in
    v s (Map.find b s.u)


(*s Merging two equivalence classes. Make sure that whenever 
 [x |-> y] in [v], then [use u x] is a subset of [use u y]. *)

let union s x y =
  assert(is_var x && is_var y); 
  let (x,y) = orient (x,y) in  
  Trace.msg 6 "Union(u)" (x,y) (Pretty.eqn Term.pp);
  let ux = use s x in
  let uy = use s y in
  let uxy = Set.union ux uy in
  let use' = if uy == uxy then s.use else Map.add y uxy s.use in
  let v' = let y' = try Map.find y s.v with Not_found -> y in
  Map.add x y' s.v
  in
  {s with use = use'; v = v'}


(*s The empty context. *)

let empty = {
  v = Map.empty;
  u = Map.empty; 
  use = Map.empty
}


(*s Adding a binding [a |-> b] to a context [s]. *)


let rec add a b s =
  let (u',use') = addu a b (s.u, s.use) in
  {s with u = u'; use = use'}

and addu a b (u,use) =
  (Map.add a b u,
   List.fold_left 
     (fun acc x -> 
	try 
	  let ux = Map.find x use in
	  let ux' = Set.add a ux in
	  if ux == ux' then acc else Map.add x ux' acc
	with
	    Not_found -> 
	      Map.add x (Set.singleton a) acc)
     use 
     (args_of a))



(*s Removing a binding [a |-> b] in [s]. *)

let restrict a s = 
  Trace.msg 6 "Restrict(u)" a Term.pp;
  {s with 
     u = Map.remove a s.u;
     use = List.fold_left
	     (fun acc x ->
		try
		  let ux = Map.find x s.use in
		  let ux' = Set.remove a ux in
		  if ux == ux' then acc else Map.add x ux' acc
		with
		    Not_found ->
		      acc)
	     s.use
	     (args_of a) }


(*s Canonization *)

let rec can s a =
  if is_var a then
    v s a
  else  
    let b = mk_app (sym_of a) (List.map (can s) (args_of a)) in
    try v s (u s b) with Not_found -> b



(*s Merging of equalities [(a,b)] and congruence-close. *)

let congruent s a b =
  assert(not(is_var a) && not(is_var b));
  Sym.eq (sym_of a) (sym_of b) &&
  try
    List.for_all2 (veq s) (args_of a) (args_of b)
  with
      Invalid_argument _ -> false
	

let merge e s =  
  Trace.msg 5 "Merge(u)" e Veq.pp;
  let (a,b) = Veq.destruct e in
  assert(is_var a && is_var b);
  let addv x y xl =              (* do not add original variable equality. *)
    let (x,y) = orient (x,y) in
    if eq x a && eq y b then xl else Veqs.add x y xl
  in
  let rec merge1 a b (s,xl) =
    if eq a b then
      (s,xl)
    else
      let s' = union s a b in
      Set.fold
	(fun x (s1,xl1) ->
	   Set.fold
	     (fun y (s2,xl2) ->
		if congruent s2 x y then 
		  let a' = u s2 x in   (* [u] never raises [Not_found] here. *)
		  let b' = u s2 y in
		  if eq a' b' then 
		    (s2, xl2) 
		  else 
		    merge1 a' b' ((* restrict x *) s2, xl2)
		else 
		  (s2, xl2))
	     (use s1 b)
	     (s1,xl1))
	(use s a)
	(s', addv a b xl)
  in
  Trace.call 5 "Merge(u)" (a,b) (Pretty.eqn Term.pp);
  let (s', xl) = merge1 a b (s, Veqs.empty) in
(*  Trace.exit 5 "Merge(u)" xl (Pretty.list Veq.pp); *)
  (s', xl)

(*s Creating a fresh label term. *)

let labels = ref Set.empty
let _ = Tools.add_at_reset (fun () -> labels := Set.empty)

let mk_label = 
  let name = Name.of_string "v" in
  fun () -> 
    let v = mk_fresh name in
    labels := Set.add v !labels;
    v

let is_label x = Set.mem x !labels


(*s Adding an entry [f(c1,...,cn) |-> c] for [c] fresh. *)

let extend a s =
  Trace.call 5 "Extend(u)" a Term.pp;
  let c = mk_label () in
  Trace.exit 5 "Extend(u)" c Term.pp;
  (c, add a c s)


(*s Compression by instantiating variables on both the lhs and rhs of [u]
 with their canonical representatives w.r.t. to [v] *)

let compress s =  
  let (u',use') = 
    Map.fold 
      (fun x y acc ->
	 let x' = inst s x in
	 let y' = inst s y in
	 addu x' y' acc)
      s.u 
      (Map.empty, Map.empty)
  in
  let v' = 
    Map.fold
      (fun x _ acc ->
	 if is_label x then
	   Map.remove x acc
	 else 
	   acc)
      s.v s.v
  in 
  {v = v'; u = u'; use = use'}


(*s Pretty-printing. *)

let pp fmt s = 
  let to_list m = Map.fold (fun x y acc -> (x,y) :: acc) m [] in
  if not(Map.empty == s.v) then
    begin
      Pretty.string fmt "v:"; 
      Pretty.map Term.pp Term.pp fmt (to_list s.v);
      Pretty.string fmt "\n"
    end;
  if not(Map.empty == s.u) then
    begin
      Pretty.string fmt "u:"; 
      Pretty.map Term.pp Term.pp fmt (to_list s.u);
      Pretty.string fmt "\n"
    end



