
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

type t = {
  find : Term.t Term.Map.t;
  inv : Term.Set.t Term.Map.t;
  changed : Term.Set.t;
  removable : Term.Set.t
}

let eq s t =
  s.find == t.find

(*s Canonical representative of equivalence class containing [x] *)

let rec find s x =
  try 
    let y = Term.Map.find x s.find in
    if Term.eq x y then y else find s y
  with 
      Not_found -> x


(*s Union of equivalence classes. *)

let union x y s =
  let find' = Term.Map.add x y s.find in
  let invy = Term.Set.add x (try Term.Map.find y s.inv with Not_found -> Term.Set.empty) in
  let inv' = Term.Map.add y invy s.inv in
  let changed' = Term.Set.add x s.changed in
  let removable' = 
    if Term.is_fresh_var x then
      Term.Set.add x s.removable
    else 
      s.removable
  in
  {find = find'; 
   inv = inv'; 
   changed = changed'; 
   removable = removable'}


(*s Canonical representative with dynamic path compression. *)

let find' s x =
  let rec loop acc x =
    try
      let y = Term.Map.find x s.find in
      if Term.eq x y then
	(acc, y)
      else 
	loop (x :: acc)  y
    with
	Not_found -> (acc, x)
  in
  let (xl, y) = loop [] x in
  let s' = List.fold_right (fun x -> union x y) xl s in
  (s', y)


(*s Variable equality modulo [s]. *)

let is_equal s x y = 
  Term.eq (find s x) (find s y)


(*s The empty context. *)

let empty = {
  find = Term.Map.empty;
  inv = Term.Map.empty;
  changed = Term.Set.empty;
  removable = Term.Set.empty
}

let is_empty s = 
  (s.find == Term.Map.empty)

(*s Return the set of removable variables. *)

let removable s = s.removable

(*
let normalize s =
  let find' = Set.fold Term.Map.remove s.removable s.find in
  let invy = Term.Set.remove x (try Term.Map.find y s.inv with Not_found -> Term.Set.empty) in
  let inv' = Term.Map.add y invy s.inv in
  let changed' = Term.Set.diff s.changed s.removable in
  let removable' = Term.Set.empty in
  {find = find'; 
   inv = inv'; 
   changed = changed'; 
   removable = removable'}
 *)

(*s Starting from the canonical representative [x' = find s x], the
  function [f] is applied to each [y] in [inv s x'] and the results are
  accumulated. *)

let fold s f x =
  let rec loop y acc =
    let acc' = f y acc in
    try
      Term.Set.fold loop (Term.Map.find y s.inv) acc'
    with
	Not_found -> acc'
  in
  loop (find s x)



(*s Adding a binding [a |-> b] to a context [s]. *)

let merge e s =
  let (x, y,_) = Fact.d_equal e in
  let x' = find s x and y' = find s y in
  if Term.eq x' y' then 
    s
  else
    union x' y' s


(*s Extension of the equivalence class for [x]. *)

let ext s x = fold s Term.Set.add x Term.Set.empty


(*s Iteration. *)

let iter s f x =
  let rec loop y =
    f y;
    try
      Term.Set.iter loop (Term.Map.find y s.inv)
    with
	Not_found -> ()
  in
  loop (find s x)


(*s Exists/Forall *)

let exists s p x =
  let rec loop y =
    p y || 
    try
      Term.Set.exists loop (Term.Map.find y s.inv)
    with
	Not_found -> false
  in
  loop (find s x)


let for_all s p x =
  let rec loop y =
    p y &&
    try
      Term.Set.for_all loop (Term.Map.find y s.inv)
    with
	Not_found -> true
  in
  loop (find s x)

(*s Changed. *)

let is_changed s x = 
  Term.Set.mem x s.changed

let changed s = s.changed

let reset s = {s with changed = Term.Set.empty}

(*s Choose an element satisfying some property. *)

exception Found of Term.t

let choose s p x =
  try
    iter s (fun y -> if p y then raise(Found y)) x;
    raise Not_found
  with
      Found(y) -> y
 

(*s Set of canonical representatives with non-trivial equivalence classes.
 These are the variables occurring in the codomain of [find] which are not
 themselves in the domain of [find]. *)

let canrepr s = 
  Term.Map.fold 
    (fun _ y acc -> 
       if Term.Map.mem y s.find then
	 acc
       else 
	 Term.Set.add y acc)
    s.find
    Term.Set.empty


(*s Representation of the equivalence classes as a map with the
 canonical representatives as domain and the corresponding extensions
 in the codomain. The following is not terribly efficient. *)

let partition s =
  Term.Set.fold 
    (fun x -> 
       Term.Map.add x (ext s x)) 
    (canrepr s) 
    Term.Map.empty
    

(*s Pretty-printing. *)

let pp fmt s =
  if not(is_empty s) then
    let m = partition s in
    let l = Term.Map.fold (fun x ys acc -> (x, Term.Set.elements ys) :: acc) m [] in
    Pretty.string fmt "\nv:";
    Pretty.map Term.pp (Pretty.set Term.pp) fmt l


(*s Only external variables. *)

let external_of s =
  Term.Map.fold 
    (fun x y acc ->
       if Term.is_fresh_var x && not(Term.is_fresh_var (find s y)) then
	 acc
       else if Term.is_fresh_var x && not(Term.eq y (find s y)) then
	 acc
       else 
	 union x y acc)
    s.find
    empty

