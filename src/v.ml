(*
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
 *)

open Term

type t = {
  find : (Term.t * Fact.justification option) Map.t;
  inv : Set.t Map.t; 
  removable: Term.Set.t
}

let eq s t =
  s.find == t.find


let apply s = function
  | App _ -> raise Not_found  (* only variables in domain. *)
  | x -> Map.find x s.find


let find s x =
  let rec loop x =
    try 
      let (y, _) = apply s x in
	if Term.eq x y then y else loop y
    with 
	Not_found -> x
  in
    (loop x, None)


let equality s x =
  let (y, prf) = find s x in
    Fact.mk_equal x y prf


let inv s x = 
  match x with
    | App _ -> raise Not_found
    | _ -> Map.find x s.inv

let removable s = s.removable


let union x y prf s = 
  Trace.msg "v" "Union" (x, y) Term.pp_equal;
  let invy = 
    Set.add x 
      (try Map.find y s.inv with Not_found -> Set.empty)
  in
    {find = Map.add x (y, prf) s.find;
     inv = Map.add y invy s.inv;
     removable = if is_rename x then Set.add x s.removable else s.removable}


let restrict x s =
  try
    let (y, prf) = apply s x in              (* [prf |- x = y]. *)
      Trace.msg "v" "Restrict" x Term.pp;
      let  (y, _) = find s y in              (* now get canonical [y]. *)
      let find' =
	let newfind = Map.remove x s.find in (* remove [x |-> y]. *)
	  try
	    let invx = inv s x in            (* for all [z |-> x], set [z |-> y]. *)
	      Set.fold
		(fun z -> Map.add z (y, None))
		invx
		newfind
	  with
	      Not_found -> newfind
      in
      let inv' =
	let newinv = Map.remove x s.inv in  (** remove the inverse of [x]. *)
	  try 
	    let invy = inv s y in           (* remove [x] from the inverse of [y]. *)
	    let invy' = Set.remove x invy in
	      if Set.is_empty invy' then 
		Map.remove y newinv
	      else 
		Map.add y invy' newinv
	  with
	      Not_found -> newinv
      in
	{find = find'; inv = inv'; removable = Set.remove x s.removable}
  with
      Not_found -> s

let gc f s =
  Set.fold
    (fun x s ->
       if f x then restrict x s else s)
    s.removable
    s
  


(** Variable equality modulo [s]. *)
let is_equal s x y = 
  let (x', _) = find s x 
  and (y', _) = find s y in
    Term.eq x' y'


(** The empty context. *)
let empty = {
  find = Map.empty;
  inv = Map.empty;
  removable = Term.Set.empty
}

let is_empty s = (s.find == Map.empty)


(** Starting from the canonical representative [x' = find s x], the
  function [f] is applied to each [y] in [inv s x'] and the results are
  accumulated. *)
let fold s f x =
  let rec loop y acc =
    let acc' = f y acc in
    try
      Set.fold loop (Map.find y s.inv) acc'
    with
	Not_found -> acc'
  in
  let (y, _) = find s x in
    loop y


(** Adding a binding [a |-> b] to a context [s]. *)
let merge e s =
  let (x, y, prf) = Fact.d_equal e in   (* [prf |- x = y] *)
  let (x', prf1) = find s x in          (* [prf1 |- x = x']. *)
  let (y', prf2) = find s y in          (* [prf2 |- y = y']. *)
  let (x', y') = Term.orient (x', y') in
    if Term.eq x' y' then 
      (Term.Set.empty, s)
    else
      let prf' = Fact.mk_rule "trans" [prf; prf1; prf2] in
	(Term.Set.singleton x', union x' y' prf' s)


(** Extension of the equivalence class for [x]. *)
let ext s x = 
  fold s Set.add x Set.empty


(** Iteration. *)

let iter s f x =
  let rec loop y =
    f y;
    try
      Set.iter loop (Map.find y s.inv)
    with
	Not_found -> ()
  in
  let (y, _) = find s x in
    loop y


let exists s p x =
  let rec loop y =
    p y || 
    try
      Set.exists loop (Map.find y s.inv)
    with
	Not_found -> false
  in
  let (y, _) = find s x in
    loop y


let for_all s p x =
  let rec loop y =
    p y &&
    try
      Set.for_all loop (Map.find y s.inv)
    with
	Not_found -> true
  in
  let (y, _) = find s x in
    loop y


(** Choose an element satisfying some property. *)
exception Found

let choose s p x =
  let result = ref (Obj.magic 1) in
  try
    iter s 
      (fun y ->
	 match p y with
	   | Some(z) -> 
	       result := z;
	       raise Found
	   | None -> ())
      x;
    raise Not_found
  with
      Found -> !result
 

(** Set of canonical representatives with non-trivial equivalence classes.
 These are the variables occurring in the codomain of [find] which are not
 themselves in the domain of [find]. *)
let canrepr s = 
  Map.fold 
    (fun _ (y, _) acc -> 
       if Map.mem y s.find then
	 acc
       else 
	 Set.add y acc)
    s.find
    Set.empty


(** Representation of the equivalence classes as a map with the
 canonical representatives as domain and the corresponding extensions
 in the codomain. The following is not terribly efficient. *)
let partition s =
  Set.fold 
    (fun x -> 
       Map.add x (ext s x)) 
    (canrepr s) 
    Map.empty
    

(** Pretty-printing. *)
let pp fmt s =
  if not(is_empty s) then
    let m = partition s in
    let l = Map.fold 
	      (fun x ys acc -> 
		 match Set.elements ys with
		   | [_] -> acc   (* restrict may lead to singleton sets. *)
		   | yl -> (x, yl) :: acc) 
	      m [] 
    in
    Pretty.string fmt "\nv:";
    Pretty.list
      (fun fmt (x, ys) -> 
	 Term.pp fmt x;
	 Pretty.string fmt ":";
	 Pretty.set Term.pp fmt ys)
      fmt l
