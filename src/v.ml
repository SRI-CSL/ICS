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

(* Elements of type [t] represent sets of variable equalities [x = y] such that
  [x] is syntactically different from [y], that is, {!Term.eq}[ x y] does not hold.
  Also, these sets are functional in the sense that whenever both [x1 = y] and [x2 = y]
  are represented, then [x1] equals [x2]. The [find] datastructure is used to represent
  equalities such as [x = y] by a binding [x |-> (y, rho)], where [rho] is a justification
  of the equality [x = y]; see also {!Fact.justification}. The [inv] map is used to 
  memoize the inverse find, that is, [x] is in [inv s y] iff [find] represents [x = y].
  Finally, [removable] contains internally generated renaming variables that are not 
  canonical w.r.t to the given set of equalities.  These variables may be garbage 
  collected. *)
  
type t = {
  find : (Term.t * Justification.t) Map.t;
  inv : Set.t Map.t; 
  removable: Term.Set.t
}


(** {6 Identity} *)

let eq s t = s.find == t.find


(** {6 Accessors} *)

let apply s = function
  | App _ -> raise Not_found           (* only variables in domain. *)
  | x -> Map.find x s.find

let find s x =
  let rec loop x (y, rho) =            (* [rho |- x = y] *)
    try 
      let (z, sigma) = apply s y in    (* [sigma |- y = z] *)
	Trace.msg "v'" "find" (y, z) (Pretty.pair Term.pp Term.pp);
	assert(not(Term.eq y z));
	let tau = Justification.trans (x, y, z) rho sigma in
	  loop x (z, tau)
    with 
	Not_found -> (y, rho)
  in
    loop x (Justification.Eqtrans.id x)

let inv s x = 
  match x with
    | App _ -> raise Not_found
    | _ -> Map.find x s.inv

let removable s = s.removable


(** {6 Basic Data Manipulations} *)

let union (x, y, rho) s = 
  Trace.msg "v" "Union" (x, y) Term.Equal.pp;
  assert(not(Term.eq x y));
  let invy = 
    Set.add x 
      (try Map.find y s.inv with Not_found -> Set.empty)
  in
    {find = Map.add x (y, rho) s.find;
     inv = Map.add y invy s.inv;
     removable = if Term.Var.is_internal x then Set.add x s.removable else s.removable}


let restrict x s =
  try
    let (y', rho) = apply s x in                  (* [rho |- x = y']. *)
      Trace.msg "v" "Restrict" x Term.pp;
      let  (y, sigma) = find s y' in              (* sigma |- y' = y *)
      let find' =
	let newfind = Map.remove x s.find in      (* remove [x |-> y]. *)
	  try
	    let invx = inv s x in                 (* for all [z |-> x], set [z |-> y]. *)
	    let rhosigma = Justification.trans (x, y', y) rho sigma in 
	      Set.fold                            (* [rhosigma |- x = y] *) 
		(fun z ->
		   let (_, theta) = apply s z in  (* [theta |- z = x] *)
		     Map.add z (y, Justification.trans (z, x, y) theta rhosigma))
		invx
		newfind
	  with
	      Not_found -> newfind
      in
      let inv' =
	let newinv = Map.remove x s.inv in  (* remove the inverse of [x]. *)
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


(** {6 Variable equality modulo [s]} *)

let is_equal s x y = 
  let (x', rho) = find s x         (* [rho |- x = x'] *)
  and (y', sigma) = find s y in 
    if Term.eq x' y' then          (* [sigma |- y = x'] *)
      Some(Justification.trans (x, x', y) rho (Justification.sym (x', y) sigma))
    else 
      None


(** {6 Context Manipulations} *)

let empty = {
  find = Map.empty;
  inv = Map.empty;
  removable = Term.Set.empty
}

let is_empty s = (s.find == Map.empty)

(** Garbage collection *)
let gc f s =
  Set.fold
    (fun x s ->
       if f x then restrict x s else s)
    s.removable
    s
   
(** Adding a binding [x |-> y] to a context [s]. *)
let merge e s =                (* [rho |- x = y] *)
  assert (Fact.Equal.is_var e);
  Trace.msg "v" "Merge" e Fact.Equal.pp;
  let e' = Fact.Equal.map (find s) e in
  let (x', y', rho') = Fact.Equal.destruct e' in
    if Term.eq x' y' then s else
      begin
	Fact.Eqs.push None e';
	union (x', y', rho') s
      end 
	  

(** {6 Functionals} *)

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

    
(** {6 Pretty-printing.} *)

let pp fmt s =
  let canrepr s = 
    Map.fold 
      (fun _ (y, _) acc -> 
	 if Map.mem y s.find then
	   acc
	 else 
	   Set.add y acc)
      s.find
      Set.empty
  in
  let partition s =
    Set.fold 
      (fun x -> 
	 Map.add x (ext s x)) 
      (canrepr s) 
      Map.empty
  in
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
