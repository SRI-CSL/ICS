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
  try
    (match x with
       | App _ -> raise Not_found
       | _ -> Map.find x s.inv)
  with
      Not_found -> Term.Set.empty

let removable s = s.removable

let is_canonical s x = 
  try
    let (y, _) = apply s x in
      Term.eq x y
  with
      Not_found -> true


(** {6 Basic Data Manipulations} *)

let union (x, y, rho) s = 
  Trace.msg "v" "Union" (x, y) Term.Equal.pp;
  assert(not(Term.eq x y));
  {find = Map.add x (y, rho) s.find;
   inv = Map.add y (Set.add x (inv s y)) s.inv;
   removable = if Term.Var.is_internal x then Set.add x s.removable else s.removable}

(** Remove a binding [x |-> y] *)
let remove x s =
  try
    let (y, rho) = apply s x in
      Trace.msg "v" "Remove" (x, y) Term.Equal.pp;
      let find' = Term.Map.remove x s.find in
      let invy' = Term.Set.remove x (inv s y) in
      let inv' = 
	if Term.Set.is_empty invy' then 
	  Term.Map.remove y s.inv
	else 
	  Term.Map.add y invy' s.inv
      in
	{find = find'; 
	 inv = inv'; 
	 removable = Term.Set.remove x s.removable}
  with
      Not_found -> s
    

(** For [zi |-> x |-> y |-> ... |-> y'] set [zi |-> y'] and remove [x]. *)
let restrict x s =
  assert(Term.Set.mem x s.removable);
  if is_canonical s x then s else
    let s' =
      Term.Set.fold
	(fun z acc ->
	   let (z', tau') = find s z in  (* change [z |-> x] to [z |-> z'] *)
	     union (z, z', tau') (remove z acc))
	(inv s x) s
    in
      remove x s'   (* now, break the link [x |-> y]. *)
 

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

let pretty = ref true

(** Representation as [z |-> {x1, ...,xn}] with [z] the
  canonical variables, and [xi] the variables with [z]
  as canonical representative. *)
let to_map s =
  Map.fold  
    (fun x (y, rho) acc ->         (* [rho |- x = y] *)
       let (z, tau) = find s y in  (* [tau |- y = z] *)
	 try
	   let zs = Term.Map.find z acc in
	   let zs' = Term.Set.add x zs in
	     Term.Map.add z zs' acc
	 with
	     Not_found -> 
	       Term.Map.add z (Term.Set.singleton x) acc)
    s.find Term.Map.empty

let to_list s =
  Map.fold 
    (fun x xs acc -> 
       (x, Term.Set.elements xs) :: acc) 
    (to_map s) []

let pp fmt s =
  if not(is_empty s) then
    begin
      Pretty.string fmt "\nv:";
      if !pretty then
	let l = to_list s in
	  Pretty.map Term.pp (Pretty.set Term.pp) fmt l
      else 
	let l = 
	  Map.fold 
	    (fun x (y, rho) acc -> (x, (y, rho)) :: acc)
	    s.find []
	in
	  Pretty.map
	    Term.pp 
	    (Pretty.pair Term.pp Justification.pp)
	    fmt 
	    l
    end 
	  
