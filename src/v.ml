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

(** Elements of type [t] represent sets of directed variable equalities [y = x] 
  such that [x] is less than [y] according to the variable comparison {!Var.cmp}. 
  These sets are functional in the sense that whenever both [x1 = y] and [x2 = y]
  are represented, then [x1] equals [x2].  
  - The [post] datastructure is used to represent equalities such as [x = y] 
  by a binding [x |-> (y, rho)], where [rho] is a justification of the equality 
  [x = y]; see also {!Jst.t}. 
  - The [pre] map is used to memoize the inverse find, that is, [x] is in [pre s y] iff 
  [x |-> (y, _)] is in [post]. 
  - Finally, [removable] contains internally generated renaming variables that are
  not canonical w.r.t to the given set of equalities.  These variables may be 
  {i garbage collected}. 
*)
  

(** Switch for enabling/disabling garbage collection of noncanonical, internal
  variables. *)
let garbage_collection_enabled = ref true
				   

(** The [pre] of a canonical term [x] contains all noncanonical terms [y]
  equivalent with [x].  This set is represented as a tree with
  terms at the leafs. A term does occur at most once in [pre].  This 
  enables {!V.merge} to be fast, whereas {!V.restrict} requires a linear 
  scan of [pre] but at most one node is changed. *)
module Pre = struct

  type t = 
    | Empty
    | Node of Term.t
    | Union of t * t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let singleton a = Node(a)

  let add a p = 
    match p with
      | Empty -> Node(a)
      | _ -> Union(Node(a), p)

  let union p1 p2 = 
    match p1, p2 with
      | Empty, _ -> p2
      | _, Empty -> p1
      | _ -> Union(p1, p2)

  let rec remove a p =
    match p with
      | Empty -> 
	  Empty
      | Node(b) -> 
	  if Term.eq a b then Empty else p
      | Union(p1, p2) ->
	  let p1' = remove a p1 in
	    if p1 == p1' then 
	      let p2' = remove a p2 in
		if p2 == p2' then p else
		  union p1 p2'
	    else  (* successful removal from [p1] *)
	      union p1' p2
	  
  let fold f p e =
    let rec loop acc = function
      | Empty -> acc
      | Node(b) -> f b acc
      | Union(p1, p2) -> loop (loop acc p1) p2
    in
      loop e p

  let elements p = 
    let cons a al = a :: al in
      fold cons p []

  let rec iter f = function
    | Empty -> ()
    | Node(b) -> f b
    | Union(p1, p2) -> iter f p1; iter f p2

  let rec for_all f = function
    | Empty -> true
    | Node(b) -> f b
    | Union(p1, p2) -> for_all f p1 && for_all f p2

  let rec exists f = function
    | Empty -> false
    | Node(b) -> f b
    | Union(p1, p2) -> exists f p1 || exists f p2

end

(** [x |-> (y, rho)] in [post] represents [rho |- x = y].
  In such a case [x] is in the [pre] of [y]. Furthermore,
  this data structure is {i maximally compressed} in that
  whenever [x |-> (y, .)], then there is no [z] such that 
  [y |-> (z, .)]. The set of removable terms is not 
  represented as a list as this makes {!V.restrict} more 
  expensive. *)
type t = {
  post : (Term.t * Jst.t) Term.Var.Map.t; 
  pre : Pre.t Term.Var.Map.t;      
  removable: Term.Var.Set.t
}

let eq s t = s.post == t.post

let empty = {
  post = Term.Var.Map.empty;
  pre = Term.Var.Map.empty;
  removable = Term.Var.Set.empty;
}

let is_empty s = (s.post == Term.Var.Map.empty)

let post s x = Term.Var.Map.find x s.post

let pre s x = 
  let ys = Term.Var.Map.find x s.pre in
    assert(not(Pre.is_empty ys));
    ys

(** Canonical representative of equivalence class for [x]. *)
let find s x = 
  match x with
    | Term.Var _ ->
	(try post s x with Not_found -> Jst.Eqtrans.id x)
    | _ ->
	Jst.Eqtrans.id x

(** Totalized [pre]. *)
let inv s x = 
  assert(Term.is_var x);
  try pre s x with Not_found -> Pre.empty

(** Set of removable variables. *)
let removable s = s.removable

let to_list s =
  Term.Var.Map.fold
    (fun _ (y, _) acc ->
       (y, (Pre.elements (inv s y))) :: acc) 
    s.post []

let to_equalities s =
  Term.Var.Map.fold 
    (fun x (y, rho) acc ->
       Fact.Equal.make (x, y, rho) :: acc)
    s.post []

let pp_as_equalities = ref false
       
let pp fmt s =
  if not(is_empty s) then
    begin
      Pretty.string fmt "\nv:";
      if !pp_as_equalities then
	Pretty.set Fact.Equal.pp fmt (to_equalities s)
      else 
	Pretty.map Term.pp (Pretty.set Term.pp) fmt (to_list s)
    end  

(** A variable [x] is {i canonical} iff it is not in the domain
  of the [post] function of [s]. In this case, {!V.find} is the
  identity. *)
let is_canonical s x =  
  assert(Term.is_var x);
  not(Term.Var.Map.mem x s.post)

(** Merging of two different canonical variables [x] and [y] *)
let rec merge e s =            
  assert (Fact.Equal.is_var e);
  let ((x, y, _) as eql) = Fact.Equal.destruct e in
    if Term.eq x y then s else
      begin 
	Trace.msg "v" "Union" e Fact.Equal.pp;
	Fact.Eqs.push None e;
	union s eql
      end 

(** Merging [x = y] is performed by 
  - adding [x |-> y] and 
  - replacing all links [z |-> x] with [z |-> y]. *)
and union s (x, y, rho) = 
  assert(not(Term.eq x y));            (* [rho |- x = y] *)
  assert(is_canonical s x);
  assert(is_canonical s y);
  let removable' = 
    if !garbage_collection_enabled &&
      Term.Var.is_internal x 
    then 
      Term.Var.Set.add x s.removable
    else 
      s.removable
  in   
  let pre_of_x = inv s x in 
  let post' = 
    Pre.fold
      (fun z ->                         (* [tau |- z = x] *)
	 let (_, tau) = find s z in
	 let sigma = Jst.trans z x y tau rho in
	   Term.Var.Map.add z (y, sigma))
      pre_of_x
      (Term.Var.Map.add x (y, rho) s.post)
  in
  let pre' = 
    try
      let pre_of_y = pre s y in
      let pre_of_y' = Pre.add x (Pre.union pre_of_y pre_of_x) in
	Term.Var.Map.add y pre_of_y'
	  (Term.Var.Map.remove x s.pre)
    with
	Not_found -> 
	  Term.Var.Map.add y (Pre.add x (inv s x))
	  (Term.Var.Map.remove x s.pre)
  in
    {post = post'; 
     pre = pre'; 
     removable = removable'}
      
      
(** Remove a binding [x |-> y] *)
let restrict s x =
  assert(Term.is_var x);
  assert(not(is_canonical s x));
  try
    let (y, rho) = post s x in
      Trace.msg "v" "Remove" x Term.pp; 
      assert(is_canonical s y); 
      assert(not(Pre.is_empty (inv s y)));
      let removable' = 
	Term.Var.Set.remove x s.removable in
      let post' = 
	Term.Var.Map.remove x s.post in
      let pre' =
	let pre_of_y' = Pre.remove x (inv s y) in
	  if Pre.is_empty pre_of_y' then
	    Term.Var.Map.remove y s.pre
	  else 
	    Term.Var.Map.add y pre_of_y' s.pre
      in
	{post = post'; 
	 pre = pre'; 
	 removable = removable'}
  with
      Not_found -> s

(** Variable equality modulo [s] *)
let is_equal s x y = 
  assert(Term.is_var x);
  assert(Term.is_var y);
  let (x', rho) = find s x         (* [rho |- x = x'] *)
  and (y', sigma) = find s y in 
    if Term.eq x' y' then          (* [sigma |- y = y'] *)
      Some(Jst.trans x x' y rho sigma)
    else 
      None

(** Garbage collection *)
let gc f s =
  let gc1 x s =
    assert(Term.is_var x);
    if f x then restrict s x else s
  in
  Term.Var.Set.fold gc1 s.removable s
	  
(** Extension of the equivalence class for [x] contains
  all [y] such that [x] and [y] are equal modulo [s].
  Expensive operation, uses memory linear in the size of the
  [pre] of [y]. *)
let ext s x =
  assert(Term.is_var x);
  let (y, _) = find s x in
    Pre.elements (Pre.add y (inv s y))

(** Starting from the canonical representative [x' = find s x], the
  function [f] is applied to each [y] in [ext s x'] and the results are
  accumulated. *)
let fold s f x e = 
  assert(Term.is_var x);
  let (y, _) = find s x in
    Pre.fold f (inv s y) (f y e)

(** Iteration on extension of equivalence class. *)
let iter s f x =
  assert(Term.is_var x);
  let (y, _) = find s x in
    f y;
    Pre.iter f (inv s y)

let exists s p x = 
  assert(Term.is_var x);
  let (y, _) = find s x in
    p y || Pre.exists p (inv s y) 

let for_all s p x =  
  assert(Term.is_var x);
  let (y, _) = find s x in
    p y && Pre.for_all p (inv s y) 

exception Found

(** Choose an element satisfying some property. *)
let choose s p x = 
  assert(Term.is_var x);
  let (y, _) = find s x in
    match p y with
      | Some(z) -> z
      | None ->
	  let result = ref (Obj.magic 1) in
	    try     
	      Pre.iter
		(fun y ->
		   match p y with
		     | Some(z) -> 
			 result := z;
			 raise Found
		     | None -> ())
		(inv s y);
	      raise Not_found
	    with
		Found -> !result

 
