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



open Sym
open Mpa

(** Known disequalities; [x |-> {y, z}] is interpreted as [x <> y & x <> z].
  The function is closed in that forall [x], [y] such that [x |-> {..., y,...}]
  then also [y |-> {...., x,....}] *)


module Set = Set.Make(
  struct
    type t = Term.t * Justification.t
    let compare (x, _) (y, _) = Term.cmp x y
  end)

exception Found of Set.elt

(** Return [(x, rho)] if there is such an entry in [ds];
  otherwise raise [Not_found]. *)
let assoc x ds =
  try
    Set.iter
      (fun ((y, _) as d) ->
	 if Term.eq x y then 
	   raise(Found(d)))
      ds;
    raise Not_found
  with
      Found(d) -> d
	    

type t =  Set.t Term.Map.t

let empty = Term.Map.empty

let eq s t = (s == t)


(** List all disequalities. *)
let to_set s =
  Term.Map.fold
    (fun x ys ->
       Set.fold 
         (fun (y, rho) -> 
	    let d = Fact.Diseq.make (x, y, rho) in
	      Fact.Diseq.Set.add d)
         ys)
    s 
    Fact.Diseq.Set.empty

let to_list s =
  Fact.Diseq.Set.fold (fun d acc -> d :: acc) (to_set s) []
	   


(** Pretty-printing. *)
let rec pp fmt s = 
  let ds = to_set s in
  if not(Fact.Diseq.Set.is_empty ds) then
    begin
      Format.fprintf fmt "\nd:";
      Pretty.list Fact.Diseq.pp fmt (Fact.Diseq.Set.elements ds)
    end
    

(** All terms known to be disequal to [a]. *)
let diseqs s x =
  try Term.Map.find x s with Not_found -> Set.empty

let map_diseqs s f a =
  let (x, rho) = f a in
  let ds = diseqs s x in
    if Term.eq a x then ds else
      Set.fold 
	(fun (z, tau) ->                       (* [tau |- y <> z] *)
	   let sigma = Justification.subst_diseq (x, z) tau [rho] in
	     Set.add (z, sigma))
	ds Set.empty

exception Found of Justification.t

(** Check if two terms are known to be disequal. *)
let is_diseq s x y =
  try
    Set.iter
      (fun (z, rho) ->
	 if Term.eq y z then
	   raise(Found(rho)))
      (diseqs s x);
    None
  with
      Found(rho) -> Some(rho)


(** Adding a disequality over variables *)
let add d s =
  let (x, y, rho) = Fact.Diseq.destruct d in
    match is_diseq s x y with
      | Some _ -> s
      | None ->
	  Trace.msg "d" "Add(d)" d Fact.Diseq.pp;
	  Fact.Diseqs.push None d;      (* new disequality *)
	  let dx' = Set.add (y, rho) (diseqs s x)
	  and dy' = Set.add (x, rho) (diseqs s y) in
	    Term.Map.add x dx'
	      (Term.Map.add y dy' s)
	    
	
(** Propagating an equality between variables. *)
let rec merge e s =                 
  Trace.msg "d" "Merge(d)" e Fact.Equal.pp;
  let (x, y, rho) = Fact.Equal.destruct e in         (* [rho |- x = y] *)
    match is_diseq s x y with
      | Some(tau) ->                                 (* [tau |- x <> y] *)
	  let sigma = Justification.contradiction rho tau in
	    raise(Justification.Inconsistent(sigma))
      | None -> 
	  let dx = diseqs s x 
	  and dy = diseqs s y in
	  let s' = 
	    let dxy = Set.union dx dy in
	      if dy == dxy then
		Term.Map.remove x s
	      else 
		Term.Map.add y dxy (Term.Map.remove x s)
	  in
	    Set.fold
	      (fun (z, tau) s ->                      (* [tau |- z <> x] *)
		 let dz = diseqs s z in
		   try
		     let dz' = Set.remove (assoc x dz) dz in
		     let sigma = Justification.subst_diseq (z, y) tau [rho] in
		     let dz'' = Set.add (y, sigma) dz' in  (* [sigma|-z<>y] *)
		       Term.Map.add z dz'' s
		   with
		       Not_found -> s)
	      dx
	      s'
  
