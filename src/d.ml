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
 
       

module Set = Set.Make(
  struct
    type t = Term.t * Jst.t
    let compare (x, _) (y, _) = Term.Var.compare x y
  end)

let mem x = Set.exists (fun (y, _) -> Term.eq x y)

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

let fold = Term.Map.fold
let iter = Term.Map.iter


(** All terms known to be disequal to [a]. *)
let diseqs s x =
  try Term.Map.find x s with Not_found -> Set.empty


(** Known disequalities; [x |-> {y, z}] is interpreted as [x <> y & x <> z].
  The function is closed in that forall [x], [y] such that [x |-> {..., y,...}]
  then also [y |-> {...., x,....}] *)
let closed s =
  Term.Map.fold
    (fun x ys acc -> acc && 
       (Set.for_all (fun (y, _) -> mem x (diseqs s y)) ys))
    s true

let occurs s x = Term.Map.mem x s


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
      Pretty.string fmt "\nd:";
      Pretty.list Fact.Diseq.pp fmt (Fact.Diseq.Set.elements ds)
    end
    


let map_diseqs s f a =
  let (x, rho) = f a in
  let ds = diseqs s x in
    if Term.eq a x then ds else
      Set.fold 
	(fun (z, tau) ->                       (* [tau |- y <> z] *)
	   let sigma = Jst.dep2 tau rho in
	     Set.add (z, sigma))
	ds Set.empty

exception Found of Jst.t

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
  assert(Fact.Diseq.is_var d);
  assert(closed s);
  let (x, y, rho) = Fact.Diseq.destruct d in
    match is_diseq s x y with
      | Some _ -> s
      | None ->
	  Trace.msg "d" "Add(d)" d Fact.Diseq.pp;
	  Fact.Diseqs.push None d;      (* new disequality *)
	  let dx' = Set.add (y, rho) (diseqs s x)
	  and dy' = Set.add (x, rho) (diseqs s y) in
	  let s' = 
	    Term.Map.add x dx'
	      (Term.Map.add y dy' s)
	  in
	    assert(closed s');
	    s'
	    
	
(** Propagating an equality between variables. *)
let merge e s =
  Trace.msg "d" "Merge(d)" e Fact.Equal.pp;
  assert(Fact.Equal.is_var e);
  assert(closed s);
  let (x, y, rho) = Fact.Equal.destruct e in         (* [rho |- x = y] *)
    if Term.eq x y then s else
      match is_diseq s x y with
	| Some(tau) ->                                 (* [tau |- x <> y] *)
	    raise(Jst.Inconsistent(Jst.dep2 rho tau))
	| None -> 
	    (try
	       let dx = Term.Map.find x s
	       and dy = diseqs s y in
		 assert(not(mem x dy));
		 assert(not(mem y dx));
		 let (s', dy') = 
		   (Term.Map.remove x s, Set.union dx dy) 
		 in
		 let (s'', dy'') = 
		   Set.fold
		     (fun (z, tau) (s, dy) ->                  (* [tau |- z <> x] *)
			let dz = diseqs s z in
			  try
			    let dz' = Set.remove (assoc x dz) dz in
			      assert(not(mem x dz'));
			      let sigma = Jst.dep2 tau rho in  (* [sigma|- z <> y] *)
			      let dz'' = Set.add (y, sigma) dz' in 
			      let dy' = Set.add (z, sigma) dy in
				(Term.Map.add z dz'' s, dy')
			  with
			      Not_found -> (s, dy))
		     dx
		     (s', dy')
		 in
		 let s''' = 
		   if dy'' == dy then s'' else
		     Term.Map.add y dy'' s''
		 in
		   assert(closed s''');
		   assert(not(occurs s''' x));
		   s'''
	     with      
		 Not_found -> 
		   assert(not(occurs s x));
		   s)
 






