(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Finite maps and sets of term variables. *)
module Map = Term.Map
module Set = Term.Set

(** Recognizer for variables. *)
let is_var = Term.is_var

(** Variable equality. *)
let eq x y =
  assert(is_var x && is_var y);
  x == y
    
(** Variable ordering using ordering on names. It is always the
  case that an internal variable is larger than an external one.
  Thus, external variables are {i more canonical} than internal variables. *) 
let cmp x y = 
  try
    let n = Term.name_of x and m = Term.name_of y in
      if Term.is_external_varname n then Name.compare n m else 
	if Term.is_external_varname m then  1 else (* internal [n] greater than external [m]*)
	  Name.compare n m
  with
      Not_found -> invalid_arg "V.cmp: not a variable"

module J = Judgement
	
(** {i Justifying union-find structure}.  Representation of a finite conjunction of 
  variable equalities, variable disequalities, and variable constraints.
  - Find structure [x |-> y] is interpreted as equality [x = y].
  [x] is {i canonical} if it does not have such a successor. [x] and [y]
  are said to be equal modulo such a structure [s] iff there are
  paths from [x] and [y] with identical endpoints. 
  - Proof structure [x |-> e] for [x |-> y] in find structure such that [e |- x = y]. 
  - Disequalities [x' |-> (d |- y <> z)] with [x'] canonical and [y] and [x'] congruent modulo [s].
  - Constraints [x' |-> (rho |- y in c)] with [x'] canonical and [x'] congruent [y] modulo [s]. 
  - for canonical [x], [rank x] denotes the length of the potentially longest path 
  from a variable [y] to the root [x] in the find structure. *)
module Config = struct
  
  type t = {
    mutable parent: Term.t Map.t;
    mutable prfs: J.equal Map.t;
    mutable diseqs: J.diseq list Map.t;
    mutable cnstrnts : J.cnstrnt Map.t;
    mutable rank : int Map.t;  
  }
      
  let empty () = {
    parent = Map.empty();
    prfs = Map.empty();
    diseqs = Map.empty();
    cnstrnts = Map.empty();
    rank = Map.empty();
  }
		   
  let is_empty s = 
    Map.is_empty s.parent &&
    Map.is_empty s.cnstrnts &&
    Map.is_empty s.diseqs
      
  (** True if [x] is canonical representative. *)
  let is_canonical s x =
    assert(is_var x);
    not(Map.mem x s.parent)
      
  let parent s x = 
    assert(is_var x);
    try Map.find x s.parent with Not_found -> x

  let prf s x = 
    assert(not(is_canonical s x));
    try Map.find x s.prfs with Not_found -> J.mk_refl x

  (** Canonical representative. Includes path compression. *)
  let find s = 
    let rec fnd x =
      try
	let y = Map.find x s.parent in
	  Map.set x (fnd y) s.parent; 
	  (if not(is_canonical s y) then
	    let e = J.mk_trans (prf s x) (prf s y) in
	      assert(J.validates_equal e x y);
	      Map.set x e s.prfs);
	  y
      with
	  Not_found -> x
    in
      fnd

  let is_compressed s x =
    let y = parent s x in
      eq (parent s y) y
	
  (** [justify s x |- x = find s x]. *)
  let justify s x =
    Format.eprintf "\nJustify <- %s@." (Term.to_string x);
    assert(is_compressed s x);
    let e = try Map.find x s.prfs with Not_found -> J.mk_refl x in
      Format.eprintf "\nJustify ->"; 
      e#pp Format.err_formatter;
      Format.eprintf "@.";
      assert(J.validates_equal e x (parent s x));
      e
	
  let rank s x = 
    try Map.find x s.rank with Not_found -> 0
      
  let diseqs s x =
    assert(is_var x);
    try Map.find (find s x) s.diseqs with Not_found -> []
      
  let is_equal s x y =
    assert(is_var x && is_var y);
    eq (find s x) (find s y)
      
  (** Least common ancestor of [x] and [y] by stepwise 
    descending along find structure. Assumes, [x] and [y] are 
    congruent modulo [s].*)
  let lca s =
    let rec descend x y =
      assert(is_equal s x y);
      let c = Term.compare x y in
	if c = 0 then x else 
	  if c > 0 then descend (parent s x) y else 
	    descend x (parent s y)
    in
      descend

  let cnstrnt_of s x =
    assert(is_var x);
    let c = Map.find (find s x) s.cnstrnts in
      assert(is_equal s x c#arg);
      c

  let cnstrnt s x = 
    let c = cnstrnt_of s x in
      c#cnstrnt
 
  (** Holds iff [x<>y] is valid in [s]. Such valid disequalities are either
   - {i explicit}, that is [x <> y] is explicitly stored, or
   - {i implicit}, that is [x <> y] have incompatible constraints. *)
  let rec is_diseq s x y =
    assert(is_var x && is_var y);
    is_explicit_diseq s x y ||
    is_implicit_diseq s x y
      
  (** [x <> y] is an {i explicit disequality} in [s] if 
    there is a disequality [x0 <> y0] in the disequality index 
    for [x'] such that [eq y' y0]. *)
  and is_explicit_diseq s x y = 
    assert(is_var x && is_var y);
    let is_explicit x y = 
      let dx = diseqs s x in
	dx <> [] &&
	let y' = find s y in
	  List.exists
	    (fun d -> 
	       assert(is_equal s x d#lhs);
	       eq y' (find s d#rhs))
	    dx
    in
      is_explicit x y || is_explicit y x
	  
  (** [x <> y] is an {i implicit disequality} iff
    - [find s x] equals [x0] with constraint [x0 in c], 
    - [find s y] equals [y0] with constraint [y0 in d], and
    - constraints [c] and [d] are disjoint. *)
  and is_implicit_diseq s x y =
    try
      let c = cnstrnt_of s x and d = cnstrnt_of s y in
	Cnstrnt.disjoint c#cnstrnt d#cnstrnt
    with
	Not_found -> false
	
  (** Holds if interpration of [x] is explicitly constrained. *)  
  let is_constrained s x = 
    Map.mem (find s x) s.cnstrnts
      
  (** [x in c] holds iff there is a constraint [y in d] with
    - [x] equals [y] modulo [s] and
    - constraint [d] is stronger than [c]. *)
  let has_cnstrnt s x c =
    assert(is_var x);
    try
      let d = cnstrnt_of s x in
	assert(is_equal s x d#arg);
	Cnstrnt.sub d#cnstrnt c
    with
	Not_found -> false
	  
  (** Compute justifications for implied facts. *)
  module Explain = struct
    
    let equal s x y =
      assert(is_equal s x y);
      assert(is_compressed s x);
      assert(is_compressed s y);
      if eq x y then J.mk_refl x else
	let e1 = justify s x 
	and e2 = justify s y in
	  J.mk_join e1 e2
	      
    exception Found of J.diseq
      
    let explicit_diseq s x y =
      assert(is_explicit_diseq s x y);
      let explicit x y = 
	let dx = diseqs s x in
	  try
	    List.iter
	      (fun d ->                           (* [d |- x1 <> y1]. *)
		 let x1 = d#lhs and y1 = d#rhs in
		   assert(is_equal s x x1);
		   if is_equal s y y1 then
		     let e1 = equal s x1 x and e2 = equal s y1 y in
		       raise(Found(J.mk_replace_in_diseq e1 e2 d)))
	      dx;
	    raise Not_found
	  with
	      Found(rho) -> rho
      in
	try explicit x y with Not_found -> 
	  try explicit y x with Not_found -> 
	    invalid_arg "V.Explicit: unreachable"

    let diseq = explicit_diseq
	       
    let implicit_diseq s x y = 
      assert(is_implicit_diseq s x y);
      let c = cnstrnt_of s x and d = cnstrnt_of s y in
	J.mk_disjoint 
	  (J.mk_replace_in_cnstrnt (equal s c#arg x) c) 
	  (J.mk_replace_in_cnstrnt (equal s d#arg y) d)

    let cnstrnt s x c =
      assert(has_cnstrnt s x c);
      let z_in_d = cnstrnt_of s x in   (* [z_in_d |- z in d] with [d sub c]. *)
      let z = z_in_d#arg in
	if eq x z then z_in_d else
	  J.mk_replace_in_cnstrnt (equal s x z) z_in_d 

  end
  
  let equalities s =
    let es = J.Equals.empty () in
      Map.iter 
	(fun x y -> 
	   let e = Explain.equal s x y in
	     J.Equals.add e es) 
	s.parent;
      es
	
  let disequalities s =
    let ds = J.Diseqs.empty () in
      Map.iter 
	(fun _ dl -> 
	   List.iter (fun d -> J.Diseqs.add d ds) dl)
	s.diseqs;
      ds
	
  let cnstrnts s =
    let cs = J.Cnstrnts.empty () in
      Map.iter (fun _ c -> J.Cnstrnts.add c cs) s.cnstrnts;
      cs
  
  (** Pretty-printing. *)
  let rec pp fmt s = 
    let es = equalities s and ds = disequalities s and cs = cnstrnts s in
      Format.fprintf fmt "@[v:{"; J.Equals.pp fmt es;
      if not(J.Diseqs.is_empty ds) then 
	(Format.fprintf fmt "@[,@;<1 3>  "; 
	 J.Diseqs.pp fmt ds;
	 Format.fprintf fmt "@]");
      if not(J.Cnstrnts.is_empty cs) then
	(Format.fprintf fmt "@[,@;<1 3>  "; 
	 J.Cnstrnts.pp fmt cs;
	 Format.fprintf fmt "@]");
      Format.fprintf fmt "}@]@;"

end


module Infsys = struct

  (** Global state. *)
  let config = ref (Config.empty())
  let unchanged = ref true

  let initialize t =
    unchanged := true;
    config := t
      
  let reset () = 
    config := Config.empty();
    unchanged := true

  let current () = !config
		     
  let is_unchanged () = !unchanged

  let finalize () =
    if !unchanged then !config else {
      Config.parent = Map.copy !config.Config.parent;
      Config.prfs = Map.copy !config.Config.prfs;
      Config.cnstrnts = Map.copy !config.Config.cnstrnts;
      Config.diseqs = Map.copy !config.Config.diseqs;
      Config.rank = Map.copy !config.Config.rank
    }

  (** Operations on global configuration. *)
  let find x = Config.find !config x
  let is_canonical x = Config.is_canonical !config x
  let rank x' = Config.rank !config x'
  let justify x = Config.justify !config x
  let cnstrnt_of x = Config.cnstrnt_of !config x
  let diseqs x = Config.diseqs !config x
  let is_equal x y = Config.is_equal !config x y
  let is_explicit_diseq x y = Config.is_explicit_diseq !config x y
  let is_implicit_diseq x y = Config.is_implicit_diseq !config x y
  let has_cnstrnt x c = Config.has_cnstrnt !config x c
  let explain x y = Config.Explain.equal !config x y

  (** Disequalities from external configurations. *)
  let external_diseqs = ref []
			  
  let register_external_diseq deq =
    external_diseqs := deq :: !external_diseqs

  let is_external_diseq x y =
    assert(is_var x && is_var y);
    let x' = find x and y' = find y in
    let rec orelse = function
      | [] -> None
      | is_diseq :: rest -> 
	  try 
	    let d = is_diseq x' y' in            (* [d |- x' <> y']. *)
	      Some(J.mk_replace_in_diseq (explain x' x) (explain y' y) d)
	  with _ -> 
	    orelse rest 
    in
      orelse !external_diseqs
	
  let is_diseq x y =
    assert(is_var x && is_var y);
    if is_explicit_diseq x y then
      Some(Config.Explain.explicit_diseq !config x y)
    else if is_implicit_diseq x y then
      Some(Config.Explain.implicit_diseq !config x y)
    else 
      is_external_diseq x y
	
  (** Merge two equivalence classes. *)
  let rec process_equal e =                       (* [e |- x = y]. *)
    let x = e#lhs and y = e#rhs in
    let x' = find x and y' = find y in
      if not(eq x' y') then 
	match is_diseq x y with                   (* [d |- x <> y]. *)
	  | Some(d) -> raise(J.Unsat(J.mk_contra e d))
	  | None -> merge e x' y'

  and merge e x' y' =
    assert(is_canonical x' && is_canonical y' && not(eq x' y'));
    let cmp = Pervasives.compare (rank x') (rank y') in
      if cmp > 0 then merge1 e x' y' else
	if cmp < 0 then merge1 (J.mk_sym e) y' x' else 
	  if cmp > 0 then
	    (merge1 e x' y'; 
	     Map.set y' (rank y') !config.Config.rank)
	  else
	    (merge1 (J.mk_sym e) y' x'; 
	     Map.set x' (rank x') !config.Config.rank)

  and merge1 e x' y' =
    unchanged := false;
    let e1 = justify e#lhs and e2 = justify e#rhs in
    assert(J.validates_equal e1 e#lhs x');
    assert(J.validates_equal e2 e#rhs y');
    let e' = J.mk_trans3 (J.mk_sym e1) e e2 in
      assert(J.validates_equal e' x' y');
      Map.set x' e' !config.Config.prfs;
      Map.set x' y' !config.Config.parent;
      merge_cnstrnts x' y';
      merge_diseqs x' y';
      Propagate.Equal.put x' 
	
  and merge_cnstrnts x' y' =      
    assert(is_equal x' y');
    try
      let c1 = cnstrnt_of x' in               (* [c1 |- x1 in c]. *)
      let x1 = c1#arg and c = c1#cnstrnt in
	assert(is_equal x' x1);  
	Map.remove x' !config.Config.cnstrnts;
	(try                                      
	   let c2 = cnstrnt_of y' in          (* [c2 |- y1 in d]. *)
	   let y1 = c2#arg and d = c2#cnstrnt in
	     assert(is_equal y1 y'); 
	     if Cnstrnt.sub c d then
	       ()
	     else 
	       let e = explain x' y' in       (* [e |- x' = y']. *)
		 if Cnstrnt.disjoint c d then   
		   let d  =                   (* [d |- x' <> y']. *)
		     J.mk_disjoint 
		       (J.mk_replace_in_cnstrnt (explain x1 y') c1) 
                       (J.mk_replace_in_cnstrnt (explain y1 y') c2)
		   in
		     raise(J.Unsat(J.mk_contra e d))
	     else 
	       let c = J.mk_inter (J.mk_replace_in_cnstrnt e c1) c2 in
		 Map.set y' c !config.Config.cnstrnts
	 with
	     Not_found ->
	       Map.set y' c1 !config.Config.cnstrnts)
    with
	Not_found -> ()

  and merge_diseqs x y =
    assert(is_equal x y);
    try
      let dx = Map.find x !config.Config.diseqs in
	assert(dx <> []);
	Map.remove x !config.Config.diseqs;
	let dy' = 
	  try
	    let dy = Map.find y !config.Config.diseqs in
	      List.fold_right 
		(fun d acc -> 
		   if is_redundant d dy then acc else d :: acc)
		dx dy
	  with
	      Not_found -> dx
	in
	  Map.set y dy' !config.Config.diseqs
    with
	Not_found -> ()

  and is_redundant d1 =
    List.exists 
      (fun d2 -> 
	 assert(is_equal d1#rhs d2#rhs);
	 is_equal d1#lhs d2#lhs)

  let process_cnstrnt c1 =
    let x = c1#arg and c = c1#cnstrnt in     (* [c1 |- x in c]. *)
    let x' = find x in
      try
	let c2 = cnstrnt_of x in            
	let y = c2#arg and d = c2#cnstrnt in (* [c2 |- y in d]. *)
	  assert(is_equal x y);
	  if Cnstrnt.sub d c then () else
	      if Cnstrnt.disjoint d c then
		raise(J.Unsat(J.mk_contra (explain x y) (J.mk_disjoint c1 c2)))
	      else 
		let c3 = J.mk_inter c1 (J.mk_replace_in_cnstrnt (explain x y) c2) in
		  unchanged := false;        (* [c3 |- y in (c inter d)]. *)
		  Map.set x' c3 !config.Config.cnstrnts;
		  Propagate.Cnstrnt.put x'
      with
	  Not_found -> 
	    unchanged := false;
	    Map.set x' c1 !config.Config.cnstrnts;
	    Propagate.Cnstrnt.put x'
	      
  (** Processing a disequality. *)
  let rec process_diseq d =                 (* [d |- x <> y]. *)              
    let x = d#lhs and y = d#rhs in
    let x' = find x and y' = find y in
      if eq x' y' then 
	raise(J.Unsat(J.mk_contra (explain x y) d))
      else if is_explicit_diseq y' x' then () else   
        begin
	  unchanged := false;
	  Map.set x' (d :: diseqs x') !config.Config.diseqs;
	  Propagate.Diseq.put (x', y')
	end    

   (** Normalizing the representation of a variable partitioning includes:
     - garbage collection of noncanonical variables (currently disabled) *)
   let normalize () =
     ()

   let can x = 
     assert(Config.is_compressed (current()) x);
     let e = Config.justify !config x in
       assert(eq x e#lhs);
       e#rhs, e
     
   (** Equality test. *)
   let is_equal x y = 
     if is_equal x y then
       Some(explain x y)
     else
       None
	 
   let has_cnstrnt x c =
     if has_cnstrnt x c then
       Some(Config.Explain.cnstrnt !config x c)
     else 
       None
      
end
  
