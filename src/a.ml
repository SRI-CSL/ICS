
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
 * Author: Harald Ruess
 i*)

(*i*)
open Term
(*i*)

(*s An arithmetic context consists of a a representation of a
 solution set [e] of equations of the form [x = a], where [x] is a 
 variable and [a] is an arithmetic term not containing any of the rhs,
 and a set of constraints [x in i],  for [i] an interval 
 constraints, in the constraint context [cnstrnt].  Inequality
 reasoning introduces so-called slack variables of name [k!i], where
 [i] is some integer. *)


type t = {
  a : Solution.t;
  c : C.t
}

let solutions s = s.a
let cnstrnts s = s.c

let apply s = Solution.apply s.a
let find s = Solution.find s.a
let inv s = Solution.inv s.a
let use s = Solution.use s.a
let mem s = Solution.mem s.a


(* Constraint of a term is obtained from the constraint of 
 an equivalent variable or by abstract interpretation of
 the arithmetic operators in the domain of constraints. *)

let lookup (v, s) x = 
  Map.find (V.find v x) s.c

let cnstrnt (v, s) = Arith.cnstrnt (lookup (v, s))


(*s Test if a variable [x] has a constraint associated with it
 by looking up the canonical representative of [x] w.r.t. to [v]. *)

let is_unconstrained (v, s) x =
  is_var x && not(Map.mem (V.find v x) s.c)


(*s Split a term into the part with constraints and the unconstraint part.
 Also, return the constraint for the term with a constraint. *)

let split (v, s) = Arith.split (lookup (v, s)) 

(*s Empty context. *)

let empty = {
  a = Solution.empty;
  c = Map.empty
}

let is_empty s =
  Solution.is_empty s.a &&
  (s.c == Map.empty)


(*s Extend. Rename [b] with a fresh variable [x]. [b] is assumed
 to be canonical. Also make sure that  [x] has a constraint associated 
 if [b] has one. *)

let extend b s =
  let (x', a') = Solution.extend b s.a in
  Trace.msg "a" "Extend" (x', b) Term.pp_equal;
  let c' = 
    try
      let i' = cnstrnt (V.empty, s) b in  (* variable are assumed to be canonical. *)
      Term.Map.add x' i' s.c              (* thus it suffices to use [V.empty]. *)
    with
	Not_found -> s.c
  in
  let s' = {s with a = a'; c = c'} in
  (x', s')

(*s Solver. First try to solve for variables [x] which occur at
 least twice on the rhs of terms in the solution set in order to
 propagate constraints. At the next stage try to solve for variables
 which occur in the solution set. If there are no variables in [a = b],
 that is, if every noninterpreted subterm is a nonlinear term, then
 [Exc.Unsolved] is thrown. *)

let solve (v, s) (a, b) = 
  let occurs_at_least_twice_on_rhs (v, s) x = 
    is_var x && (Set.cardinal (use s x) > 1)
  in
  let occurs_on_rhs (v, s) x = 
    is_var x && (not(Set.is_empty (use s x)))
  in
  let dom s x = is_var x && mem s x in
  try
    match Arith.solve_for (occurs_at_least_twice_on_rhs (v,s)) (a, b) with
      | None -> []
      | Some(x', b') -> [(x', b')]
  with
      Exc.Unsolved ->
	try
	  match Arith.solve_for (occurs_on_rhs (v,s)) (a, b) with
	    | None -> []
	    | Some(x', b') -> [(x', b')]
	with
	    Exc.Unsolved ->
	      try
		match Arith.solve_for (dom s) (a, b) with
		  | None -> []
		  | Some(x', b') -> [(x', b')]
	      with
		 Exc.Unsolved -> 
		   match Arith.solve_for is_var (a, b) with
		     | None -> []
		     | Some(x', b') -> [(x', b')]


(*s The following functions are state transformers for 
 the state/configuration [(v, d, s, focus)], where [v]
 contains the currently known variable equalities, [d] is
 the set of disequalities, [s] consists of the arithmetic
 solution set [s.a] and the constraint map [s.c], and [focus]
 contains the set of variables with an updated canonical
 representative in [v] and the newly induced disequalities. *)


(*s Merging a variable equality [x = y] by composing the
 current solution set with the solved form of [a = b] where
 [a] is the find of [x] in [s] and [b] is the find of [y]. *)

let rec merge_a (x, y) ((v, _, s, _) as acc) =
  Trace.msg "a" "Merge" (x, y) Term.pp_equal;
  let a = find s x in
  let b = find s y in
  equality (a, b) acc

and equality (a, b) ((v, d, s, focus) as acc) =
  Trace.msg "a" "Equality" (a, b) Term.pp_equal;
  try
    match solve (v, s) (a, b) with
      | [] -> acc
      | sl -> compose sl acc
  with
      Exc.Unsolved -> acc

and compose sl (v, d, s, focus) = 
  Trace.msg "a" "Compose" sl (Pretty.list Term.pp_equal);
  let vfocus = Focus.v focus in
  let (v', a', vfocus', ch') = Solution.compose Arith.map (v, s.a, sl, vfocus) in 
  let s' = {s with a = a'} in
  let focus' = Focus.make vfocus' (Focus.d focus) in
  deduce ch' (update vfocus' (v', d, s', focus'))  
          (* update should actually be called only on the new equalities in v. *)
          (* thus there is way too much work done here!. *)


(*s Update the constraint part with the new variable equalities.
  [x] has a new canonical representative in [v] and this information
  needs to be propagated in order to keep the invariant that the
  constraint map has only canonical variables in the domain. *)

and update vfocus = V.Focus.fold update1 vfocus

and update1 x ((v, d, s, focus) as acc) =
  Trace.msg "a" "Update" x Term.pp;
  try
    let y = V.find v x in
    if Term.eq x y then acc else merge_c (x, y) acc
  with
      Not_found -> acc


(*s Deduce possible new constraints from equalities [x = b],
 where [b] has changed recently. Decompose [b] into two parts
 [b'] and [b''] such that [b = b' + b''] and [b'] has a constraint
 [j'] associated with it, while none of the monomials in [b''] 
 have constraints. Now, if [x] has constraint [i], then we
 obtain the new constraint [b'' in i -- j'], which is propagated
 using the state transformer [termcnstrnt]. *)


and deduce ch = Set.fold deduce1 ch

and deduce1 x ((v, d, s, focus) as acc) =
  Trace.msg "a" "Deduce" x Term.pp;
  try
    let b = apply s x in
    match split (v, s) b with
      | None, None -> acc          (* Case: unreachable. *)
      | None, Some _ ->            (* Case: [b] is unconstrained. *)
	  (try 
	     let i = lookup (v, s) x in
	     termcnstrnt (b, i) acc
	   with
	       Not_found -> acc)
      | Some(_, j), None ->       (* Case: [b] is constrained. *)
	  (try 
	     let i = lookup (v, s) x in
	     match Cnstrnt.cmp i j with
	       | Binrel.Disjoint -> raise Exc.Inconsistent
	       | Binrel.Same -> acc
	       | Binrel.Super -> varcnstrnt (x, j) acc
	       | Binrel.Sub -> termcnstrnt (b, i) acc
	       | Binrel.Overlap(ij) -> termcnstrnt (b, ij) (varcnstrnt (x, ij) acc)
	       | Binrel.Singleton(q) -> 
		   let n = Arith.mk_num q in
		   compose [(x, n)] (equality (b, n) acc)
	  with
	      Not_found -> varcnstrnt (x, j) acc)
      | Some(b', j'), Some(b'') ->  (* [b = b' + b''], [b' in j'] *)
	  (try                      (* and [b''] is unconstrained *) 
	     let i = lookup (v, s) x in
	     termcnstrnt (b'', Cnstrnt.subtract i j') acc
	   with
	       Not_found -> 
		 let a' = find s x in  (* make sure that all variables in [s] are prop. *)
		 termcnstrnt (Arith.mk_sub a' b'', j') acc) (* important for termination*)
  with
      Not_found -> acc

(*s Add a new constraint [x' in i], where [x] is a variable,
 and [x'] is the canonical representative of [x]. If [i] is
 a singleton constraint with element [q], then a new variable
 [b = q], where [x = b] in [s.a], is propagated. *)

and varcnstrnt (x, i) ((v, d, s, focus) as acc) = 
  Trace.msg "a" "Var cnstrnt" (x, i) Term.pp_in;
  match Cnstrnt.status i with
    | Status.Empty ->
	raise Exc.Inconsistent
    | Status.Singleton(q) ->
	let n = Arith.mk_num q in
	let acc' = compose [(x, n)] acc in
	(try
	   let b = apply s x in
	   equality (b, n) acc'
	 with
	     Not_found -> acc')
    | _ ->
	let c' = Map.add (V.find v x) i s.c in
	let s' = {s with c = c'} in 
	let acc' = (v, d, s', focus) in
	acc'

(*s Add a constraint [a in i], where [a] can now be an arbitrary
 arithmetic term. If [a] is a variable, then this function
 reduces to [varcnstrnt] above, and if there is a [x = a] in
 the solution set, then the constraint [x in i] is again propagated
 using [varcnstrnt]. Otherwise, a fresh name [x'] is created for 
 the term [a], and the equality [x' = a] is propagated using the
 [equality] transformer after extending the constraint part with [x' in i]. *)

and termcnstrnt (a, i) ((v, d, s, focus) as acc) = 
  Trace.msg "a" "Term cnstrnt" (a, i) Term.pp_in;
  let (a, i) = Arith.normalize (a, i) in
  match Cnstrnt.status i with
    | Status.Empty ->
	raise Exc.Inconsistent
    | Status.Singleton(q) ->
	equality (a, Arith.mk_num q) acc
    | _ ->
	if is_var a then
	  varcnstrnt (a, i) acc
	else 
	  try
	    let x = V.find v (inv s a) in
	    varcnstrnt (x, i) acc
	  with                         (* generate new variable. *)
	      Not_found -> 
		let x' = Term.mk_fresh_var (Name.of_string "v") None in
		equality (x', a)
		  (varcnstrnt (x', i) acc)
		

(*s Merge a variable equality [x = y] in the constraint map by
 adding [x in ij] for the canonical variable [x], where [x in i],
 [y in j] are in the constraint map and [ij] is the intersection of
 [i] and [j], and by removing the constraint for [y]. In case, [ij]
 is a singleton constraint with element [q], an equality [x = q] is
 generated. Singleton constraints are always retained in the constraint
 map in order to keep the invariant that the best available constraint
 are always associated with canonical variables. *)

and merge_c (x, y) (v, d, s, focus) = 
  Trace.msg "a" "Cnstrnt merge" (x, y) Term.pp_equal;
  try
    let i = Map.find x s.c in
    try
      let j = Map.find y s.c in
      match Cnstrnt.cmp i j with
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| (Binrel.Same | Binrel.Super) ->
	    let c' = Map.remove x s.c in
	    let s' = {s with c = c'} in
	    (v, d, s', focus)
	| Binrel.Sub ->
	    let c' = Map.add y i (Map.remove x s.c) in
	    let s' = {s with c = c'} in
	    (v, d, s', focus)
	| Binrel.Singleton(q) ->
	    let i' = Cnstrnt.mk_singleton q in
	    let c' = Map.add y i' (Map.remove x s.c) in
	    let s' = {s with c = c'} in
	    compose [(x, Arith.mk_num q)] (v, d, s', focus)
	| Binrel.Overlap(ij) ->
	    let c' = Map.add y ij (Map.remove x s.c) in
	    let s' = {s with c = c'} in
	    (v, d, s', focus)
    with
	Not_found ->         
	  let c' = Map.add y i (Map.remove x s.c) in
	  let s' = {s with c = c'} in
	  (v, d, s', focus)
  with
      Not_found ->
	(v, d, s, focus)


(*s Adding a new constraint [x in i] by first adding [k in i]
 for a fresh variable followed by propagating [x = k]. In effect,
 the introduction of the fresh [k] triggers the constraint propagation
 through equality reasoning, since [a = k] will usually be solved for
 a variable occuring in the solution set.  See also the definition 
 of [solve]. *)

let add c (v, d, s) =
  let (x, i,_) = Fact.d_cnstrnt c in
  if Cnstrnt.is_empty i then
    raise Exc.Inconsistent
  else 
    let k' = Term.mk_fresh_var (Name.of_string "k") None in
    let c' = Map.add k' i s.c in
    let s' = {s with c = c'} in
    merge_a (x, k') (v, d, s', Focus.empty)



(*s Merge first in the solution then in the constraint part. *)

let merge (x, y) acc =
  merge_c (x, y) (merge_a (x, y) acc)


(*s Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let diseq (x, y) ((v, d, s, focus) as acc) =
  Trace.msg "a" "Diseq" (x, y) Term.pp_diseq;
  try
    let i = lookup (v, s) x in
    let j = lookup (v, s) y in
    match Cnstrnt.d_singleton i, Cnstrnt.d_singleton j with
      | Some(q), Some(p) ->
	  if Mpa.Q.equal q p then
	    raise Exc.Inconsistent
	  else 
	    acc
      | Some(q), None ->
	  let j' = Cnstrnt.inter j (Cnstrnt.mk_diseq q) in
	  varcnstrnt (y, j') acc
      | None, Some(q) -> 
	  let i' = Cnstrnt.inter i (Cnstrnt.mk_diseq q) in
	  varcnstrnt (x, i') acc
      | None, None ->
	  acc
  with
      Not_found -> acc

(*s Merging in new equalities and disequalities in the [focus] set. *)

let rec close acc =
  close_v (close_d acc)

and close_v (v, d, s, focus) = 
  Focus.fold_v 
    (fun x (v, d, s, focus) ->
       merge (x, V.find v x) (v, d, s, focus))
    focus 
    (v, d, s, Focus.empty)

and close_d ((_, _, _, focus) as acc) =
  Focus.fold_d diseq focus acc
   

(*s Instantiate the lhs variables in the solution set with
 their canonical representative. *)

let inst v s =
  let a' = Solution.inst v s.a in
  {s with a = a'}

(*s Split. *)

let split s =
  Term.Map.fold
    (fun x i acc ->
       if Cnstrnt.is_finite i then
	 (x, i) :: acc
       else 
	 acc)
    s.c []
