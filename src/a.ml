
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
 variable and [a] is an arithmetic term not containing any of the rhs. *)


module A = Subst.Make(
  struct
    let name = "a"
    let fold = Arith.fold
    let map = Arith.map
  end)

(*s The pair [(e, c)] represents an arithmetic context consisting
 of equalities [x = a], for [a] purely interpreted in arithmetic,
 in the [e] part, and constraints [x in i],  for [i] an interval 
 constraints, in the constraint context [c]. *)

type t = {
  solution : A.t;
  cnstrnt : C.t;
  slacks : Set.t
}


(*s [solution (e,_)] and [domain (_,c)] conjoined form
 the arithmetic context represented by [(e,_)]. *)

let solutions s = A.solution s.solution

let domains s =  C.domains s.cnstrnt

(*s List of split predicates. *)

let split s = C.split s.cnstrnt

(*s Pretty-printing. *)

let pp fmt s =
  if not(A.is_empty s.solution) then
    (Format.fprintf fmt "\na:"; A.pp fmt s.solution);
  if not(C.is_empty s.cnstrnt) then
    (Format.fprintf fmt "\nc:"; C.pp fmt s.cnstrnt)


(*s Accessors. *)

let apply s = A.apply s.solution
let find s = A.find s.solution
let inv s = A.inv s.solution
let mem s = A.mem s.solution

let use s a = 
  if is_var a then A.use s.solution a else Set.empty

(*s Canonical form modulo the solution set in [e]. 
 Returns a variable whenever possible. *)

let can e =
  let rec canvar a = 
    a 
  and canterm a =
    if is_var a then
      canvar a
    else 
      let f, l = destruct a in
      match Sym.d_arith f with
	| None -> a
	| Some(op) ->
	    let l' = Term.mapl (fun b -> A.find e (canterm b)) l in
	    let a' = Arith.sigma op l' in
	    try A.inv e a' with Not_found -> a'
  in
  canterm


(* Constraint of a term is obtained from the constraint of 
 an equivalent variable or by abstract interpretation of
 the arithmetic operators in the domain of constraints. *)

let cnstrnt s a =
  let a' = can s.solution a in
  Arith.cnstrnt (C.apply s.cnstrnt) a'

(*s [occurs (e, c) x] holds if [x] occurs in a solution in [e]
 or if it is a member of the domain of [c]. *)

let occurs (e, c) x =
  A.occurs e x || C.mem x c

(*s Empty context. *)

let empty = {
  solution = A.empty;
  cnstrnt =  C.empty;
  slacks = Set.empty
}

(*s [name e a] returns a variable [x] if there is
 a solution [x = a]. Otherwise, it creates a new name
 [x'] and installs a solution [x' = a] in [e]. *)

type status =
    | Name of Term.t
    | Fresh of Term.t * A.t

let name b e =
  try
    Name(A.inv e b)
  with
      Not_found ->
	let (x', e') = A.extend b e in 
	Fresh(x', e')


(*s Slack variables *)

let mk_slack =
  let name = Name.of_string "k" in
  fun () -> Term.mk_fresh_var name None

let is_slack slacks x =
  Set.mem x slacks

let is_var_and_not_slack slacks x = 
  is_var x && not (is_slack slacks x)


(*s Normalize constraint such as ['q + p * x' in 'c'] 
 to ['x' in '1/p ** (c -- q)'], where ['**'], ['--'] are abstract interval 
 operations for linear multiplication and subtraction. *)

let normalize q p c =           
  if Mpa.Q.is_zero q && Mpa.Q.is_one p then
    c
  else if Mpa.Q.is_zero q then
    Cnstrnt.multq (Mpa.Q.inv p) c
  else 
    Cnstrnt.multq (Mpa.Q.inv p) (Cnstrnt.addq (Mpa.Q.minus q) c)

(*s Propagation of equalities and constraints works by
 transforming states of the form [(es, cs, e, c, veqs)] until
 saturation. Here, [es] is a list of equalities and [cs] is a 
 list of constraints [a in i]. [es] and [cs] include facts
 which still have to be propagated. Thus, states is saturated
 only if [es] and [cs] are both empty. Together, [e] and [c] form 
 the current arithmetic context [(e, c)] of type [t] above,
 and [veqs] is the list of newly generated equalities. *)

type state = {
  es : (Term.t * Term.t) list;
  cs : (Term.t * Cnstrnt.t) list;
  e : A.t;
  c : C.t;
  slcks : Set.t;
  v : V.t;
  veqs : Veqs.t
}

let pp_state fmt s =
  let sep () = Format.fprintf fmt ", @;" in
  Format.fprintf fmt "@[(";
  Pretty.list pp_equal fmt s.es; sep();
  Pretty.list pp_in fmt s.cs; sep();
  A.pp fmt s.e; sep();
  C.pp fmt s.c; sep();
  Veqs.pp fmt s.veqs;
  Format.fprintf fmt ")@]"

(*s Generating new equalities. *)

let add_generated_eq s a b es = 
  let a' = can s.e a and b' = can s.e b in
  if eq a' b' then es
  else if List.exists (fun (x, y) -> (eq a' x) && (eq b' y)) es then
    es
  else 
    (a', b') :: es

let rec union_generated_eq s es1 es2 =
  match es1 with
    | [] -> es2
    | (x1, y1) :: es1 -> add_generated_eq s x1 y1 (union_generated_eq s es1 es2)

let add_generated_cnstrnt s a i cs =
  let a' = can s.e a in
  if List.exists (fun (b,j) -> eq a' b && Cnstrnt.sub j i) cs then
    cs
  else
    (a', i) :: cs


(*s Toplevel processing function. *)  

let rec process s =
  Trace.msg "a'" "Process" s pp_state;
  match s.es, s.cs with
    | [], [] -> 
	({solution = s.e; cnstrnt = s.c; slacks = s.slcks}, 
	 s.veqs)
    | _, (a, i) :: cs' ->
	process (add a i {s with cs = cs'})
    | (a, b) :: es', _ ->
	process (equality a b {s with es = es'})

and add a i s =
  match Cnstrnt.status i with
    | Status.Empty ->
	raise Exc.Inconsistent
    | Status.Singleton(q) ->
	let num = Arith.mk_num q in
	{s with es = add_generated_eq s a num s.es}
    | _ ->
	let a' = can s.e a in    (* returns a variable whenever possible. *)
	if is_var a' then
	  addcnstrnt a' i s
	else                
	  match Arith.decompose a' with     
	    | Arith.Const(q) ->                 (* Case: [q in i] *)
		if Cnstrnt.mem q i then s else raise Exc.Inconsistent
	    | Arith.One(q, p, x) ->              (* Case: [q + p * x in i] *)
		addcnstrnt x (normalize q p i) s (* iff [x in 1/p * (i - q)] *)  
	    | Arith.Many _ ->                    (* Case: new slack *)     
		let k' = mk_slack () in 
		let es' = add_generated_eq s k' a' s.es in
		let slcks' = Set.add k' s.slcks in
		addcnstrnt k' i {s with es = es'; slcks = slcks'}
    
and addcnstrnt x i s =
  let (c', eq) =  C.add x i s.c in
  if consistent (s.e, c') x then 
    let s' = 
      match eq with
	| None -> {s with c = c'}
	| Some(x', n') -> {s with es = add_generated_eq s x' n' s.es; c = c'}
    in
    deduce (A.use s.e x) s' (*s deduce constraints e.g. for [z] in [z=x+y]. *) 
  else 
    raise Exc.Inconsistent

and equality a b s = 
  let a' = A.norm s.e a and b' = A.norm s.e b in
  try
    match Arith.solve_for (is_var_and_not_slack s.slcks) (a', b') with
      | None -> s
      | Some(x',b') -> compose x' b' s
  with
      Exc.Unsolved -> 
	(try
	   (match Arith.solve_for (is_slack s.slcks) (a', b') with
	     | None -> s
	     | Some(k', b') -> compose k' b' s)
	 with
	     Exc.Unsolved -> 
	       abstract a' b' s)

(*s Merging a constraint equality. *)

and compose x a s =
  assert(is_var x);
  let (e', veqs, ys) = A.compose s.e [(x, a)] in
  let s' = {s with e = e'; veqs = Veqs.union veqs s.veqs} in
  deduce ys (cmerge (x, a) veqs s')

and cmerge (x, a) veqs s =
  let eqs = (x, a) :: Veqs.to_list veqs in  (* equalities to be propagated *)
  let (c', es', ch') = C.merge eqs s.c in
  if Set.for_all (consistent (s.e, c')) ch' then  
    let focus = Set.fold (fun y -> Set.union (A.use s.e y)) ch' ch' in
    let s' = {s with es = union_generated_eq s es' s.es; c = c'} in
    deduce focus s' (* e.g. if [z = x + y] and constraint for [x] changes *)
  else              (* then the constraint for [z] might change, too. *)
    raise Exc.Inconsistent


(*s Test if the value for [x] in [e] and its constraint are consistent. *)

and consistent (e, c) x =
  try
    let i = C.apply c x in
    match Arith.d_num (A.apply e x) with
      | Some(q) -> Cnstrnt.mem q i
      | None -> true
  with
      Not_found -> true

(*s Deduce a new constraint from the equality [x = a] in [s]. *)
    
and deduce1 x a s =
    try  
      let j = Arith.cnstrnt (C.apply s.c) a in (* ??? *)
      try
	let i = C.apply s.c x in
	match Cnstrnt.cmp i j with
	  | Binrel.Disjoint ->
	      raise Exc.Inconsistent
	  | Binrel.Sub | Binrel.Same ->
	      s
	  | Binrel.Super ->
	      {s with cs = add_generated_cnstrnt s x j s.cs}
	  | Binrel.Overlap(ij) ->
	      {s with cs = add_generated_cnstrnt s x ij s.cs}
	  | Binrel.Singleton(q) ->
	      let num = Arith.mk_num q in
	      {s with es = add_generated_eq s x num 
			     (add_generated_eq s a num s.es)}
      with
	  Not_found ->
	    (match Cnstrnt.status j with
	       | Status.Empty ->
		   raise Exc.Inconsistent
	       | Status.Singleton(q) ->
		   {s with es = add_generated_eq s x (Arith.mk_num q) s.es}
	       | _ ->
		   {s with cs = add_generated_cnstrnt s x j s.cs})
    with
	Not_found -> s
	

(*s [xs] is a set of variables for which the [find] in [e] has changed. *)

and deduce xs =  
  Trace.msg "a''" "Deduce" (Set.elements xs) (Pretty.set Term.pp);
  Set.fold
    (fun x s ->
       try deduce1 x (A.apply s.e x) s with Not_found -> s)
    xs
       


(*s Both [a] or [b] are nonlinear.  In this case, abstract [a] and [b] with 
 possibly new variables, which are merged. Termination only when new variables 
 are introduced rather lazily. *)

and abstract a b s =
  match name a s.e with
    | Name(x) ->
	(match name b s.e with
	   | Name(y) -> s
	   | Fresh(y', e') -> 
	       let veqs' = Veqs.add (Veq.make x y') s.veqs in
	       {s with e = e'; veqs = veqs'})
    | Fresh(x', e') ->
	(match name b e' with
	   | Name(y) -> 
	       let veqs' = Veqs.add (Veq.make x' y) s.veqs in
	       {s with e = e'; veqs = veqs'}
	   | Fresh(y', e'') ->
	       let veqs' = Veqs.add (Veq.make x' y') s.veqs in
	       {s with e = e''; veqs = veqs'})



(*s Merging in an equation between two variables ['x = y']. 
 In the resulting context, [x] does not occur any more, and
 new equalities between variables may be infered. *)

let rec merge veq ecs = 
  let (x, _) = Veq.destruct veq in
  if not(occurs (ecs.solution, ecs.cnstrnt) x) then   
    (ecs, Veqs.empty)
  else 
    begin
      Trace.call "a" "Merge" veq Veq.pp;
      let (ecs', veqs') = merge1 veq ecs in
      Trace.exit "a" "Merge" veqs' Veqs.pp;
      (ecs', veqs')
    end

and merge1 veq ecs = 
  let s = {
    es = [Veq.destruct veq]; 
    cs = []; 
    e = ecs.solution;
    c = ecs.cnstrnt;
    v = V.empty;
    slcks = ecs.slacks;
    veqs = Veqs.empty} 
  in
  let (ecs', veqs') = process s in
  (ecs', Veqs.remove veq veqs')         (* remove input equality [e]. *)


(*s Adding a constraint ['x in i'] to [s]. *)

let rec add (a, i) ecs = 
  Trace.call "a" "Add" (a, i) pp_in;
  let s = {
    es = []; 
    cs = [(a, i)]; 
    e = ecs.solution; 
    c = ecs.cnstrnt;
    v = V.empty;
    slcks = ecs.slacks;
    veqs = Veqs.empty}
  in
  let (s', veqs') = process s in
  Trace.exit "a" "Add" veqs' Veqs.pp;
  (s', veqs')


(*s Extend. Rename [b] with a variable [x]. *)

let extend b ecs =
  let e = ecs.solution and c = ecs.cnstrnt in
  try
    (A.inv e b, ecs)
  with
      Not_found ->
	Trace.call "a" "Extend" b Term.pp;
	let (x', e') = A.extend b e in
	let c' = 
	  try            (* no deduced eqs, since [x'] is fresh. *)
	    let i = cnstrnt ecs b in
	    let (c', _) = C.add x' i c in
	    c'
	  with
	      Not_found -> c
	in
	Trace.exit "a" "Extend" x' Term.pp;
	(x', {ecs with solution = e'; cnstrnt = c'})

(*s Interpret variable as a slack. *)

let slackify b ecs =
  Trace.call "a" "Slackify" b Term.pp;
  let k = mk_slack () in
  let s = {
    es = [(k, b)];
    cs = []; 
    e = ecs.solution;
    c = ecs.cnstrnt;
    v = V.empty;
    slcks = Set.add k ecs.slacks;
    veqs = Veqs.empty} 
  in
  let (ecs', _) = process s in
  Trace.exit "a" "Slackify" k Term.pp;
  (k, ecs')

let is_slack s x =
  Set.mem x s.slacks
