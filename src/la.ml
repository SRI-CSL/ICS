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

open Mpa

(** The state [s] of the decision procedures for linear arithmetic consists of
  a pair [(s, r)], where
  - [r] is a set of solutions [x = a], where [x] is a nonslack variable, and
  - [t] is a set of solutions [k = b], where [k] is a slack variable and
    all variables in [b] are also slack variables.

  In addition, neither [a] nor [b] are variable terms.  *)

(** {6 Exceptions} *)

(** Indicates {i unboundedness} of a term in a state. *)
exception Unbounded

(** only used loacally to search in sets. *)
exception Found of Term.t 


(** {6 Restrictions} *)

(** All slack variables are considered to be {i restricted}. *)
let is_restricted_var = Term.Var.is_slack

let is_unrestricted_var x = Term.is_var x && not(Term.Var.is_slack x)

(** An arithmetic term is {i restricted} if it only contains
  slack variables at uninterpreted positions. Otherwise, it
  is said to be {i unrestricted}. *)
let is_restricted =
  let is_restricted (_, x) = is_restricted_var x in
    Arith.Monomials.for_all is_restricted

let is_restricted_equality e =
  let (a, b, rho) = Fact.Equal.destruct e in
    is_restricted a && is_restricted b
      
let is_unrestricted a = not(is_restricted a)

let choose_unrestricted a = 
  let is_unrestr (_, z) = is_unrestricted z in
    snd(Arith.Monomials.choose is_unrestr a)

let choose_neg_least a = snd(Arith.Monomials.Neg.least a)
let choose_pos_least a = snd(Arith.Monomials.Pos.least a)

let choose a = 
  snd(Arith.Monomials.choose 
	Arith.Monomials.is_true a)


(** {6 Slack variables} *)

let mk_slack knd a rho =
  let dom = try Arith.dom_of a with Not_found -> Dom.Real in 
  let k = Term.Var.mk_slack None dom knd in
  let rho = Justification.slackify (k, a) rho in
    (k, rho)

(** Create fresh {i nonnegative slack} variable *)
let mk_nonneg_slack = mk_slack Var.Nonneg

(** Create fresh {i zero slack} variable *)
let mk_zero_slack = mk_slack Var.Zero



(** {6 Arithmetic operations} *)

(** [solve e] returns a list of equivalent solved equalities [[e1;...;en]]
  with [ei] of the form [xi = ai] such that [xi] are variables in [e], all
  the [xi] are pairwise disjoint, no [xi] occurs in any of the [ai]. *)
let solve = Fact.Equal.Inj.solver Arith.solve

let solve = 
  Trace.func "la" "Solve"  Fact.Equal.pp (Pretty.list Fact.Equal.pp)
    solve

(**  Solve for an unrestricted variable if possible. *)
let resolve e =
  let (a, b, rho) = Fact.Equal.destruct e in
    match Arith.qsolve (a, b) with
      | None -> 
	  None
      | Some(x, c) -> 
	  let tau = Justification.solve (x, c) rho in
	    Some(Fact.Equal.make (x, c, tau))


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
let apply1 = 
  Fact.Equal.Inj.apply1 Arith.apply1


(** [norm el a] iteratively applies the substitution [el] to [a]. *)
let norm = 
  Fact.Equal.Inj.norm Arith.apply1


(** Transform the equation [e] of the form [a = b] to the equivalent [x = c].  
  Assumes that [x] is a subterm of [a - b]. *)
let isolate x e =
  try
    Fact.Equal.Inj.trans (Arith.isolate x) e
  with 
      Not_found -> failwith "Fatal Error: arithmetic isolation failed"


(** [mk_diff e] for [e] of the form [a = b] constructs the difference
    [a - b] or [b - a] depending on whether the constant becomes positive. *)
let mk_diff e =
  let (a, b, rho) = Fact.Equal.destruct e in
  let a0 = Arith.constant_of a
  and b0 = Arith.constant_of b in
  let diff = 
    if Q.ge a0 b0 then 
      Arith.mk_sub b a
    else 
      Arith.mk_sub a b
  in
    assert(Q.is_nonpos (Arith.constant_of diff));
    (diff, rho)


(** Raise a contradiction. *)
let inconsistent rho = 
  let tau = Justification.contradiction_star [rho] in
    raise(Justification.Inconsistent(tau))


(** Test if [a], [b] in the equality [a = b] have consistent domains. *) 
let is_infeasible (a, b) =
  try
    let d = Arith.dom_of a
    and e = Arith.dom_of b in
      if Dom.disjoint d e then
	let tau = Justification.dependencies [] in
	  Some(tau)
      else 
	None
  with
      Not_found -> None


(** [occurs_negatively x e] holds if [e] is of the form [y = a] 
  and [x] is in [a-]. *)
let occurs_negatively x e  = 
  let a = Fact.Equal.rhs_of e in
    try Q.is_neg (Arith.coefficient_of x a) with Not_found -> false


(** {6 Solution Sets} *)

let is_diseq_num  a b =
  try 
    not(Mpa.Q.equal (Arith.d_num a) (Arith.d_num b))
  with
      Not_found -> false

(** Index for equalities [x = q], where [q] is a rational constant. *)
module QIdx: Eqs.CNSTNT = struct  
  let is_const = Arith.is_num
  let is_diseq = is_diseq_num
end
  

(** [R.t] represents solution sets of the form [x = a], where [x] is
  unrestricted, and [a] is an arithmetic term. *)
module R: Eqs.SET = Eqs.MakeCnstnt(
  struct
    let th = Th.a
    let nickname = Th.to_string Th.a
    let apply = Arith.apply1
  end)
  (QIdx)


(** Specification for an index for the variable [x] such that
  [x = a] with [|a| = 0]. *)
module ZeroIdx: Eqs.INDEX1 = struct
  let name = "zero"
  let holds a = Mpa.Q.is_zero(Arith.constant_of a)
end


(** A {b tableau} [t] consists of equalities [k = a] between a slack 
  variable [k] and terms [a] containing only slack variables.  
  We maintain an index [zero t] such that [k] is in [zero t] iff 
  [k = a] with  constant summand of [a] is  [0]. *)
module T: Eqs.SET = 
  Eqs.MakeIndexCnstnt1
    (struct
       let th = Th.a
       let nickname = "t"
       let apply = Arith.apply1
     end)
    (ZeroIdx)
    (QIdx)


(** Effects for enforcing invariants between the two solution sets [R] and [T]. *)
module Effects = struct
  type left = R.t
  type right = T.t

  let other_cnstnt (r, t) = function
    | Eqs.Right -> T.cnstnt t
    | Eqs.Left -> R.cnstnt r

  let other_find (r, t) = function
    | Eqs.Right -> T.find t
    | Eqs.Left -> R.find r
      
  (** Check for feasibility and guarantee confluence across solution sets. 
    It is ok to restrict with empty effects here. *)
  let do_at_update tag (p, r, t) (x, a, rho) =     (* [rho |- x = a] *)
    match is_infeasible (x, a) with
      | Some(tau) -> 
	  inconsistent tau
      | None ->
	  Fact.Eqs.push (Th.inj Th.a) (Fact.Equal.make (x, a, rho));
	  (try                           (* establish confluence *)
	     (match tag with
		| Eqs.Right ->
		    let (y, tau) = R.inv r a in         (* [tau |- y = a] *)
		    let  sigma = Justification.trans (x, a, y) tau rho in
		      Partition.merge p (Fact.Equal.make (x, y, sigma));
		      R.restrict Eqs.effects0 (p, r) y  (* now restrict [y = a] in [R]. *)
		| Eqs.Left ->
		    let (y, tau) = T.inv t a in         (* [tau |- y = a] *)
		    let  sigma = Justification.trans (x, a, y) tau rho in
		      Partition.merge p (Fact.Equal.make (x, y, sigma));
		      R.restrict Eqs.effects0 (p, r) x) (* restrict [x = a] in [R] *)
	   with
	       Not_found -> ());
	  if Arith.is_num a then         (* generate constant disequalities *)
	    Term.Set.iter                (* across boundaries. *)
	      (fun y ->
		 let (b, tau) = other_find (r, t) tag y in
		   if is_diseq_num a b then
		     let sigma = Justification.dependencies [rho; tau] in
		       Partition.dismerge p (Fact.Diseq.make (x, y, sigma)))
	      (other_cnstnt (r, t) tag)

  let do_at_restrict _ _ _ = ()
end


(** Combined solution set [S = (R; T)]. *)
module S = Eqs.Union(R)(T)(Effects)

type t = S.t

let eq = S.eq
let copy = S.copy
let pp = S.pp
let empty = S.empty
let is_empty = S.is_empty


(** Accessing modes. *)
let r = Eqs.Left
let t = Eqs.Right


let is_dependent s x =
  S.is_dependent t s x || 
  S.is_dependent r s x

let is_independent s x =
  S.is_independent t s x ||
  S.is_independent r s x


(** [Zero] index. *)
module Zero = struct

  (** [zero t] returns the set of [x] with [x = a] in [t] such that
    the constant monomial of [a] is [0]. *)
  let get s = S.index t s 0
 
  (** Apply [f e] to all equalities [k = a] in [t] such that [|a| = 0]. *)
  let iter s f =
    let apply_to k = f (S.equality t s k) in
      Term.Set.iter apply_to (get s)
end
    
(** Iterators for [negdep] index. Hereby, a restricted variable
  [x] is said to {i negatively depend} on [y], if [x = a] is in [T]
  and [x] occurs in [a] with a negative coefficient. *)
module Negdep = struct
  
  (** [negdep t x] contains all [y] such that [y = a] in [t] and
    [x] occurs negatively in [a]. This index is realized as a subindex
    of the dependency structure of equality sets. *)
  let get s x = 
    S.Dep.fold t s
      (fun e acc ->
	 if occurs_negatively x e then
	   let x = Fact.Equal.lhs_of e in
	     Term.Set.add x acc
	 else 
	   acc)
      x Term.Set.empty
     
 
  (** Choose an arbitrary value in the [negdep] index. *)
  let choose s x =
    S.equality t s (Term.Set.choose (get s x))
    
  
  (** Succedds if [f e] succeeds for all equalities [k = a] such  
    that [y] occurs negatively in [a]. *)
  let forall s f y =
    let f' e =  if occurs_negatively y e then f e else true in
      S.Dep.for_all t s f' y
     

  (** Apply [f] to all equalities [k = a] such that [y] occurs
    negatively in [a]. *)
  let iter s f y =
    let f' e = if occurs_negatively y e then f e in
    S.Dep.iter t s f' y
  
end 
  	 
(** [x] is {i unbounded} in [s] if [x] does not occur positively in [t], 
  that is, every coeffient of independent [x] is positive. *)
let is_unbounded s x = 
  Term.Set.is_empty (Negdep.get s x)


(** The gain [gain t x] is the minimum of all [|a|/(-q)]
  such that [y = a] in [t] with [q*x] in [a-], that is, [q < 0]. 
  The second argument is the equality [ y = ... + q * x + ...] 
  with [q < 0] for which the minimum gain is obtained.
  If [x] is {i unbounded} in [s], then [Unbounded] is raised. *)
let rec gain s x = 
  let bound = ref false   (* only access [min] when [bound] is set. *)
  and min = ref (Obj.magic (-1)) in
    Negdep.iter s
      (fun e -> 
	 let (k, a, _) = Fact.Equal.destruct e in
	 let g = gain0 x (k, a) in
	   if not(!bound) then
	     (min := (g, e); bound := true)
	   else 
	     let (g', e') = !min in
	     let cmp = Q.compare g g' in
	       if cmp < 0 || 
		 (cmp = 0 && 
		  let k' = Fact.Equal.lhs_of e' in
	            Term.cmp k k' < 0) 
	       then
		 min := (g, e))
      x;
    if !bound then !min else raise Unbounded

and gain0 x (k, a) =
  assert(Arith.Monomials.Neg.mem x a);
  let q = Arith.coefficient_of x a in
    Mpa.Q.div (Arith.constant_of a) (Mpa.Q.minus q)
 


let apply s x =
  try S.apply t s x with Not_found -> S.apply r s x

let find s x =
  try apply s x with Not_found -> Justification.Eqtrans.id x  

let inv s a =
  try S.inv t s a with Not_found -> S.inv r s a

let dep s x =
  Term.Set.union (S.dep r s x) (S.dep t s x)
 

(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized using {!Arith.map}. *)
let replace s =
  Fact.Equal.Inj.replace Arith.map
    (find s)

(** Either return a fully interpreted term or a canonical variable. *)
let interp (p, s) =
  Fact.Equal.Inj.replace Arith.map
    (Justification.Eqtrans.compose
       (Partition.find p)
       (find s))

(** Return a canonical variable for lhs terms. *)
let uninterp (p, s) a = 
  try
    Justification.Eqtrans.compose
      (Partition.find p)
      (inv s)
      a
  with
      Not_found -> Partition.find p a

(** Canonize a term w.r.t to a partitioning [p] and solution set [s]. *)
let can cfg =
  Justification.Eqtrans.compose
    (uninterp cfg)
    (interp cfg)


(** {6 Iterators} *)

(** Apply [f] to all equalities in [s] and accumulate 
  results starting with [e]. *)
let fold f s e = 
  S.fold r f s (S.fold t f s e)


(** {6 Predicates} *)

(** [is_num s q x] holds if [x = q] in s. *)
let is_num s q x =
  try
    let (a, _) = apply s x in
    let p = Arith.d_num a in
      Mpa.Q.equal q p 
  with
      Not_found -> false

(* Return [(q, rho)] such that [rho |- x = q] or raise [Not_found]. *)
let d_num s x =
  let (a, rho) = apply s x in
  let p = Arith.d_num a in
    (p, rho)

(** Test if [x] and [q] are known to be disequal. *)
let is_num_diseq (p, s) x q =
  let q = Arith.mk_num q in
    try
      let (y, rho) = inv s q in        (* [rho |- y = q]. *)
	(match Partition.is_diseq p x y with
	   | Some(tau) ->              (* [tau |- x <> y] *)
	       let sigma = Justification.subst_diseq (x, q) tau [rho] in
		 Some(sigma)
	   | None -> 
	       None)
    with
	Not_found -> None

(** [is_diseq (p, s) a b] holds if the canonical forms of [a] and [b]
  w.r.t to the configuration [(p, s)] are disequal in the variable 
  partition [p]. *)
let is_diseq (p, s) =
  Justification.Pred2.apply 
    (can (p, s))
    (Partition.is_diseq p)


(** [is_nonzero (p, s) a] succeeds if [a] is disequal in the partition [p]
  to a variable [x] equal to [0]. *)
let is_nonzero cfg =
  is_diseq cfg Arith.mk_zero

let is_nonzero cfg =
  Trace.func "foo4" "is_nonzero" Term.pp (Pretty.option Justification.pp)
    (is_nonzero cfg)
    

(** {6 Basic Updates} *)

type config = Partition.t * S.t

let protect (p, s) f a =
  let p' = Partition.copy p   (* Protecting configurations against destructive *)
  and s' = S.copy s in        (* updates. Also, disable propagation of newly   *)
    Fact.with_disabled_stacks (* generated facts on stacks. *) 
      (f (p', s')) a


(** Return a variable for a term [a], possibly extending [R]
  with a fresh variable. First, canonize to ensure that all
  dependent variables are removed. *)
let name cfg =
  Justification.Eqtrans.compose
    (S.name r cfg)
    (interp cfg)

(** Composing a list of solved equalities into solution set for mode [m]. *)
let compose1 tag cfg e =
  Trace.msg "la" "Compose" e Fact.Equal.pp;
  match tag with
    | Eqs.Left ->
	S.compose r cfg [e] 
    | Eqs.Right -> 
	S.fuse r cfg [e];     (* to do: add lazy fusion for R. *)
	S.compose t cfg [e]
	  

(** {6 Processing} *)


(** Process an equality [e].
  - An equality that contains a unrestricted rational variable, is solved
    for that variable.
  - An equality containing only restricted rational variables is processed
    by introducing a slack variable.
  - If it contains only integer variables, then we apply the integer Euclidean
    solver.

  Given [a = b], if there is an unrestricted [y] in [a = b], solve for [y] 
  and compose with [R].  Otherwise, all variables are restricted.
  Rearrange as [k = b - a], where [|b-a|] is non-positive. Process [k = b - a] 
  as in the inequality case to convert it into a feasible equality [k' = a'].  
  In particular, whenever the right-hand side contains a positive unbounded variable 
  but the constant is negative, solve the equality for the unbounded and compose.  
  If [k /= k'], then [k'] appears on the rhs, compose [T] with [k = 0]. If [k = k'], 
  the following cases arise:
   - Case [|a'| <0|].  Then [a'] contains no positive variables and [k] is maximized
     below [0].  Report infeasibility.
   -  Case |a'| = 0.  If [a'] contains a positive or negative variable [y], solve [k = a'] 
     for [y] and compose with [T].  This is safe (preserves feasibility) since the
     constant remains at [0].  We can now compose in [k = 0].  If [a'] does not
     contain a variable, then [k] is equal to [0] and there is nothing to do,
     but report back that a is indeed equal to b.
   - Case [|a'| > 0|].  If [a'] contains no negative variables, then report infeasibility 
     since [k] is minimized at [|a'|]. If [a'] contains negative variables, then we see 
     if there is a negative variable [y] in [a'] such that the gain of [y] in [T] is
     no smaller than the gain of [y] in [k = a'].  If so, we can pivot [y] in [k = a'], 
     and compose [k = 0].   If not, we can pivot the least [y] (occurring negatively in [a']) 
     in [T] and repeat with [k = a''].  Eventually, all the negative variables will have been
     eliminated or we are back to one of the previous cases.  Note that [|a'|] will never 
     go below [0] in this process. *)
let rec process_equal ((p, s) as cfg) e =  
  let e = Fact.Equal.map (replace s) e in
    Trace.msg "la" "Process" e Fact.Equal.pp;
    match solve e with
      | [] -> ()
      | e :: el -> 
	  process_solved cfg e;           (* because of resolving, dependent variables *)
	  List.iter                       (* must be plugged in again for all but the  *)
	    (fun e ->                     (* first equality. *)
	       let e = Fact.Equal.map (replace s) e in
		 match resolve e with
		   | None -> ()
		   | Some(e) -> process_solved cfg e)
	    el
      
and process_solved ((p, s) as cfg) e = 
  let (x, a, rho) = Fact.Equal.destruct e in
    assert(Term.is_var x);
    if is_unrestricted_var x then
      compose1 r cfg e
    else
      process_solved_restricted cfg e

and process_solved_restricted ((p, s) as cfg) e =
  assert(is_restricted_equality e);
  try
    compose1 t cfg 
      (try_isolate_unbounded s e)
  with
      Not_found -> 
	let (diff, rho) = mk_diff e in                   (* [rho |- diff = 0] *)
	  assert(Mpa.Q.is_nonpos (Arith.constant_of diff));
	  let (k, tau) = mk_zero_slack diff rho in       (* [tau |- k = diff] *)
	    add_to_t cfg (Fact.Equal.make (k, diff, tau));   
	    infer cfg;
	    (try
	       let (a', rho') = S.apply t s k in         (* [rho' |- k = a'] *)
	       let e' = Fact.Equal.make (k, a', rho') in
	       let cmp = Mpa.Q.compare (Arith.constant_of a') Mpa.Q.zero in
		 if cmp < 0 then                 (* I. [|a'| > 0] *)
		   inconsistent 
		     (Justification.dependencies [rho])
		 else if cmp = 0 then            (* II. [|a'| = 0] *)
		   (try 
		      compose1 t cfg 
			(isolate (choose a') e') 
		    with 
			Not_found -> ())  (* skip *)
		 else                            (* III. [a0' > 0] *)
		   if Arith.Monomials.Neg.is_empty a' then
		     inconsistent 
		       (Justification.dependencies [rho])
		   else 
		     (try
			let y = choose_negvar_with_no_smaller_gain s a' in
			  pivot cfg y;
			  let sigma = Justification.trans (k, diff, Arith.mk_zero) rho tau in
			    compose1 t cfg 
			      (Fact.Equal.make (k, Arith.mk_zero, sigma))
		      with
			  Not_found -> 
			    assert(not(Arith.Monomials.Neg.is_empty a'));
			    pivot cfg (choose_neg_least a');
			    process_solved_restricted cfg 
			      (Fact.Equal.map_rhs (replace s) e'))
	     with           
		 Not_found ->  (* [k] is not a dependent variable *)
		   let sigma = Justification.trans (k, diff, Arith.mk_zero) rho tau in
		     compose1 t cfg 
		       (Fact.Equal.make (k, Arith.mk_zero, tau)))

(* If [a'] contains negative variables, then we see 
   if there is a negative variable [y] in [a'] such that the 
   gain of [y] in [T] is no smaller than the gain of [y] in [k = a'].  *)
and choose_negvar_with_no_smaller_gain s a' =
  let c0 = Arith.constant_of a' in
    snd(Arith.Monomials.Neg.choose
	  (fun (q, y) ->
	     let g0 = Mpa.Q.div c0 (Mpa.Q.minus q) in (* gain of [y] in [k = a'] *)
	     let (g1, _) = gain s y in                (* gain of [y] in [t] *)
	       Mpa.Q.ge g1 g0)
	  a')       
     

(** Process a nonnegativity constraint [c] of the form [a >= 0]. *)
and process_nonneg ((_, s) as cfg) c = 
  let c = Fact.Nonneg.map (replace s) c in 
    Trace.msg "la" "Process" c Fact.Nonneg.pp;
    let (a, rho) = Fact.Nonneg.destruct c in           (* [rho |- a >= 0] *)
      match Arith.is_nonneg a with
	| Three.Yes -> () 
	| Three.No -> inconsistent rho
	| Three.X -> 
	    let (k, tau) = mk_nonneg_slack a rho in    (* [tau |- k = a] *)
	    let e = Fact.Equal.make (k, a, tau) in
	      try
		let e' = isolate (choose_unrestricted a) e in
		  compose1 r cfg e'
	      with
		  Not_found ->
		    add_to_t cfg e;
		    infer cfg
		  
    
and add_to_t ((_, s) as cfg) e =
  let (k, a, rho) = Fact.Equal.destruct e in
    assert(is_restricted_var k && is_restricted a);
    Trace.msg "la" "Add_to_t" e Fact.Equal.pp;
    let c = Arith.constant_of a in
      if Q.is_nonneg (Arith.constant_of a) then
	compose1 t cfg e
      else if  Arith.Monomials.Pos.is_empty a then
	raise(Justification.Inconsistent(rho))
      else
	try
	  let e' = try_isolate_unbounded s e in                  (* k = a == y = b *)                     
	  let (y, b, _) = Fact.Equal.destruct e' in
	    if Term.Var.is_zero_slack k then
	      let rho' = Justification.dependencies [rho] in     (* [rho' |- k = 0] *)
	      let (b', rho'') = 
		apply1 (Fact.Equal.make (k, Arith.mk_zero, rho')) b 
	      in  
		compose1 t cfg                                   (* [rho'' |- y=b[k:=0]] *)  
		  (Fact.Equal.make (y, b', rho'')) 
	    else 
	      let tau' = Justification.dependencies [rho] in     (* [tau' |- y = b] *)     
		compose1 t cfg 
		  (Fact.Equal.make (y, b, tau'))
	with
	    Not_found ->  
	      assert(not(Arith.Monomials.Pos.is_empty a));
	      let y = choose_pos_least a in
		pivot cfg y;
		let (a', rho') = replace s a in                  (* [rho' |- a = a'] *)
		  assert(not(Term.subterm y a'));
		  let rho' = Justification.trans (k, a, a') rho rho' in
		    add_to_t cfg                                 (* [rho' |- k = a'] *)
		      (Fact.Equal.make (k, a', rho')) 


(** [x = a == y = a'] with [y] unbounded in [s]; 
  otherwise [Not_found] is raised. *)
and try_isolate_unbounded s e =
  let is_unb (_, x) =  is_unbounded s x in
  let a = Fact.Equal.rhs_of e in
  let (_, y) = Arith.Monomials.Pos.choose is_unb a in
    isolate y e


and pivot ((_, s) as cfg) y =
  assert(not(is_unbounded s y));
  try
    let (_, e) = gain s y in
    let e' = isolate y e in                
      Trace.msg "la" "Pivot" (e, e') (Pretty.infix Fact.Equal.pp " ==> " Fact.Equal.pp);
      compose1 t cfg e'
  with
      Not_found -> failwith "Fatal Error: Failed pivot step"
	

and infer cfg =
  let zs = analyze cfg in
    maximize cfg zs


(** The maximize step need only be applied to dependent variables [k] s.t.,
  [isZero(k)].  If [k = a+ + a-] in [t], then repeat: 
  - if [a+] is empty, then it is maximized at [0], and all the variables
  in k = [a-] can be set to [0].  
  - If [a+] contains an unbounded variable, then skip, since it is certainly 
  not equal to [0].  
  - Otherwise, if [a+] contains a variable [y] s.t., [isZero y] is false, then skip.  
  - Otherwise, pivot the first variable in [a+] in [t], to obtain [k = a']. *)
and maximize ((_, s) as cfg) zeros = 
  Term.Set.iter
    (fun k ->
       try
	 let (a, rho) = S.apply t s k in
	   maximize1 cfg zeros (k, a, rho)
       with
	   Not_found -> ())
    zeros


and maximize1 ((_, s) as cfg) zeros (k, a, rho) =   (* [rho |- k = a] *)
  Trace.msg "la" "Maximize" (k, a) Term.Equal.pp;
  let monomial_is_unbounded (_, y) = is_unbounded s y in
  let monomial_is_nonzero (_, y) = not(Term.Set.mem y zeros) in
  if Arith.Monomials.Pos.is_empty a then
    set_to_zero cfg (k, a, rho)
  else if Arith.Monomials.Pos.exists monomial_is_unbounded a then
    ()
  else if Arith.Monomials.Pos.exists monomial_is_nonzero a then
    ()
  else 
    let (_, y) = Arith.Monomials.Pos.least a in
      assert(not(is_unbounded s y));
      pivot cfg y;
      maximize cfg zeros


and set_to_zero cfg (x, a, rho) =
  Trace.msg "la" "Zero" (x, a) Term.Equal.pp;
 (* assert(Mpa.Q.is_zero(Arith.constant_of a)); *)
  assert(Arith.Monomials.Pos.is_empty a);
  Arith.Monomials.Neg.iter
    (fun (_, y) ->
       let tau = Justification.dependencies [rho] in
       let e = Fact.Equal.make (y, Arith.mk_zero, tau) in
	 compose1 t cfg e)
    a


(** Associated with each variable [x] in [t] is a bit [is_zero x].  
  Initially, all the bits are false.  In the first step, all 
  independent variables [x] with gain [0] have [is_zero x] set to 
  true.  These are the variables that occur negatively in a rhs where 
  the constant is [0]. Repeat:
  If for some [y] such that [is_zero y], 
  it is the case that for each [k] in [negdep(y)] s.t. [find(k) = a+ + a-], 
  there is an [x] in [a+] such that not [is_zero x], then set 
  [is_zero y] to false. Finally, for dependent [k], s.t. [k = a+ + a-] in [t], 
   if forall [y] in [a+], [is_zero(y)], then [is_zero(k)] is set to true, too. *)
and analyze ((_, s) as cfg) =
  let current = ref Term.Set.empty in 
  let is_zero (_, y) = Term.Set.mem y !current in
  let not_is_zero (_, y) = not(Term.Set.mem y !current) in
    Zero.iter s
      (fun e ->                  (* [|a| = 0] *)
	 let a = Fact.Equal.rhs_of e in
	   Arith.Monomials.Neg.iter 
	     (fun (_, y) -> 
		current := Term.Set.add y !current)
	     a);
    (try
       while true do
	 let y = 
	   (try                (* choose [y] such that [is_zero y] and *)
	      Term.Set.iter     (* for all [k] in [negdep y] and [k = a] in [t] *)
		(fun y ->       (* there is an [x] in [a+] such that [not(is_zero x)]. *)
		   if (Negdep.forall s
			 (fun e ->
			    let a = Fact.Equal.rhs_of e in
			      Arith.Monomials.Pos.exists not_is_zero a)
			 y)
		   then
		     raise(Found(y)))
		!current;
	      raise Not_found
	    with
		Found(y) -> y)
	 in
	   current := Term.Set.remove y !current
       done
     with 
	 Not_found -> ());
    Zero.iter s
      (fun e ->                     (* [k = a] with [|a| = 0]. *)
	 let k, a, _ = Fact.Equal.destruct e in
	   if Arith.Monomials.Pos.for_all is_zero a then
	     current := Term.Set.add k !current);
    !current


(** Processing disequalities only deals with integer disequalities,
  which are maintained in the form [e <> n] where [n] is a natural 
  number. *) 
let rec process_diseq ((p, s) as cfg) d =
  Trace.msg "la" "Process" d Fact.Diseq.pp;
  let d = Fact.Diseq.map (replace s) d in    (* replace dependent variables. *)
    if Fact.Diseq.is_diophantine d then
      process_diophantine_diseq cfg d
    else 
      process_nondiophantine_diseq cfg d 
	

(** Nondiophantine disequalities are variable-abstracted
  and added to the disequalities in the variable partitioning. *)
and process_nondiophantine_diseq ((p, _) as cfg) d =
  let d' = Fact.Diseq.map (name cfg) d in
    assert(Fact.Diseq.is_var d');
    Partition.dismerge p d'
	
(* Whenever we add [e<>n], we calculate the largest contiguous segment [N] 
   containing [n] such that [e<>m] for [m] in [N], then with [l = min(N)], and [h = max(N)], 
   separately assert [e <= l-1] and [e>=h+1].  If both calls yield contradictions,
  then we report a contradiction.  If only one of the calls
  yields a contradiction, then we press on with the other one.
  (Any equalities generated from the inequality are propagated
  to the variable equality and disequality parts).  
  If both calls are free of contradiction, then we revert to
  the original state and continue processing inputs. *)
and process_diophantine_diseq cfg d =
  let (e, n, rho) = Fact.Diseq.d_diophantine d in
  let (min, max, taus) =                   (* [taus] together prove *)
    contiguous_diseq_segment cfg (e, n)    (* [e] not in [[min+1, max-1]]. *)
  in
  let a' = Arith.mk_sub (Arith.mk_num min) e         (* [a' = min - e] *)
  and a'' = Arith.mk_sub e (Arith.mk_num max) in     (* [a'' = e - max] *)
    match is_nonneg_unsat cfg a' with                (* Case [e <= min] *)
      | None -> 
	  (match is_nonneg_unsat cfg a''  with       (* Case [max <= e] *)
	     | None ->
		 process_nondiophantine_diseq cfg d
	     | Some(tau'') ->                        (* [tau'' |- max > e] *)
		 let rho' = Justification.dependencies (tau'' :: taus) in 
		   try 
		     process_nonneg cfg              (* now: [rho' |- e <= min] *)
		       (Fact.Nonneg.make (a', rho')) (* reprocess! *)
		   with
		       Justification.Inconsistent _ -> 
			 failwith "Unsoundness.")
      | Some(tau') ->                                (* [tau' |- e > min] *)   
	  let rho'' = Justification.dependencies (tau' :: taus) in  
	  let c'' = Fact.Nonneg.make (a'', rho'') in (* now: [rho'' |- e >= max] *)
	    process_nonneg cfg c''                  

and is_nonneg_unsat (p, s) a =
  let c = Fact.Nonneg.make (a, Justification.dependencies []) in
  let process_nonneg' cfg =
    Trace.proc "la" "Case" Fact.Nonneg.pp (process_nonneg cfg)
  in
    try     
      protect (p, s) process_nonneg' c;
	None
    with
	Justification.Inconsistent(rho) -> Some(rho)

	   
and contiguous_diseq_segment (p, s) (e, n) = 
  let taus = ref [] in
  let rec upper max =                           (* [rho |- e <> n] *)
    let max' = Q.add max Q.one in
      match is_num_diseq (p, s) e max' with 
	| Some(tau) ->                       (* [tau |- e <> max'] *)
	    taus := tau :: !taus;
	    upper max'             (* only succeeds finitely often *)   
	| None -> 
            max
  in
  let rec lower min =
    let min' = Q.sub min Q.one in
      match is_num_diseq (p, s) e min' with
	| Some(tau) -> 
	    taus := tau :: !taus;
	    lower min'
	| None ->
	    min
  in
  let (max, min) = (lower n, upper n) in
    (max, min, !taus)

  
(** {6 Maximization} *)    

(** Update [s] such that [x] is a dependent variable whenever
  this variable occurs in [s] *)
let make_dependent ((_, s) as cfg) x =
  assert(is_restricted_var x);
  try
    let e = S.Dep.choose t s x in     (* choose [y = a] such that [x] occurs in [a], *)
      compose1 t cfg (isolate x e)    (* and solve [y = a] for [x]. *)
  with
      Not_found -> ()


(** Maximize [a] in [s] by systematically eliminating positive 
  monomials in the canonical form [a'] of [a].  It returns either 
  - [(b, rho') such that [b+] is empty and [rho' |- a = b], or
  - raises [Unbounded] if [a] is unbounded. *)
let maximize ((p, s) as cfg) a =
  let rec max_term a (b, rho) =                (* [rho |- b = a] *)
    Trace.msg "foo4" "Maximize(term)" a Term.pp;
    if Term.is_var a then
      max_var a (b, rho)
    else 
      try
	let (_, x) = Arith.Monomials.Pos.least a in  (* choose least [x] in [a+]. *)
	let (_, e) = gain s x in          (* [y = ... + q*x + ...] with [q < 0].  *)
	let e' = isolate x e in           (* Isolate [x] in [e] and substitute *)
	let (a', rho') = apply1 e' a in   (* [rho' |- a = a'] *)
	let tau = Justification.trans (b, a, a') rho rho' in
	  max_term a' (b, tau)            (* [tau |- b = a'] *)
      with
	  Not_found -> 
	    assert(Arith.Monomials.Pos.is_empty a);
	    (a, rho)
  and max_var x (b, rho) =             (* [rho |- x = b] *)
    assert(is_restricted_var x);
    Trace.msg "foo4" "Maximize(term)" x Term.pp;
    make_dependent cfg x;              (* make [x] a dependent variable. *)
    try
      let (a, rho') = S.apply t s x in (* [rho' |- x = a] *)
      let tau = Justification.trans (a, x, b) rho rho' in
	max_term a (b, tau)
    with
	Not_found -> raise Unbounded
  and max_term_top cfg (b, a, rho) =
    max_term b (a, rho)
  in
  let (b, rho) = interp cfg a in
    if Arith.is_num b then
      (b, rho)
    else if is_unrestricted b then 
      raise Unbounded
    else
      protect cfg
	max_term_top (b, a, rho)     (* [rho |- a = b] *)


let minimize cfg a = 
  let (b, rho) = maximize cfg (Arith.mk_neg a) in
    (Arith.mk_neg b, rho)


let sup cfg a =
  let (b, rho) = maximize cfg a in
    assert(Arith.Monomials.Pos.is_empty b);
    (Arith.constant_of b, rho)


let inf cfg a =
  let (b, rho) = minimize cfg a in
    assert(Arith.Monomials.Neg.is_empty b);
    (Arith.constant_of b, rho) 


(** Test if [a <= 0]. *)
let is_nonpos cfg a =
  try
    let (a', rho') = maximize cfg a in
      assert(Arith.Monomials.Pos.is_empty a');
      let q = Arith.constant_of a' in
	if Q.is_nonpos q then 
	  Some(rho')
	else 
	  None
  with
      Unbounded -> None

exception Found of Justification.t

let rec is_neg cfg a =
  try
    let (a', rho) = maximize cfg a in
      assert(Arith.Monomials.Pos.is_empty a');
      let q = Arith.constant_of a' in
	if Q.is_neg q then 
	  Some(rho)
	else if Q.is_nonneg q then
	  match neg_is_nonzero cfg a' with
	    | Some(tau) -> 
		let sigma = Justification.dependencies [rho; tau] in
		  Some(sigma)
	    | None ->
		None
	else 
	  None
  with
      Unbounded -> None

and neg_is_nonzero cfg a =
  assert(Arith.Monomials.Pos.is_empty a);
  let nonzero (_, k) =  is_nonzero cfg k in
    try
      Arith.Monomials.Neg.iter
	(fun m -> 
	   match nonzero m with
	     | Some(tau) -> raise(Found(tau))
	     | None -> ())
	a;
      None
    with
	Found(tau) -> Some(tau)
    

(** Test if [a >= 0]. *)
let is_nonneg cfg a = is_nonpos cfg (Arith.mk_neg a)


(** Test if [a > 0]. *)
let is_pos cfg a = is_neg cfg (Arith.mk_neg a)


let is_nonneg cfg =
  Trace.func "foo4" "is_nonneg" Term.pp (Pretty.option Justification.pp)
    (is_nonneg cfg)


let is_pos cfg =
  Trace.func "foo4" "is_pos" Term.pp (Pretty.option Justification.pp)
    (is_pos cfg)


let is_neg cfg =
  Trace.func "foo4" "is_neg" Term.pp (Pretty.option Justification.pp)
    (is_neg cfg)



(** {6 Finite Domains} *)

module Finite = struct

  module Zset = Set.Make(
    struct
      type t = Mpa.Z.t
      let compare = Mpa.Z.compare
    end)

  type t = {
    lo : Mpa.Z.t;
    hi : Mpa.Z.t;
    diseqs : Zset.t
  }

  let pp fmt fin = 
    let lo = Pretty.to_string Mpa.Z.pp fin.lo
    and hi = Pretty.to_string Mpa.Z.pp fin.hi in
      if Zset.is_empty fin.diseqs then
	Format.fprintf fmt "[%s..%s]" lo hi
      else 
	let ds = Pretty.to_string (Pretty.set Mpa.Z.pp) 
		   (Zset.elements fin.diseqs) 
	in
	  Format.fprintf fmt "[%s..%s] - %s" lo hi ds

  let rec make (lo, hi, ds) =
    if Mpa.Z.gt lo hi then
      raise Unbounded
    else if Zset.mem lo ds then
      make (Mpa.Z.add lo Mpa.Z.one, hi, Zset.remove lo ds)
    else if Zset.mem hi ds then
      make (lo, Mpa.Z.sub hi Mpa.Z.one, Zset.remove hi ds)
    else 
      {lo = lo; hi = hi; diseqs = ds}

  let rec of_var cfg x = 
    if not(Term.Var.is_int x) then 
      raise Unbounded
    else 
      let (hi, _) = sup cfg x
      and (lo, _) = inf cfg x in
      let ds = integer_diseqs_between cfg x (lo, hi) in
	make (Mpa.Q.floor lo, Mpa.Q.floor (Mpa.Q.add lo Mpa.Q.one), ds)
	
  and integer_diseqs_between ((p, s) as cfg) x (lo, hi) =
    D.Set.fold
      (fun (y, _) acc ->
	 try
	   let (q, _) = d_num s y in  (* [y = q] *)
	     if Mpa.Q.is_integer q && Mpa.Q.le lo q && Mpa.Q.le q hi then
	       Zset.add (Mpa.Q.to_z q) acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      (Partition.diseqs p x)
      Zset.empty

  (** Construct bindings [x |-> fin] for the subset of [xs] with finite
    interpretation. *)
  let of_vars cfg xs =
    Term.Set.fold
      (fun x acc ->
	 try
	   let fin = of_var cfg x in
	     Term.Map.add x fin acc
	 with
	     Unbounded -> acc)
      xs

  let of_config ((_, s) as cfg) =
    let of_equal x (a, _) = 
      let xs = Term.Set.add x (Term.vars_of a) in
	of_vars cfg xs
    in
      fold of_equal s Term.Map.empty
	
end 


(*

(**  [interp (d, s) x] returns either 
  - [(q, true)] if [q] is a nonstrict upper bound,
  - [(q, false)] if [q] is a strict upper bound, or
  - raise [Unbounded], if [x] is unbounded. 

  If [x] is a slack variable, we make [x] a dependent variable and maximize
  [find t x].  If [find t x] is unbounded, we return infinity.  
  If [x] is a nonslack, then we make [x] dependent in R, decompose
  [find r x] into [w + u], where the variables in [w] are restricted and 
  those in [u] unrestricted.  We can then maximize [w] to [a], and return 
  [a] + [u].   If there are any numeric disequalities [x<>ni], then we 
  pick the largest compatible [ni].  If this coincides with [x], then 
  we choose, [x - epsilon] as the value for [x].  *)

let rec interpretation (d, s) x =
  if is_restricted x then
    let (m, alpha) = 
      interp_of_restricted (d, s) x
    in
      (Arith.mk_num m, alpha)
  else 
    let (m, a, alpha) = 
      interp_of_unrestricted (d, s) x
    in
      (Arith.mk_addq m a, alpha)

and interp_of_restricted (d, s) k =
  make_dependent s k;           (* make [k] a dependent variable. *)
  let (b, _) = T.find s.t k in
  let m = maximizes b in
    (m, true)

and interp_of_unrestricted (d, s) x = 
  make_dependent s x;        (* make [x] a dependent variable. *)
  let (b, _) = R.find s.r x in       (* [x = b] *)
  let (n, w, u) =                    (* [b = n + w + u] *)
    Arith.partition (fun (_, y) -> is_restricted y) b 
  in
  let m = Q.add n (maximizes w) in
    (m, u, true)

*)

let interpretation_of (d, s) x =
  failwith "interpretation_of : to do" 
(*
  try
    (match interpretation (d, s) x with
       | m, true -> m
       | m, false -> Arith.mk_sub m Arith.mk_eps)
  with
      Unbounded -> Arith.mk_inf
*)

let model ((_, s) as cfg) xs =
  let s = S.copy s in         (* protect state against updates. *)
  let m = ref Term.Map.empty in
    Term.Set.iter
      (fun x ->
	 let a = interpretation_of cfg x in
	   m := Term.Map.add x a !m;
	   let e = Fact.Equal.make (x, a, Justification.dependencies []) in 
	     compose1 t cfg e)
      xs;
    !m
