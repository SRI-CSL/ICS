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

(** The state [s] of the decision procedures for linear arithmetic is separated
  into {i unrestricted} and {i restricted} solved equalities and consists 
  of a pair [(r, t)], where
  - [r] is a set of solutions [x = a], where [x] is {i unrestricted},
    that is, a nonslack variable, and
  - [t] is a set of solutions [k = b], where [k] is a {i restricted} 
    slack variable and all variables in [b] are also {i restricted}.

  In addition, neither [a] nor [b] are variable terms. For the rhs [a] we
  sometimes use the notation [|a|] for the constant monomial, [a+] for the
  positive monomials, and [a-] for the negative monomials in [a]. 

 The union of the solution sets [(r, t)] is {i functional} in that there 
  is no [x = a1] and [x = a2] in [(s, r)], and it is {i inverse functional} 
  in that there are no [x1 = a] and [x2 = a].  

  We maintain various indices. 
  - The {i constant} index records all [x]s in the domain of [(r, t)]
    with [x = q] with [q] a rational number.
  - The {i zero} index records all [k]s in the domain of [t] 
    with [|a|] equals zero.
  - The {i negdep} index records for [y] all [x] in [t] such that 
    [x = a] in [t] and [y] is in [a-]; that is [y] occurs with negative
    coefficient. 

  The notations used are
  - [q], [p] for rational numbers,
  - [a], [b] for arbitrary terms, and [x], [y], [z] for variable terms
  - [s] for a solution state [(r, t)] and [r], [t] for the respective components,
  - [p] for variable partitionings of type {!Partition.t},
  - [e] for equality facts ({!Fact.Equal.t}), [d] for disequality 
    facts ({!Fact.Diseq.t}), and [c] for arithmetic constraints such as
    constraints of type {!Fact.Nonneg.t}, and
  - Greek letters for justifications ({!Jst.t}).
*)

(** {6 Exceptions} *)

(** Indicates {i unboundedness} of a term in a state. *)
exception Unbounded

(** only used locally to search in sets. *)
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
      
let is_unrestricted a = not (is_restricted a)

let choose_unrestricted a =
  let is_unrestr (_, z) = is_unrestricted z in
    snd(Arith.Monomials.choose is_unrestr a)

let choose a = snd(Arith.Monomials.choose Arith.Monomials.is_true a)
let choose_neg_least a = snd(Arith.Monomials.Neg.least a)
let choose_pos_least a = snd(Arith.Monomials.Pos.least a)

(** Replace the zero slack [k0] with [0] in the argument term. *)
let replace_zero_slacks = 
  let lookup x = 
    if Term.Var.is_zero_slack x then
      Arith.mk_zero()
    else 
      x 
  in
    Arith.map lookup 



(** {6 Arithmetic operations} *)

(** [solve e] returns a list of equivalent solved equalities [[e1;...;en]]
  with [ei] of the form [xi = ai] such that [xi] are variables in [e], all
  the [xi] are pairwise disjoint, no [xi] occurs in any of the [ai]. *)
let solve =
  Trace.func "la'" "Solve"  
    Fact.Equal.pp 
    (Pretty.list Fact.Equal.pp)
    (Fact.Equal.equivn Arith.solve)


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
let apply1 e = 
  let (x, a, rho) = Fact.Equal.destruct e in
  let lookup y = if Term.eq x y then (a, rho) else Jst.Eqtrans.id y in
    Jst.Eqtrans.replace Arith.map lookup

(** Transform the equation [e] of the form [a = b] to the equivalent [x = c].  
  Assumes that [x] is a subterm of [a - b]. *)
let isolate x e =
  try
    Fact.Equal.equiv (Arith.isolate x) e
  with 
      Not_found -> failwith "Fatal Error: arithmetic isolation failed"

(** Fusing an equality [x = a] into right-hand sides of a solved list. *)
let solved_fuse e =
  let (x, a, rho) = Fact.Equal.destruct e in
  let rec loop acc = function
    | [] -> acc
    | s :: sl -> 
	let (y, b, tau) = Fact.Equal.destruct s in
	let b' = Arith.apply (x, a) b in
	let s' = if b == b' then s else Fact.Equal.make (y, b', Jst.dep2 rho tau) in
	let acc' = s' :: acc in
	  loop acc' sl
  in
    loop []


(** [mk_diff e] for [e] of the form [a = b] constructs the difference
  [a - b] or [b - a] depending on whether the constant becomes positive. *)
let mk_diff e =
  let (a, b, rho) = Fact.Equal.destruct e in
  let a0 = Arith.constant_of a and b0 = Arith.constant_of b in
  let diff = if Q.ge a0 b0 then Arith.mk_sub b a else Arith.mk_sub a b in
    assert(Q.is_nonpos (Arith.constant_of diff));
    (diff, rho)
   

(** [occurs_negatively x e] holds if [e] is of the form [y = a] 
  and [x] is in [a-]. *)
let occurs_negatively x e  = 
  let a = Fact.Equal.rhs_of e in
    try Q.is_neg (Arith.coefficient_of x a) with Not_found -> false


(** [R.t] represents solution sets of the form [x = a], where [x] is
  unrestricted, and [a] is an arithmetic term. *)
module R: (Eqs.SET with type ext = unit) = 
  Eqs.Make0(
    struct
      let th = Th.la
      let nickname = Th.to_string Th.la
      let map = Arith.map
      let is_infeasible = Arith.is_infeasible
    end)


(** Specification for an index for the variable [x] such that
  [x = a] with [|a| = 0]. *)
module ZeroIdx: Eqs.INDEX = struct
  let name = "zero"
  let holds a = Mpa.Q.is_zero(Arith.constant_of a)
end


(** A {b tableau} [t] consists of equalities [k = a] between a slack 
  variable [k] and terms [a] containing only slack variables.  
  We maintain an index [zero t] such that [k] is in [zero t] iff 
  [k = a] with  constant summand of [a] is  [0]. *)
module T: (Eqs.SET with type ext = Eqs.index) = 
  Eqs.MakeIndex
    (struct
       let th = Th.la
       let nickname = "t"
       let map = Arith.map
       let is_infeasible = Arith.is_infeasible
     end)
    (ZeroIdx)


(** Combined solution set [S = (R; T)]. *)
module S =
  Eqs.Union(struct let th = Th.la end)(R)(T)

type t = S.t

let eq = S.eq
let copy = S.copy
let pp = S.pp
let empty = S.empty
let is_empty = S.is_empty


(** Accessing modes. *)
let r = S.Left
let t = S.Right


let is_dependent s x =
  if is_restricted_var x then
    S.is_dependent t s x
  else 
    S.is_dependent r s x

let is_independent s x =
  S.is_independent t s x ||
  S.is_independent r s x


(** [Zero] index. *)
module Zero = struct

  (** [get t] returns the set of [x] with [x = a] in [t] such that
    the constant monomial of [a] is [0]. *)
  let get s = 
    let (_, zero) = S.ext s in
      zero
 
  (** Apply [f e] to all equalities [k = a] in [t] such that [|a| = 0]. *)
  let iter s f =
    let apply_to k =
      let e = 
	try 
	  S.equality t s k 
	with 
	    Not_found ->
	      Format.printf "\nError in Zero iter. Failed application of %s in\n"  
	         (Term.to_string k);  
	      S.pp Format.std_formatter s;
	      Format.printf "@.";
	      failwith "Terminate"
      in
         f e
    in
      Term.Var.Set.iter apply_to (get s)
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
	   let y = Fact.Equal.lhs_of e in
	     Term.Set.add y acc
	 else 
	   acc)
      x Term.Set.empty

 
  (** Choose an arbitrary value in the [negdep] index. *)
  let choose s x =
    S.Dep.choose t s (occurs_negatively x)


  (** Check if [negdep] is empty for [x]. *)
  let is_empty s x = 
    Term.Set.is_empty (get s x)

  
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
let is_unbounded = Negdep.is_empty


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
  if is_restricted_var x then
     S.apply t s x
  else 
    S.apply r s x


let find s x =
  try apply s x with Not_found -> Jst.Eqtrans.id x  

let inv s a =
  try S.inv t s a with Not_found -> S.inv r s a

let dep s x =
  Term.Var.Set.union (S.dep r s x) (S.dep t s x)

let equality tag =
  match tag with
    | S.Left -> S.equality r
    | S.Right -> S.equality t


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized using {!Arith.map}. *)
let replace s =
  Jst.Eqtrans.replace Arith.map (find s)

(** Either return a fully interpreted term or a canonical variable. *)
let interp (p, s) =
  Jst.Eqtrans.replace Arith.map
    (Jst.Eqtrans.compose
       (Partition.find p)
       (find s))

let uninterp (_, s) =
  Jst.Eqtrans.totalize (inv s)


(** {6 Slack variables} *)

(** Create fresh {i nonnegative slack} variable *)
let rec mk_nonneg_slack cfg a rho =
  let (d, tau) = dom_of cfg a in
  let k = Term.Var.mk_slack None (Var.nonneg d) in
    (k, Jst.dep2 tau rho)

and dom_of cfg a =
  try
    let (x, rho) = uninterp cfg a in
      (try (Arith.dom_of x, rho) with Not_found -> (Dom.Real, Jst.dep0))
  with
      Not_found -> 
	((try Arith.dom_of a with Not_found -> Dom.Real), Jst.dep0)
  

(** Create fresh {i zero slack} variable *)
let mk_zero_slack a rho = 
  let k = Term.Var.mk_slack None Var.Zero in
    (k, rho)

(** Create fresh {i rename} variable *)
let mk_rename cfg a rho =
  let (d, tau) = dom_of cfg a in
  let u = Term.Var.mk_rename (Name.of_string "v") None (Var.Cnstrnt.mk_real(d)) in
    (u, Jst.dep2 rho tau)


(** {6 Iterators} *)

(** Apply [f] to all equalities in [s] and accumulate 
  results starting with [e]. *)
let fold f s e = 
  S.fold r f s 
    (S.fold t f s e)

let iter f s = 
  S.iter r f s;
  S.iter t f s


(** {6 Predicates} *)

(** [is_num s q x] holds if [x = q] in s. *)
let is_num s q x =
  try
    let (a, rho) = apply s x in
    let p = Arith.d_num a in
      if Mpa.Q.equal q p then Some(rho) else None
  with
      Not_found -> None


(** Return [(q, rho)] such that [rho |- x = q] or raise [Not_found]. *)
let d_num s x =
  let (a, rho) = apply s x in
  let p = Arith.d_num a in
    (p, rho)



(** {6 Basic Updates} *)

(** All update functions manipulate configurations [(p, s)]
  consistiting of a variable partitioning [p] and a solution set [s]. *)
type config = Partition.t * S.t


(** Protecting configurations against destructive
  updates. Also, disable propagation of newly
  generated facts on stacks. *)  
let protect (p, s) f a =
  let p' = Partition.copy p 
  and s' = S.copy s in       
    Fact.with_disabled_stacks 
      (f (p', s')) a


(** Return a variable for a term [a], possibly extending [S]
  with a fresh variable if [a] is not already a rhs.  Applies
  [replace s] in order to eliminate all dependent variables. *)
let name ((p, s) as cfg) a =
  let (b, rho) = replace s a in 
    try
      let (c, tau) = inv s b in
	(c, Jst.dep2 rho tau)
    with
	Not_found ->
	  if Arith.is_zero b then
	    let (k0, tau) = mk_zero_slack b rho in
	    let e = Fact.Equal.make (k0, b, tau) in
	      begin
		S.update t cfg e;
		(k0, tau)
	      end 
	  else if Arith.is_nonneg b = Three.Yes then
	    let (k, tau) = mk_nonneg_slack cfg b rho in
	    let e = Fact.Equal.make (k, b, tau) in
	      begin
		S.update t cfg e;
		(k, tau)
	      end 
	  else 
	    let (v, tau) = mk_rename cfg b rho in
	    let e = Fact.Equal.make (v, b, tau) in
	      S.update r cfg e;
	      (v, tau)
		  

(** Fusing a list of solved equalities into solution set for mode [m]. *)
let fuse1 tag cfg e =
  match tag with
    | S.Left -> S.fuse r cfg [e] 
    | S.Right -> S.fuse t cfg [e]
	

(** Composing a list of solved equalities into solution set for mode [m]. *)
let fuse_in_r = ref Term.Var.Map.empty

let rec compose1 tag cfg e =
  match tag with
    | S.Left ->
	S.compose r cfg [e] 
    | S.Right -> 
	lazy_fuse cfg e;
	S.compose t cfg [e]

and lazy_fuse cfg e =
  let (k, a, _) = Fact.Equal.destruct e in
    if Arith.is_num a || Term.is_var a then   (* eagerly fuse numeric constants and variables. *)
      S.fuse r cfg [e]
    else 
      fuse_in_r := Term.Var.Map.add k e !fuse_in_r

and complete_fuse_in_r ((_, s) as cfg) =  
  Term.Var.Map.iter
    (fun k e ->
       Trace.msg "la" "Lazy Fuse" e Fact.Equal.pp;
       S.fuse r cfg [e])
    !fuse_in_r

let initialize () = 
  fuse_in_r := Term.Var.Map.empty

let finalize c =
  complete_fuse_in_r c

let unwind_protect f c a =
  initialize ();
  try
    f c a;
    finalize c
  with
      exc -> 
	finalize c;
	raise exc
	

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
let rec process_equal cfg e =  
  unwind_protect 
    merge1 cfg e

and merge1 ((p, s) as cfg) e =
  let e = Fact.Equal.map (replace s) e in
    Trace.msg "la" "Process" e Fact.Equal.pp;
    process_solved_list cfg (solve e)

and process_solved_list cfg = 
  function
    | [] -> ()
    | e :: sl -> process_solved cfg e sl
      
(** Process an equality [e] by trying to solve for an unrestricted
  variable. In this case, the equation is composed to [r]. Otherwise,
  the [e] consists only of unrestricted variables. *)
and process_solved ((p, s) as cfg) e sl = 
  Trace.msg "la'" "Process_solved" e Fact.Equal.pp;
  let (x, a, _) = Fact.Equal.destruct e in
    if is_unrestricted_var x then
      begin
	compose_unrestricted cfg e;
	process_solved_list cfg sl
      end 
    else  
      try
	let y = choose_unrestricted a in
	let e' = isolate y e in
	let (y', _, _) = Fact.Equal.destruct e' in
	  assert(Term.eq y y');
	  let sl' = solved_fuse e' sl in
	    begin
	      if Term.Var.is_fresh Th.la y' then
		fuse1 r cfg e'
	      else 
		compose_unrestricted cfg e';
	      process_solved_list cfg sl'
	    end 
      with
	  Not_found -> 
	    assert(is_restricted a);
	    process_solved_restricted cfg e;
	    process_solved_list cfg sl


and process_solved_restricted ((p, s) as cfg) e =
  assert(is_restricted_equality e);
  if is_lhs_unbounded s e then
    compose_restricted cfg e
  else 
    try
      compose_restricted cfg 
	(try_isolate_rhs_unbounded s e)
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
		   if cmp < 0 then                 (* I. [|a'| < 0] *)
		     raise(Jst.Inconsistent (Jst.dep3 rho rho' tau))
		   else if cmp = 0 then            (* II. [|a'| = 0] *)
		     (try 
			compose_restricted cfg 
			  (isolate (choose a') e');
			let sigma = Jst.dep3 rho rho' tau in
			let e0 = Fact.Equal.make(k, Arith.mk_zero(), sigma) in
			  compose_restricted cfg e0
		    with 
			Not_found -> ())  (* skip *)
		   else                            (* III. [|a'| > 0] *)
		     if Arith.Monomials.Neg.is_empty a' then
		       raise(Jst.Inconsistent (Jst.dep3 rho rho' tau))
		     else 
		       (try
			  let y = choose_negvar_with_no_smaller_gain s a' in
			    pivot cfg y;
			    let sigma = Jst.dep3 rho rho' tau in
			      compose_restricted cfg 
				(Fact.Equal.make (k, Arith.mk_zero(), sigma))
			with
			    Not_found -> 
			      assert(not(Arith.Monomials.Neg.is_empty a'));
			      pivot cfg (choose_neg_least a');
			      process_solved_restricted cfg 
				(Fact.Equal.map_rhs (replace s) e'))
	       with           
		   Not_found ->  (* [k] is not a dependent variable *)
		     let sigma = Jst.dep2 rho tau in
		     let e0 = Fact.Equal.make (k, Arith.mk_zero(), sigma) in
		       compose_restricted cfg e0)

and compose_restricted cfg e =  
  Trace.msg "la'" "Compose(t)" e Fact.Equal.pp;
  compose1 t cfg e;
  if !Arith.integer_solve && Fact.Equal.is_diophantine e then
    gomory_cut cfg e
      
and compose_unrestricted cfg e =
  Trace.msg "la'" "Compose(r)" e Fact.Equal.pp;
  let (x, _, _) = Fact.Equal.destruct e in
    if Term.Var.is_fresh Th.la x then
      fuse1 r cfg e
    else 
      compose1 r cfg e

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

(** Let [a = ceil(a) - def(a) = floor(a) + frac(a)].  If 
        [x = b + Sigma_i ci*xi], 
  then this can be rewritten as 
        [x = ceil(b) - def(b) + Sigma_i floor(ci)*xi + Sigma_i frac(ci)*xi]
  which can be rearranged as
        [x - ceil(b) - Sigma_i floor(ci)*xi = -def(b) + Sigma_i frac(ci)*xi].
  If all the [x] and [xi] range over non-negative integers, then the lhs is
  integer and the rhs is a non-negative integer (since all the fracs are
  non-negative and [def(b) > -1]).  So, we have to add the inequality that
  [-def(b) + Sigma_i frac(ci)*xi >= 0], proceed. *)
and gomory_cut ((_, s) as cfg) e =
  assert(is_restricted_equality e);
  let (x, a, rho) = Fact.Equal.destruct e in (* [rho |- x = a]. *)
  let (b, ml) = Arith.destruct a in
  let b' = Mpa.Q.minus (Mpa.Q.def b) in
  let ml' = Arith.Monomials.mapq Mpa.Q.frac ml in
  let a' = Arith.mk_addq b' ml' in
  let rho' = Jst.dep1 rho in
  let nn' = Fact.Nonneg.make (a', rho') in
    Trace.msg "la'" "Gomory" nn' Fact.Nonneg.pp;
    process_nonneg1 cfg nn'
  
(** Process a nonnegativity constraint [nn] of the form [a >= 0]. *)
and process_nonneg cfg nn =  
  Trace.msg "la" "Process" nn Fact.Nonneg.pp;
  let process ((_, s) as cfg) nn = 
    let nn = Fact.Nonneg.map (replace s) nn in
      process_nonneg1 cfg nn
  in
    unwind_protect 
      process cfg nn

and process_nonneg1 ((_, s) as cfg) nn =
  let (a, rho) = Fact.Nonneg.destruct nn in          (* [rho |- a >= 0] *)
    match Arith.is_nonneg a with (* cheap test. *)
      | Three.Yes -> 
	  () 
      | Three.No -> 
	  raise(Jst.Inconsistent rho)
      | Three.X -> 
	  let (k, tau) = mk_nonneg_slack cfg a rho in (* [tau |- k = a] *)
	  let e = Fact.Equal.make (k, a, tau) in
	    try
	      let e' = isolate (choose_unrestricted a) e in
		compose_unrestricted cfg e'
	    with
		Not_found ->
		  (try
		     compose_restricted cfg 
		       (try_isolate_rhs_unbounded s e);
		   with
		       Not_found -> 
			 add_to_t cfg e;
			 infer cfg)
	      
and add_to_t ((_, s) as cfg) e =
  let (k, a, rho) = Fact.Equal.destruct e in
    assert(is_restricted_var k && is_restricted a);
    Trace.msg "la'" "Add_to_t" e Fact.Equal.pp;
    let c = Arith.constant_of a in
      if Q.is_nonneg (Arith.constant_of a) then
	compose_restricted cfg e
      else if  Arith.Monomials.Pos.is_empty a then
	raise(Jst.Inconsistent rho)
      else
	try
	  let e' = try_isolate_rhs_unbounded s e in         (* k = a == y = b *)   
	  let (y, b, _) = Fact.Equal.destruct e' in
	    if Term.Var.is_zero_slack k then
	      let b' = replace_zero_slacks b in
	      let e'' = Fact.Equal.make (y, b', rho) in     (* [rho |-y=b[k:=0]] *) 
		compose_restricted cfg e''
	    else 
		compose_restricted cfg e'
	with
	    Not_found ->  
	      assert(not(Arith.Monomials.Pos.is_empty a));
	      let y = choose_pos_least a in
		pivot cfg y;
		let (a', rho') = replace s a in              (* [rho' |- a = a'] *)
		  assert(not(Term.subterm y a'));
		  let rho' = Jst.dep2 rho rho' in            (* [rho' |- k = a'] *)
		    add_to_t cfg (Fact.Equal.make (k, a', rho')) 
		    


(** [x = a == y = a'] with [y] unbounded in [s]; 
  otherwise [Not_found] is raised. *)
and try_isolate_rhs_unbounded s e =
  let is_unb (_, x) =  is_unbounded s x in
  let a = Fact.Equal.rhs_of e in
    if Mpa.Q.is_nonpos (Arith.constant_of a) then
      let (_, y) = Arith.Monomials.Pos.choose is_unb a in
	isolate y e
    else 
      raise Not_found

(** [e] is of the form [k = a] with [k] unbounded and [|a|]
  positive and [a-] is empty. *)
and is_lhs_unbounded s e =
  try
    let (x, a, _) = Fact.Equal.destruct e in
      is_unbounded s x
      && Mpa.Q.is_nonneg (Arith.constant_of a)
      && Arith.Monomials.Neg.is_empty a    
  with
      Not_found -> failwith "is_lhs_unbounded raised Not_found"
      

and pivot ((_, s) as cfg) y =
  assert(not(is_unbounded s y));
  try
    let (_, e) = gain s y in
    let e' = isolate y e in                
      Trace.msg "la'" "Pivot" (e, e') (Pretty.infix Fact.Equal.pp " ==> " Fact.Equal.pp);
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
    zeros     (* zeros need to be sorted starting with least one. *)


and maximize1 ((_, s) as cfg) zeros (k, a, rho) =   (* [rho |- k = a] *)
  Trace.msg "la'" "Maximize" (k, a) Term.Equal.pp;
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
      pivot cfg y;             (** following loops sometimes :-( *)
      let (b, tau) = S.find t s k in
	maximize1 cfg zeros (k, b, tau)


and set_to_zero cfg (x, a, rho) =
  assert(Arith.Monomials.Pos.is_empty a);
  Arith.Monomials.Neg.iter
    (fun (_, y) ->
       let tau = Jst.dep1 rho in             (* [tau |- y = 0] *)
       let e = Fact.Equal.make (y, Arith.mk_zero(), tau) in
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


(** {6 Inequality Tests} *)

(** Test if [a < 0] by asserting [a >= 0]. If this fails, then [a < 0] holds. *)
let rec is_neg cfg a =
  if not(Term.is_pure Th.la a) then None else
    try
      let nna = Fact.Nonneg.make (a, Jst.dep0) in
	protect cfg process_nonneg nna;
	None
    with
	Jst.Inconsistent(rho) -> Some(rho)

(** [a <> b] if [solve(S[a] = S[b])] is inconsistent. *)
and is_diseq ((_, s) as cfg) a b =
  if is_empty s || not(Term.is_pure Th.la a) || not(Term.is_pure Th.la b) then
    None
  else 
    let (a', rho) = replace s a
    and (b', tau) = replace s b in
      try
	let _ = Arith.solve (a', b') in
	  None
      with
	  Exc.Inconsistent -> Some(Jst.dep2 rho tau)
    
(** [a > 0] iff [-a < 0]. *)
and is_pos cfg a =  
  assert(Term.is_pure Th.la a);
  is_neg cfg (Arith.mk_neg a)
	
  
(** [a >= 0] iff [a > 0] or [a = 0]. *)
and is_nonneg cfg a = 
  assert(Term.is_pure Th.la a);
  match is_pos cfg a with
    | None -> is_zero cfg a
    | res -> res


(** [a <= 0] iff [a < 0] or [a = 0]. *)
and is_nonpos cfg a =
  assert(Term.is_pure Th.la a);
  match is_neg cfg a with
    | None -> is_zero cfg a
    | res -> res


(** [a = 0] in iff [a == 0] or [x = 0] in [s] with [x = a]. *)
and is_zero (p, s) a =
  if Arith.is_zero a then
    Some(Jst.dep0)
  else if Term.is_var a then
    try
      let (x, rho) = inv s (Arith.mk_zero()) in
	(match Partition.is_equal p x a with
	  | Some(tau) -> Some(Jst.dep2 rho tau)
	  | None -> None)
    with
	Not_found -> None
  else 
    None


(** Test if [a <= b]. *)
and is_le cfg (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg cfg (Arith.mk_sub b a)


(** Test if [a >= b]. *)
and is_ge cfg (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg cfg (Arith.mk_sub a b)


(** Test if [a > b]. *)
and is_gt cfg (a, b) = 
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_pos cfg (Arith.mk_sub a b)


(** {6 Processing Disequalities} *)

(** Processing disequalities only deals with integer disequalities,
  which are maintained in the form [e <> n] where [n] is a natural 
  number. *) 
and process_diseq cfg d = 
  Trace.msg "la" "Process" d Fact.Diseq.pp;
  unwind_protect
    dismerge1 cfg d

and dismerge1 ((p, s) as cfg) d =
  let d = Fact.Diseq.map (replace s) d in    (* replace dependent variables. *)
    match is_inconsistent_diseq cfg d with
      | Some(rho) -> 
	  raise(Jst.Inconsistent(rho))
      | None -> 
	  if !Arith.integer_solve && Fact.Diseq.is_diophantine d then
	    process_diophantine_diseq cfg d
	  else 
	    process_nondiophantine_diseq cfg d 

(** A disequality [a <> b] with [a], [b] not containing any dependent
  variables is inconsistent if [solve(a = b)] yields an inconsistency. *)
and is_inconsistent_diseq cfg d =
  let (a, b, rho) = Fact.Diseq.destruct d in
    try
      if Arith.solve (a, b) = [] then 
	Some(rho)
      else 
	None
    with
	Exc.Inconsistent -> None
 

(** Nondiophantine disequalities are variable-abstracted
  and added to the disequalities in the variable partitioning. *)
and process_nondiophantine_diseq ((p, s) as cfg) d =
    let d' = Fact.Diseq.map (name cfg) d in
      assert(Fact.Diseq.is_var d');
      Partition.dismerge p d';                     (* now, add -x <> 0 for x <> 0 *)
      let (x, y, rho) = Fact.Diseq.destruct d' in  (* [rho |- x <> y] *)
	match is_zero cfg x with
	  | Some(tau) ->                           (* [tau |- x = 0] *)
	      ()
	  | None -> 
	      ()   (* to do *)
	
(** Whenever we add [e<>n], we calculate the largest contiguous segment [N] 
  containing [n] such that [e<>m] for [m] in [N], then with 
  [l = min(N)], and [h = max(N)], 
  separately assert [e <= l-1] and [e>=h+1].  If both calls yield contradictions,
  then we report a contradiction.  If only one of the calls
  yields a contradiction, then we press on with the other one.
  (Any equalities generated from the inequality are propagated
  to the variable equality and disequality parts).  
  If both calls are free of contradiction, then we revert to
  the original state and continue processing inputs. *)
and process_diophantine_diseq cfg d =
  assert(Fact.Diseq.is_diophantine d);
  let (e, n, rho) = Fact.Diseq.d_diophantine d in
    try
      let m = Arith.d_num e in
	if Q.equal n m then
	  raise(Jst.Inconsistent(rho))
	else   
	  ()   (* skip *)
    with
	Not_found -> 
	  let (min, max, taus) =                   (* [taus] together prove *)
	    contiguous_diseq_segment cfg (e, n)    (* [e] not in interval [[min..max]]. *)
	  in
	  let l = Arith.mk_num (Q.sub min Q.one)
	  and h = Arith.mk_num (Q.add max Q.one) in
	    try
	      case_process_le cfg (e, l);           (* try [e <= l] *)
	      (try
		 case_process_ge cfg (e, h);        (* try [e >= h] *)
		 process_nondiophantine_diseq cfg d
	       with
		   Jst.Inconsistent(tau) -> (* [tau |- e < h] *)
		     let sigma = Jst.dep (tau :: taus) in
		       process_le cfg (e, l, sigma))  (* [sigma |- e <= l] *)
	    with
		Jst.Inconsistent(tau) ->   (* [tau |- e > l] *)
		  let sigma = Jst.dep (tau :: taus) in
		    process_ge cfg (e, h, sigma)   (* [sigma |- e >= h] *)

and process_le cfg (a, b, rho) =
  let nn = Fact.Nonneg.make (Arith.mk_sub b a, rho) in
    process_nonneg cfg nn
      
and process_ge cfg (a, b, rho) =
  let nn = Fact.Nonneg.make (Arith.mk_sub a b, rho) in
    process_nonneg cfg nn

and case_process_le cfg =
  Trace.proc "la'" "Case(le)" (Pretty.pair Term.pp Term.pp)
    (fun (a, b) ->
       let diff = Arith.mk_sub b a in
	 try
	   let nn = Fact.Nonneg.make (diff, Jst.dep0) in
	     protect cfg process_nonneg nn
	 with
	     Jst.Inconsistent(rho) ->
		 raise(Jst.Inconsistent(rho)))
      
and case_process_ge cfg = 
  Trace.proc "la'" "Case(ge)" (Pretty.pair Term.pp Term.pp)
    (fun (a, b) ->
       let diff = Arith.mk_sub a b in
	 try
	   let nn = Fact.Nonneg.make (diff, Jst.dep0) in
	     protect cfg process_nonneg nn
	 with
	     Jst.Inconsistent(rho) ->
		 raise(Jst.Inconsistent(rho)))
  
and contiguous_diseq_segment ((p, s) as cfg) (e, n) =
  Trace.call "la'" "Contiguous" (e, n) (Pretty.pair Term.pp Mpa.Q.pp);
  let taus = ref [] in
  let rec upper max =                           (* [rho |- e <> n] *)
    let max' = Q.add max Q.one in
      match is_num_diseq cfg e max' with 
	| Some(tau) ->                       (* [tau |- e <> max'] *)
	    taus := tau :: !taus;
	    upper max'             (* only succeeds finitely often *)   
	| None -> 
            max
  in
  let rec lower min =
    let min' = Q.sub min Q.one in
      match is_num_diseq cfg e min' with
	| Some(tau) -> 
	    taus := tau :: !taus;
	    lower min'
	| None ->
	    min
  in
  let (min, max) = (lower n, upper n) in
    Trace.exit "la'" "Contiguous" (min, max) (Pretty.pair Mpa.Q.pp Mpa.Q.pp);
    (min, max, !taus)

    
(** Test if [a] and [q] are known to be disequal, where all variables in [a]
  are independent. This is the case if either
  - [a] is a rational disequal from [q]
  - [x = q] is in [s] and [y] is [uninterp a] and [x <> y] in [p]. *) 
and is_num_diseq ((p, s) as cfg) a q =
  try
    let p = Arith.d_num a in
      if Mpa.Q.equal q p then None else Some(Jst.dep0)
  with
      Not_found -> 
	(try
	   let (x, rho) = inv s (Arith.mk_num q)
	   and (y, tau) = uninterp cfg a in
	     (match Partition.is_diseq p x y with
		| Some(sigma) -> Some(Jst.dep3 rho tau sigma)
		| None -> None)
	 with
	     Not_found -> None)

  
(** {6 Extremal Values} *)    

(** Maximize [a] in [s] by systematically eliminating positive 
  monomials in the canonical form [a'] of [a].  It returns either 
  - [(b, rho')] such that [b+] is empty and [rho' |- a = b], or
  - raises [Unbounded] if [a] is unbounded. *)
let rec upper ((p, s) as cfg) a =
  let rec max_term a (b, rho) =                (* [rho |- b = a] *)
    if Term.is_var a then
      max_var a (b, rho)
    else 
      try
	let (_, x) = Arith.Monomials.Pos.least a in  (* choose least [x] in [a+]. *)
	let (_, e) = gain s x in          (* [y = ... + q*x + ...] with [q < 0].  *)
	let e' = isolate x e in           (* Isolate [x] in [e] and substitute *)
	let (a', rho') = apply1 e' a in   (* [rho' |- a = a'] *)
	let tau = Jst.dep2 rho rho' in
	  max_term a' (b, tau)            (* [tau |- b = a'] *)
      with
	  Not_found -> 
	    assert(Arith.Monomials.Pos.is_empty a);
	    (a, rho)
  and max_var x (b, rho) =             (* [rho |- x = b] *)
    assert(is_restricted_var x);
    make_dependent cfg x;              (* make [x] a dependent variable. *)
    try
      let (a, rho') = S.apply t s x in (* [rho' |- x = a] *)
      let tau = Jst.dep2 rho rho' in
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


(** Update [s] such that [x] is a dependent variable whenever this variable 
  occurs in [s] *)
and make_dependent ((_, s) as cfg) x =
 (*  assert(is_restricted_var x); *)
  let is_true _ = true in
  try
    let e = S.Dep.choose t s is_true x in (* choose [y = a] such that [x] occurs in [a] *)
      compose1 t cfg (isolate x e)        (* and solve [y = a] for [x]. *)
  with
      Not_found -> ()
	

(** Either return a term of the form [q - a-] or raise [Unbounded]. *)
let lower cfg a = 
  let (b, rho) = upper cfg (Arith.mk_neg a) in
    (Arith.mk_neg b, rho)


(** Returns an upper bound or raises [Unbounded]. *)
let sup cfg a =
  let (b, rho) = upper cfg a in
    assert(Arith.Monomials.Pos.is_empty b);
    (Arith.constant_of b, rho)


(** Returns a lower bound or raises [Unbounded]. *)
let inf cfg a =
  let (b, rho) = lower cfg a in
    assert(Arith.Monomials.Neg.is_empty b);
    (Arith.constant_of b, rho) 


(** {6 Finite Domains} *)

module Finite = struct

  module Zset = Set.Make(
    struct
      type t = Mpa.Z.t
      let compare = Mpa.Z.compare
    end)

    (** A finite integer domain consists of nonstrict lower [lo] and upper [hi]
      bounds and a disequality set [ds] with integer values strictly between
      these bounds. The interpretation [D(lo, hi, ds)] is given
      by [{i in int | lo <= i <= hi, not(i in ds)}] *)
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
    try
      (if not(Term.Var.is_int x) then 
	 raise Unbounded
       else 
	 let (hi, _) = sup cfg x
	 and (lo, _) = inf cfg x in
	 let ds = integer_diseqs_between cfg x (lo, hi) in
	   make (Mpa.Q.floor lo, Mpa.Q.floor (Mpa.Q.add lo Mpa.Q.one), ds))
    with
	Unbounded -> raise Not_found
	
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

  
  exception Found of Term.t * t

  (** Find a finite interpretation for one of the variables [x] in [a]. *)
  let of_term cfg a =
    try
      (Term.iter
	 (fun x ->
	    try
	      let fin = of_var cfg x in raise(Found(x, fin))
	    with 
		Not_found -> ())
	 a);
      raise Not_found
    with
	Found(x, fin) -> (x, fin)

  let split ((_, s) as cfg) =
    let of_equal (x, a) = 
      try
	let fin = of_var cfg x in
	  raise(Found((x, fin)))
      with
	  Not_found -> 
	    (try
	       let (x, fin) = of_term cfg a in
		 raise(Found(x, fin))
	     with
		 Not_found -> ())
    in
      try                    
	iter (fun x (a, _) -> of_equal (x, a)) s;
	raise Not_found
      with
	  Found(x, fin) -> (x, fin)
end 

type mode = Max | Min


(** {6 Model Generation} *)

(** Given a pair [(s, d)] with [s] a tableau and [d] variable disequalities
  - Start with the zero assignment to all independent in [s] to obtain a
  ground substitution [s0], also replace all [s]-dependent variables in [d]
  to obtain [(s0, s[d])].
  - Take the disequalities and separately solve them as equalities with an
  ordering [<<] where [unrestricted << unbounded << slacks << integer slacks]
  to obtain [(s0, d')] with [d'] equals [solve(s)(s[d])]. Now all the disequalities 
  in [d'] are of the form [x <> e].  
  - Start with the highest (w.r.t. [<<]] variable disequality that is falsified.
  That is, find highest [x] such that [s0(x) = s[e]] for [x <> e] in [d'].
  - If [x] is a slack variable, maximize [x] in [s] to obtain [m' - sl'] with
  [sl'] a positive combination of slack variables. Plug in values for slack
  variables to obtain [m' - sl' = m' - s0[sl'] = m - sl]. Thus, [m] is the
  maximum value of [x] (which we know is not [0]). 
  Also, find all the points [v0], [v1],..., such that [x<>v0],[ x<>v1],....
  - If [x] is an integer variable, pick the minimum integer below [m] that is
    different from [v0], [v1],.... 
  - If [x] is real, pick the minimum [n] of [m], [v0], [v1]..., and let the 
     new value of [x] be [(x + n)/2].  
  Now plug this value into the other disequalities and continue.  The idea behind the 
  ordering is that the scarce resources (integers, slacks) are given priority in 
  choice of values. *)



let rec model ((_, s) as cfg) xl =
  failwith "to do"

(*
  try
    Format.eprintf "\nWarning: model generation is work in progress...@?";
    let s = S.copy s in             (* protect state against updates. *)
    let m = ref Term.Map.empty in
      List.iter
	(fun (x, mode) -> 
	   let a = interpretation_of cfg (x, mode) in
	     m := Term.Map.add x a !m;
	     let e = Fact.Equal.make (x, a, Jst.dep0) in 
	       merge cfg e)
	xl;
      !m
  with
      Jst.Inconsistent _ ->
	failwith "Inconsistency detected in model generation"

and interpretation_of ((p, _) as cfg) (x, mode) =
  let q = interpretation cfg (x, mode) in
      q                                        (* disequalities: to do *)

and interpretation cfg (x, mode) =
  if is_restricted x then
    interp_of_restricted cfg (x, mode)
  else 
    interp_of_unrestricted cfg (x, mode)
      
and interp_of_restricted ((_, s) as cfg) (k, mode) =
  make_dependent cfg k;           (* make [k] a dependent variable. *)
  let (b, _) = S.find t s k in
    match mode with
      | None -> 
	  Arith.mk_num (Arith.constant_of b)
      | Some(Max) -> 
	  (try
	     let (b', _) = upper cfg b in
	       Arith.mk_num (Arith.constant_of b')
	   with
	       Not_found -> Arith.mk_posinf())
      | Some(Min) -> 
	  (try
	     let (b', _) =  lower cfg b in
	       Arith.mk_num (Arith.constant_of b')
	   with
	       Not_found -> Arith.mk_neginf())

and interp_of_unrestricted ((_, s) as cfg) (x, mode) = 
  let is_restricted_monomial (_, y) = is_restricted_var y in
    make_dependent cfg x;              (* make [x] a dependent variable. *)
    let (b, _) = S.find r s x in       (* [x = b] *)
    let (n, w, u) = Arith.Monomials.partition is_restricted_monomial b in
      match mode with
	| None -> 
	    Arith.mk_num (Q.add n (Arith.constant_of w))
	| Some(Max) ->
	    (try 
	       let (w', _) = upper cfg w in
		 Arith.mk_num (Q.add n (Arith.constant_of w'))
	     with
		 Not_found -> Arith.mk_posinf())
      | Some(Min) -> 
	  (try
	     let (w', _) = lower cfg w in
	     Arith.mk_num (Q.add n (Arith.constant_of w'))
	   with
	       Not_found -> Arith.mk_neginf())
*)
