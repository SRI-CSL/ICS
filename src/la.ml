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

(** Inference system for linear arithmetic. *)

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
  - The {i dependency} index of {!Solution.SET}
  - The {i zero} index records all [k]s in the domain of [t] 
    with [|a|] equals zero.
  - The {i negdep} index records for [y] all [x] in [t] such that 
    [x = a] in [t] and [y] is in [a-]; that is [y] occurs with negative
    coefficient. This index is currently derived from the {i dependency}
    index.

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
let rec solve e =
  let (a, b, rho) = Fact.Equal.destruct e in
    try
      let sl = Arith.solve (a, b) in
      let sl' = solved_normalize sl in
      let inj (a, b)= Fact.Equal.make (a, b, rho) in
	List.map inj sl'
    with
	Exc.Inconsistent -> raise(Jst.Inconsistent(rho))

(** Construct a list of equalities from a solved form. It is
  ok to reverse the order.  Variable equalities such as
  [x = la!1] are eliminated, since [Fact.Equal.make] might flip
  an equality such as [k!2 = la!1] and the constraint on slack
  would be lost. *)
and solved_normalize sl =
  let rec loop pre = function
    | [] -> pre
    | ((x, a) as e) :: post -> 
	if Term.Var.is_fresh Th.la a then
	  let pre' = Term.Subst.fuse Arith.apply (a, x) pre
	  and post' = Term.Subst.fuse Arith.apply (a, x) post in
	    loop pre' post'
	else        
	  loop (e :: pre) post             
  in
    loop [] sl
	  

let solve =
  Trace.func "la'" "Solve" Fact.Equal.pp (Pretty.list Fact.Equal.pp) solve


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
let apply1 e = 
  let (x, a, rho) = Fact.Equal.destruct e in
  let lookup y = 
    if Term.eq x y then 
      (a, rho) 
    else 
      Jst.Eqtrans.id y 
  in
    Jst.Eqtrans.replace Arith.map lookup

(** Transform the equation [e] of the form [a = b] to the equivalent [x = c].  
  Assumes that [x] is a subterm of [a - b]. *)
let isolate x e =
  let (a, b, rho) = Fact.Equal.destruct e in
  try
    let (y, c) = Arith.isolate x (a, b) in
      assert(Term.eq x y);
      Fact.Equal.make (x, c, rho)
  with 
      Not_found -> failwith "Fatal Error: arithmetic isolation failed"


(** Check if all variables are interpreted over the integers. *)
let is_diophantine e =
  Fact.Equal.both_sides Arith.is_diophantine e

(** Destruct a disequality [a <> b] over integer terms as [e <> n] with
  [n] an integer. *)
let d_diophantine d =
  assert(Fact.Diseq.both_sides Arith.is_diophantine d);
  let (a, b, rho) = Fact.Diseq.destruct d in
  let (e, n) = 
    let lcm =                     (* least common multiple of denominators *)
      Mpa.Q.abs                   (* of the coefficients in [a] and [b]. *)
	(Mpa.Q.of_z
           (Mpa.Z.lcm
              (Arith.lcm_of_denominators a)
              (Arith.lcm_of_denominators b)))
    in
    let (a', b') =                (* all coefficients are integer now. *)
      if Mpa.Q.is_one lcm then (a, b) else
	(Arith.mk_multq lcm a, Arith.mk_multq lcm b)
    in
    let (q', c') = Arith.destruct (Arith.mk_sub a' b') in (* [q' + c' <> 0] *)
      (c', Mpa.Q.minus q')                               (* ==> [c' <> -q'] *)
  in
    (e, n, rho)

let d_diophantine = 
  Trace.func "foo7" "d_diophantine" 
    Fact.Diseq.pp
    (Pretty.triple Term.pp Mpa.Q.pp Jst.pp)
    d_diophantine


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


(** [occurs_negatively x e] holds if [e] is of the 
  form [y = a] and [x] is in [a-]. *)
let occurs_negatively x e  = 
  let a = Fact.Equal.rhs_of e in
    try Q.is_neg (Arith.coefficient_of x a) with Not_found -> false


(** Specification for an index for restricted variables [k]
  such that [x = a] with [|a| = 0]. *)
module ZeroIdx: (Solution.EXT with type t = Term.Var.Set.t) = struct
  type t = Term.Var.Set.t
  let pp fmt zero = 
    Pretty.set Term.pp fmt (Term.Var.Set.elements zero)
  let empty = Term.Var.Set.empty
  let update zero e =
    let (x, a, _) = Fact.Equal.destruct e in
      if is_restricted_var x && Mpa.Q.is_zero (Arith.constant_of a) then
	Term.Var.Set.add x zero
      else 
	zero
  let restrict zero e = 
    let (x, a, _) = Fact.Equal.destruct e in
      if is_restricted_var x then
	Term.Var.Set.remove x zero
      else 
	zero
end


(** Specifification of the theory of linear arithmetic as a canonizable theory. *)
module LA: Solution.TH = struct 
  let th = Th.la
  let map = Arith.map
end


(** Solution set for linear arithmetic *)
module S: Solution.SET with type ext = Term.Var.Set.t = 
  Solution.Make(LA)(ZeroIdx)


type tag = R | T

(** Partition a solution set into the 
  - {i regular} equations [x = a] with [x] unrestricted and the
  - {i tableau} equations [k = a] with [k] and [a] restricted. *)
let partition s = 
  S.fold
    (fun e (r, t) ->
       let x = Fact.Equal.lhs_of e in
	 if is_unrestricted_var x then
	   (e :: r, t)
	 else 
	   (r, e :: t))
    s ([], [])

let index = ref false

(** Print a partitioned version of [s]. *)
let pp tag fmt s =
  let (r, t) = partition s in
    match tag with
      | R -> 
	  Pretty.set Fact.Equal.pp fmt r;
      | T ->
	  Pretty.set Fact.Equal.pp fmt t;
	  if !index then
	    begin
	      Pretty.string fmt "\nZero Index: ";
	      Pretty.set Term.pp fmt (Term.Var.Set.elements (S.ext s));
	    end 


let apply = S.apply 
let find s = Jst.Eqtrans.totalize (S.apply s)
let inv s = Jst.Eqtrans.totalize (S.inv s)
let dep = S.dep


(** Return domain of interpretation for [a]. *)
let dom s a = 
  try
    let (b, rho) = inv s a in
      (Arith.dom_of b, rho)
  with
      Not_found -> (Arith.dom_of a, Jst.dep0)


(** Create fresh {i nonnegative slack} variable *)
let rec mk_nonneg_slack s a rho =
  let (d, tau) = dom s a in
  let k = Term.Var.mk_slack None (Var.nonneg d) in
    (k, Jst.dep2 tau rho)


(** Create fresh {i zero slack} variable *)
let mk_zero_slack a rho = 
  let k = Term.Var.mk_slack None Var.Zero in
    (k, rho)


(** Create fresh {i rename} variable *)
let mk_rename s a rho =
  let v = Name.of_string "v" in
  let (d, tau) = dom s a in
  let c = Var.Cnstrnt.mk_real d in
  let u = Term.Var.mk_rename v None c in
    (u, Jst.dep2 rho tau)


(** View solution set [S] as composed of two different 
  solution sets [R; T] with
  - [R] consisting of equalities [x = a] with [x] unrestricted
  - [T] consisting of equlities [k = a] with [k] restricted and all
  variables in [a] are restricted, too. *)
module Dep = struct

  let fold tag s f =
    let f' e acc =
      let (x, _, _) = Fact.Equal.destruct e in
	match tag with
	  | R -> if is_unrestricted_var x then f e acc else acc
	  | T -> if is_restricted_var x then f e acc else acc
    in
      S.Dep.fold s f'

  let choose tag s p =
    let p' e =
      let (x, _, _) = Fact.Equal.destruct e in
	match tag with
	  | R -> if is_unrestricted_var x then p e else false
	  | T -> if is_restricted_var x then p e else false
    in
      S.Dep.choose s p'

  let for_all tag s p =
    let p' e =
      let (x, _, _) = Fact.Equal.destruct e in
	match tag with
	  | R -> if is_unrestricted_var x then p e else true
	  | T -> if is_restricted_var x then p e else true
    in
      S.Dep.for_all s p'

  let iter tag s f =
    let f' e =
      let (x, _, _) = Fact.Equal.destruct e in
	match tag with
	  | R -> if is_unrestricted_var x then f e
	  | T -> if is_restricted_var x then f e
    in
      S.Dep.iter s f'

end 


(** [Zero] index. *)
module Zero = struct

  (** [get t] returns the set of [x] with [x = a] in [t] such that
    the constant monomial of [a] is [0]. *)
  let get s = S.ext s
 
  (** Apply [f e] to all equalities [k = a] in [t] such that [|a| = 0]. *)
  let iter s f =
    let apply_to k =
      let e = 
	try 
	  S.equality s k 
	with 
	    Not_found -> invalid_arg "Fatal Error in Zero iteration"
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
    Dep.fold T s
      (fun e acc ->
	 if occurs_negatively x e then
	   let y = Fact.Equal.lhs_of e in
	     Term.Set.add y acc
	 else 
	   acc)
      x Term.Set.empty

 
  (** Choose an arbitrary value in the [negdep] index. *)
  let choose s x =
    Dep.choose T s (occurs_negatively x)


  (** Check if [negdep] is empty for [x]. *)
  let is_empty s x = 
    Term.Set.is_empty (get s x)

  
  (** Succedds if [f e] succeeds for all equalities [k = a] such  
    that [y] occurs negatively in [a]. *)
  let forall s f y =
    let f' e =  if occurs_negatively y e then f e else true in
      Dep.for_all T s f' y
     

  (** Apply [f] to all equalities [k = a] such that [y] occurs
    negatively in [a]. *)
  let iter s f y =
    let f' e = 
      if occurs_negatively y e then f e 
    in
      Dep.iter T s f' y
  
end 


  	 
(** [x] is {i unbounded} in [s] if [x] does not occur positively in [t], 
  that is, every coeffient of independent [x] is positive. *)
let is_unbounded s = 
  Negdep.is_empty s


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
 

(** Replace dependent variables in [s] with corresponding
  right hand sides (assumed to be canonical). This function
  should only be used in outer loops! *)
let can (p, s) a =
  let hyps = ref Jst.dep0 in
  let lookup y = 
    try
      let (b, rho) = S.apply s y in
	  hyps := Jst.dep2 rho !hyps; b
    with
	Not_found -> 
	  let (x, rho) = Partition.find p y in
	    hyps := Jst.dep2 rho !hyps; x
  in
  let b = Arith.map lookup a in
    (b, !hyps)

let can (p, s) =
  Jst.Eqtrans.trace "la'" "Can" (can (p, s))

let is_canonical (p, s) a =
  let (b, _) = can (p, s) a in
    Term.eq a b

let name (p, s) a =
  if Term.is_var a then ((a, Jst.dep0), (p, s)) else
  let (b, rho) = can (p, s) a in 
    try
      let (x, tau) = S.inv s b in
	((x, Jst.dep2 rho tau), (p, s))
    with
	Not_found ->
	  let (v, tau) = 
	    if Arith.is_zero b then
	      mk_zero_slack b rho
	    else if Arith.is_nonneg b = Three.Yes then
	      mk_nonneg_slack s b rho 
	    else 
	      mk_rename s b rho
	  in
	  let e = Fact.Equal.make (v, a, Jst.dep2 rho tau) in
	  let (p, s) = S.update (p, s) e in
	    ((v, tau), (p, s))



let is_equal (p, s) a b =
  let (a', rho') = can (p, s) a 
  and (b', tau') = can (p, s) b in
    if Term.eq a' b' then
      Some(Jst.dep2 rho' tau')
    else 
      None


(** Test if [a = 0] *)
let is_zero (p, s) a =
  is_equal (p, s) a (Arith.mk_zero())


(** Test if disequality [d] is unsatisfiable. *)
let is_unsat (p, s) d = 
  assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
  if S.is_empty s then None else  
    let (a, b, rho) = Fact.Diseq.destruct d in
      is_equal (p, s) a b


(** Check if all variables in [s] are canonical in [p].  *)
let is_canonical (s, p)	=
  S.for_all
    (fun e -> 
       let (x, a, _) = Fact.Equal.destruct e in
	 Partition.is_canonical p x &&
	 Arith.for_all (Partition.is_canonical p) a)
    s


(** {6 Basic Updates} *)

(** All update functions manipulate configurations [(p, s)]
  consistiting of a variable partitioning [p] and a solution set [s]. *)
type config = Partition.t * S.t


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
let rec process_equal (p, s) e =  
  Trace.msg "la'" "Process" e Fact.Equal.pp;
  let e = Fact.Equal.map (can (p, s)) e in   (* canonize. *)
    compose_solved_list (p, s) (solve e)

and merge_equal (p, s) e =
  Trace.msg "la'" "Merge" e Fact.Equal.pp;
  compose_solved_list (p, s) (solve e)


(** A solved form is composed one-by-one, since composing might
  involve resolving an equality in case the equality is restricted. *)
and compose_solved_list (p, s) sl = 
  match sl with
    | [] -> (p, s)
    | e :: sl' -> compose_solved (p, s) e sl'
   
   
(** Process an equality [e] by trying to solve for an unrestricted
  variable. In this case, the equation is composed to [r]. Otherwise,
  the [e] consists only of unrestricted variables. *)
and compose_solved (p, s) e sl = 
  let (x, a, _) = Fact.Equal.destruct e in
    if is_unrestricted_var x then
      let (p, s) = compose (p, s) e in
	compose_solved_list (p, s) sl
    else  
      try
	let y = choose_unrestricted a in
	let e' = isolate y e in
	  assert(Term.eq y (Fact.Equal.lhs_of e'));
	  let sl' = solved_fuse e' sl in    (* propagate the fliped equality into *)
	  let (p, s) = compose (p, s) e' in (* solved list [sl] to be processed. *)
	    compose_solved_list (p, s) sl'
      with
	  Not_found -> 
	    begin
	      assert(is_restricted a);
	      let (p, s) = compose_solved_restricted (p, s) e in
		Trace.msg "foo8" "Restricted" e Fact.Equal.pp;
		compose_solved_list (p, s) sl
	    end 


and compose_solved_restricted (p, s) e = 
  Trace.msg "la'" "Compose_solved_restricted" e Fact.Equal.pp;
  assert(is_restricted_equality e);
  if is_lhs_unbounded s e then
    compose (p, s) e
  else 
    try
      compose (p, s)
	(try_isolate_rhs_unbounded s e)
    with
	Not_found -> 
	  let (diff, rho) = mk_diff e in                   (* [rho |- diff = 0] *)
	    assert(Mpa.Q.is_nonpos (Arith.constant_of diff));
	    let (k, tau) = mk_zero_slack diff rho in       (* [tau |- k = diff] *)
	    let (p, s) = add_to_t (p, s) (Fact.Equal.make (k, diff, tau)) in   
	    let (p, s) = infer (p, s) in
	    let (p, s) = eliminate_zero_slack (p, s) (k, Jst.dep2 rho tau) in
	      (p, s)


and eliminate_zero_slack (p, s) (k, rho) =     (* [rho |- k = 0] *)
  try
    let (a', rho') = S.apply s k in           (* [rho' |- k = a'] *)
    let e' = Fact.Equal.make (k, a', rho') in
    let cmp = Mpa.Q.compare (Arith.constant_of a') Mpa.Q.zero in    (* *** *)
      if cmp < 0 then                 (* I. [|a'| < 0] *)
	raise(Jst.Inconsistent (Jst.dep2 rho rho'))
      else if cmp = 0 then            (* II. [|a'| = 0] *)
	 (try 
	    let (p, s) = 
	      compose (p, s)
		(isolate (choose a') e')
	    in
	    let sigma = Jst.dep2 rho rho' in
	    let e0 = Fact.Equal.make(k, Arith.mk_zero(), sigma) in
	      compose (p, s) e0
	  with 
	      Not_found -> (p, s))     (* skip *)
      else                             (* III. [|a'| > 0] *)
	let rec repeat_case_iii (p, s) e' =
	  let (_, a', rho') = Fact.Equal.destruct e' in
	    if Arith.Monomials.Neg.is_empty a' then
	      raise(Jst.Inconsistent (Jst.dep2 rho rho'))
	    else 
	      (try
		 let y = choose_negvar_with_no_smaller_gain s a' in
		 let (p, s) = pivot (p, s) y in
		 let sigma = Jst.dep2 rho rho' in
		 let e'' = Fact.Equal.make (k, Arith.mk_zero(), sigma) in
		   compose (p, s) e''
	       with
		   Not_found -> 
		     assert(not(Arith.Monomials.Neg.is_empty a'));
		     let (p, s) = pivot (p, s) (choose_neg_least a') in
		     let e'' = Fact.Equal.map_rhs (S.replace s) e' in
		    repeat_case_iii (p, s) e'')
	in
	  repeat_case_iii (p, s) e'
  with                                (* [S.replace] removes dependent vars. *)
      Not_found ->  (* [k] is not a dependent variable *)
	let e0 = Fact.Equal.make (k, Arith.mk_zero(), rho) in
	  compose (p, s) e0
	    
and compose (p, s) e = 
  Trace.msg "la'" "Compose" e Fact.Equal.pp;
  let (p, s) = S.compose (p, s) [e] in      
    if !Arith.integer_solve 
      && is_diophantine e 
      && is_restricted_equality e
    then
      gomory_cut (p, s) e
    else 
      (p, s)

and fuse (p, s) e =
  Trace.msg "la'" "Fuse" e Fact.Equal.pp;
  S.fuse1 (p, s) e

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
and gomory_cut (p, s) e =
  assert(is_restricted_equality e);
  Trace.call "gomory" "Gomory cut" e Fact.Equal.pp;
  let (x, a, rho) = Fact.Equal.destruct e in (* [rho |- x = a]. *)
  let (b, ml) = Arith.destruct a in
  let b' = Mpa.Q.minus (Mpa.Q.def b) in
  let ml' = Arith.Monomials.mapq Mpa.Q.frac ml in
  let a' = Arith.mk_addq b' ml' in
  let rho' = Jst.dep1 rho in
    Trace.exit "gomory" "Gomory cut: nonnegative" a' Term.pp;
    process_nonneg (p, s) (a', rho')
  

(** Process a nonnegativity constraint of the form [a >= 0]. *)
and process_nonneg (p, s) (a, rho) = 
  match Arith.is_nonneg a with           (* cheap test on unexpanded term *)
    | Three.Yes -> 
	(p, s)   
    | Three.No -> 
	raise(Jst.Inconsistent rho)
    | Three.X -> 
	let (b, rho) = can (p, s) a in                   (* [rho |- a = b] *)
	  match Arith.is_nonneg b with      (* cheap test on expanded term *)
	    | Three.Yes -> 
		(p, s)
	    | Three.No -> 
		raise(Jst.Inconsistent rho)
	    | Three.X -> 
		let (k, tau) = mk_nonneg_slack s a rho in (* [tau |- k = b] *)
		let e = Fact.Equal.make (k, b, Jst.dep2 rho tau) in
		  process_equal (p, s) e

	      
and add_to_t (p, s) e =
  let (k, a, rho) = Fact.Equal.destruct e in
    assert(is_restricted_var k && is_restricted a);
    Trace.msg "la'" "Add_to_t" e Fact.Equal.pp;
    let c = Arith.constant_of a in
      if Q.is_nonneg (Arith.constant_of a) then
	compose (p, s) e
      else if  Arith.Monomials.Pos.is_empty a then
	raise(Jst.Inconsistent rho)
      else
	try
	  let e' = try_isolate_rhs_unbounded s e in         (* k = a == y = b *)   
	  let (y, b, _) = Fact.Equal.destruct e' in
	    if Term.Var.is_zero_slack k then
	      let b' = replace_zero_slacks b in
	      let e'' = Fact.Equal.make (y, b', rho) in     (* [rho |-y=b[k:=0]] *) 
		compose (p, s) e''
	    else 
		compose (p, s) e'
	with
	    Not_found ->  
	      assert(not(Arith.Monomials.Pos.is_empty a));
	      let y = choose_pos_least a in
	      let (p, s) = pivot (p, s) y in
	      let (a', rho') = S.replace s a in      (* [rho' |- a = a'] *)
		assert(not(Term.subterm y a'));      (* [S.replace] removes dependent vars. *)
		let rho' = Jst.dep2 rho rho' in      (* [rho' |- k = a'] *)
		  add_to_t (p, s) (Fact.Equal.make (k, a', rho')) 
		    


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
      Not_found -> false

and pivot (p, s) y =
  assert(not(is_unbounded s y));
  try
    let (g, e) = gain s y in
    let e' = isolate y e in     
      Trace.msg "la'" "Gain" g Mpa.Q.pp;
      Trace.msg "la'" "Pivot" (e, e') (Pretty.infix Fact.Equal.pp " ==> " Fact.Equal.pp);
      compose (p, s) e'
  with
      Not_found -> failwith "Fatal Error: Failed pivot step"


and infer (p, s) =
  let zs = analyze (p, s) in
    maximize (p, s) zs


(** The maximize step need only be applied to dependent variables [k] s.t.,
  [isZero(k)].  If [k = a+ + a-] in [t], then repeat: 
  - if [a+] is empty, then it is maximized at [0], and all the variables
  in k = [a-] can be set to [0].  
  - If [a+] contains an unbounded variable, then skip, since it is certainly 
  not equal to [0].  
  - Otherwise, if [a+] contains a variable [y] s.t., [isZero y] is false, then skip.  
  - Otherwise, pivot the first variable in [a+] in [t], to obtain [k = a']. *)
and maximize (p, s) zeros =
  let zeros = zeros in   (* zeros need to be sorted starting with least one. *)
    Trace.msg "la'" " Maximize" (Term.Set.elements zeros) (Pretty.set Term.pp);
    Term.Set.fold
      (fun k ((p, s) as acc) ->
	 try
	   let (a, rho) = S.apply s k in
	     maximize1 acc zeros (k, a, rho)
	 with
	     Not_found -> acc)
      zeros 
      (p, s)

      
and maximize1 (p, s) zeros (k, a, rho) =   (* [rho |- k = a] *)
  let monomial_is_unbounded (_, y) = is_unbounded s y in
  let monomial_is_nonzero (_, y) = not(Term.Set.mem y zeros) in
    Trace.msg "la'" "Maximize1" (Fact.Equal.make (k, a, rho)) Fact.Equal.pp;
    if Arith.Monomials.Pos.is_empty a then
      set_to_zero (p, s) (k, a, rho)
    else if Arith.Monomials.Pos.exists monomial_is_unbounded a then
      (p, s)
    else if Arith.Monomials.Pos.exists monomial_is_nonzero a then
      (p, s)
    else 
      let (_, y) = Arith.Monomials.Pos.least a in
	assert(not(is_unbounded s y));
	let (p, s) = pivot (p, s) y in
	let (b, tau) = S.find s k in
	  maximize1 (p, s) zeros (k, b, tau)


and set_to_zero (p, s) (x, a, rho) =
  assert(Arith.Monomials.Pos.is_empty a);
  Arith.Monomials.Neg.fold
    (fun (_, y) (p, s) ->
       let tau = Jst.dep1 rho in             (* [tau |- y = 0] *)
       let e = Fact.Equal.make (y, Arith.mk_zero(), tau) in
	 compose (p, s) e)
    a 
    (p, s)


(** Associated with each variable [x] in [t] a bit [is_zero x].  
  Initially, all the bits are false.  In the first step, all 
  independent variables [x] with gain [0] have [is_zero x] set to 
  true.  These are the variables that occur negatively in a rhs where 
  the constant is [0]. Repeat:
  If for some [y] such that [is_zero y], 
  it is the case that for each [k] in [negdep(y)] s.t. [find(k) = a+ + a-], 
  there is an [x] in [a+] such that not [is_zero x], then set 
  [is_zero y] to false. Finally, for dependent [k], s.t. [k = a+ + a-] in [t], 
   if forall [y] in [a+], [is_zero(y)], then [is_zero(k)] is set to true, too. *)
and analyze (p, s) =
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

let protect f c =
  if !Tools.destructive then
    let (p, s) = c in
      f (Partition.copy p, S.copy s)
  else 
    f c

(** [is_diseq (p, s) a b] iff adding [a = b] to [(p, s)] yields
  inconsistency. *)
let is_diseq (p, s) a b =
  try
    let e = Fact.Equal.make (a, b, Jst.dep0) in
    let _ = (protect process_equal (p, s)) e in
      None
  with
      Jst.Inconsistent(sigma) -> Some(sigma)

let is_diseq (p, s) = 
  Jst.Pred2.trace "la'" "La.is_diseq" (is_diseq (p, s))



(** {6 Inequality Tests} *)

(** Test if [a < 0] by asserting [a >= 0]. If this fails, then [a < 0] holds. *)
let rec is_neg (p, s) a =
  if not(Term.is_pure Th.la a) then None else
    try
      let _ = 
	protect process_nonneg (p, s) (a, Jst.dep0) 
      in
	None
    with
	Jst.Inconsistent(rho) -> Some(rho)

    
(** [a > 0] iff [-a < 0]. *)
and is_pos (p, s) a =  
  assert(Term.is_pure Th.la a);
  is_neg (p, s) (Arith.mk_neg a)
	
  
(** [a >= 0] iff [a > 0] or [a = 0]. *)
and is_nonneg (p, s) a = 
  assert(Term.is_pure Th.la a);
  match is_pos (p, s) a with
    | None -> is_zero (p, s) a
    | res -> res


(** [a <= 0] iff [a < 0] or [a = 0]. *)
and is_nonpos (p, s) a =
  assert(Term.is_pure Th.la a);
  match is_neg (p, s) a with
    | None -> is_zero (p, s) a
    | res -> res


(** Test if [a <= b]. *)
and is_le (p, s) (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg (p, s) (Arith.mk_sub b a)


(** Test if [a >= b]. *)
and is_ge (p, s) (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg (p, s) (Arith.mk_sub a b)


(** Test if [a > b]. *)
and is_gt (p, s) (a, b) = 
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_pos (p, s) (Arith.mk_sub a b)


(** Test if [a < b]. *)
and is_lt (p, s) (a, b) =
  is_gt (p, s) (b, a)


(** {6 Extremal Values} *)    

(** Maximize [a] in [s] by systematically eliminating positive 
  monomials in the canonical form [a'] of [a].  It returns either 
  - [(b, rho')] such that [b+] is empty and [rho' |- a = b], or
  - raises [Unbounded] if [a] is unbounded. *)
and upper (p, s) a =
  let rec max_term (p, s) a (b, rho) =     (* [rho |- b = a] *)
    if Term.is_var a then
      max_var a (b, rho)
    else 
      try
	let (_, x) = Arith.Monomials.Pos.least a in  (* choose least [x] in [a+]. *)
	let (_, e) = gain s x in          (* [y = ... + q*x + ...] with [q < 0].  *)
	let e' = isolate x e in           (* Isolate [x] in [e] and substitute *)
	let (a', rho') = apply1 e' a in   (* [rho' |- a = a'] *)
	let tau = Jst.dep2 rho rho' in
	  max_term (p, s) a' (b, tau)     (* [tau |- b = a'] *)
      with
	  Not_found -> 
	    assert(Arith.Monomials.Pos.is_empty a);
	    (a, rho)
  and max_var x (b, rho) =                     (* [rho |- x = b] *)
    assert(is_restricted_var x);
    let (p, s) = make_dependent (p, s) x in    (* make [x] a dependent variable. *)
    try
      let (a, rho') = S.apply s x in           (* [rho' |- x = a] *)
      let tau = Jst.dep2 rho rho' in
	max_term (p, s) a (b, tau)
    with
	Not_found -> raise Unbounded
  in
  let (b, rho) = can (p, s) a in
    if Arith.is_num b then
      (b, rho)
    else if is_unrestricted b then 
      raise Unbounded
    else
      protect 
	max_term (p, s) b (a, rho)              (* [rho |- a = b] *)


(** Update [s] such that [x] is a dependent variable whenever this variable 
  occurs in [s] *)
and make_dependent (p, s) x =
  assert(is_restricted_var x);
  let is_true _ = true in
  try
    let e = Dep.choose T s is_true x in   (* choose [y = a] such that [x] occurs in [a] *)
      compose (p, s) (isolate x e)        (* and solve [y = a] for [x]. *)
  with
      Not_found -> (p, s)
	

(** Either return a term of the form [q - a-] or raise [Unbounded]. *)
and lower (p, s) a = 
  let (b, rho) = upper (p, s) (Arith.mk_neg a) in
    (Arith.mk_neg b, rho)


(** Returns an upper bound or raises [Unbounded]. *)
and sup (p, s) a =
  let (b, rho) = upper (p, s) a in
    assert(Arith.Monomials.Pos.is_empty b);
    (Arith.constant_of b, rho)


(** Returns a lower bound or raises [Unbounded]. *)
and inf (p, s) a =
  let (b, rho) = lower (p, s) a in
    assert(Arith.Monomials.Neg.is_empty b);
    (Arith.constant_of b, rho) 

(** Closed interval with rational endpoints or [Unbounded]. *)
and bounds (p, s) a =
  let (lo, rho) = inf (p, s) a 
  and (hi, tau) = sup (p, s) a in
    (lo, hi, Jst.dep2 rho tau)
    


(** {6 Processing Disequalities} *)

(** Processing disequalities only deals with integer disequalities,
  which are maintained in the form [e <> n] where [n] is a natural 
  number. *) 
and process_diseq (p, s) d = 
  Trace.msg "la'" "Process" d Fact.Diseq.pp;
  assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
  let d = Fact.Diseq.map (can (p, s)) d in    (* replace dependent variables. *)
    match is_unsat (p, s) d with
      | Some(rho) -> 
	  raise(Jst.Inconsistent(rho))
      | None -> 
	  if !Arith.integer_solve &&
	    Fact.Diseq.both_sides Arith.is_diophantine d
	  then
	    process_diophantine_diseq (p, s) d
	  else 
	    process_nondiophantine_diseq (p, s) d

and process_nondiophantine_diseq (p, s) d =
  let (a, b, rho) = Fact.Diseq.destruct d in
  let (x, tau), (p, s) = name (p, s) a in
  let (y, sigma), (p, s) = name (p, s) b in
  let d = Fact.Diseq.make (x, y, Jst.dep3 rho tau sigma) in
    assert(Fact.Diseq.is_var d);
    (Partition.dismerge p d, s)

	
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
and process_diophantine_diseq (p, s) d =
  assert(Fact.Diseq.both_sides Arith.is_diophantine d);
  Trace.msg "la'" "Diophantine" d Fact.Diseq.pp;
  let (e, n, rho) = d_diophantine d in         (* [rho |- e <> n]. *)
    assert(Arith.is_int e);
    assert(Mpa.Q.is_integer n);
    try
      let (lo, hi, tau) = bounds (p, s) e in    (* [tau |- lo <= e <= hi] *)
      let n = Mpa.Q.to_z n in    
      let lo = Mpa.Q.ceil lo                    (* strengthen bounds to integers. *)
      and hi = Mpa.Q.floor hi in
	Trace.msg "foo7" "Lo" lo Mpa.Z.pp;
	Trace.msg "foo7" "Hi" hi Mpa.Z.pp;
	if Mpa.Z.lt n lo || Mpa.Z.gt n hi then  (* case [n < lo] or [n > hi] *)
	  let d = Fact.Diseq.make (e, Arith.mk_num(Mpa.Q.of_z n), rho) in
	  process_nondiophantine_diseq (p, s) d
	else                        (* minimal [min >= lo], maximal [max <= hi] s.t. *)
	  let (min, max, sigma) =      (* [sigma |- forall i in [min..max].  e <> i] *)
	    contiguous_diseq_segment (p, s) (lo, hi) (e, n)
	  in
	    assert(Mpa.Z.le lo min && Mpa.Z.le max hi);
	    Trace.msg "foo7" "Min" min Mpa.Z.pp;
	    Trace.msg "foo7" "Max" max Mpa.Z.pp;
	    if Mpa.Z.le min lo && Mpa.Z.ge max hi then 
	      let theta = Jst.dep3 rho tau sigma in
		raise(Jst.Inconsistent(theta))
	    else if Mpa.Z.le min lo && Mpa.Z.equal (Mpa.Z.succ max) hi then
	      let theta = Jst.dep3 rho tau sigma in  (* ==> [rho,tau,sigma |- e = hi] *)
	      let e = Fact.Equal.make (e, Arith.mk_num (Mpa.Q.of_z hi), theta) in
		process_equal (p, s) e
	    else if Mpa.Z.equal (Mpa.Z.succ lo) min && Mpa.Z.ge max hi then
	      let theta = Jst.dep3 rho tau sigma in  (* ==> [rho,tau,sigma |- e = lo] *)
	      let e = Fact.Equal.make (e, Arith.mk_num (Mpa.Q.of_z lo), theta) in
		process_equal (p, s) e
	    else 
	      let d = Fact.Diseq.make (e, Arith.mk_num (Mpa.Q.of_z n), rho) in
		process_nondiophantine_diseq (p, s) d
    with
	Unbounded -> 
	  let d = Fact.Diseq.make (e, Arith.mk_num n, rho) in
	    process_nondiophantine_diseq (p, s) d
		    

(** returns minimal [min], maximal [max] with
  - [lo <= min <= n <= max <= hi]
  - [forall i in [min..max]: e <> i] *)
and contiguous_diseq_segment (p, s) (lo, hi) (e, n) =
  assert(Mpa.Z.le lo n && Mpa.Z.le n hi); 
  let hyps = ref Jst.dep0 in
  let is_num_diseq m =       (* test if [e <> m] *)
    match is_diseq (p, s) e (Arith.mk_num(Mpa.Q.of_z m)) with
      | Some(tau) -> 
	  hyps := Jst.dep2 tau !hyps; true
      | None -> 
	  false
  in
  let rec upper max =     
    if Mpa.Z.ge max hi then max else 
      let max' = Z.succ max in
	if is_num_diseq max' then upper max' else max
  in
  let rec lower min =
    if Mpa.Z.le min lo then min else
    let min' = Mpa.Z.sub min Z.one in
      if is_num_diseq min' then lower min' else min
  in
  let (min, max) = (lower n, upper n) in
    (min, max, !hyps)



(** Test if disequality is unsatisfiable. *)
let unsat (p, s) d = 
  assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
  if S.is_empty s then () else  
    let (a, b, rho) = Fact.Diseq.destruct d in
      match is_equal (p, s) a b with
	| Some(rho) -> raise(Jst.Inconsistent(rho))
	| None -> ()



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

  let rec of_var (p, s) x = 
    try
      (if not(Term.Var.is_int x) then 
	 raise Unbounded
       else 
	 let (hi, _) = sup (p, s) x
	 and (lo, _) = inf (p, s) x in
	 let ds = integer_diseqs_between (p, s) x (lo, hi) in
	   make (Mpa.Q.floor lo, Mpa.Q.floor (Mpa.Q.add lo Mpa.Q.one), ds))
    with
	Unbounded -> raise Not_found
	
  and integer_diseqs_between (p, s) x (lo, hi) =
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

      
  (** Return [(q, rho)] such that [rho |- x = q] or raise [Not_found]. *)
  and d_num s x =
    let (a, rho) = S.apply s x in
    let p = Arith.d_num a in
      (p, rho)
  
  exception Found of Term.t * t

  (** Find a finite interpretation for one of the variables [x] in [a]. *)
  let of_term (p, s) a =
    try
      (Term.iter
	 (fun x ->
	    try
	      let fin = of_var (p, s) x in raise(Found(x, fin))
	    with 
		Not_found -> ())
	 a);
      raise Not_found
    with
	Found(x, fin) -> (x, fin)

  let disjunction (p, s) =
    let of_equal (x, a) = 
      try
	let fin = of_var (p, s) x in
	  raise(Found((x, fin)))
      with
	  Not_found -> 
	    (try
	       let (x, fin) = of_term (p, s) a in
		 raise(Found(x, fin))
	     with
		 Not_found -> ())
    in
      try                    
	S.iter 
	  (fun e -> 
	     let (x, a, _) = Fact.Equal.destruct e in
	       of_equal (x, a)) s;
	raise Not_found
      with
	  Found(x, fin) -> (x, fin)
end 


(** {6 Model Generation} *)

(** Given a pair [(s, d)] with [s] a tableau and [d] variable disequalities
  - Start with the zero assignment to all independent in [s] to obtain a
  ground substitution [s0], also replace all [s]-dependent variables in [d]
  to obtain [(s0, s[d])].
  - Take the disequalities and separately solve them as equalities with an
  ordering [<<] where [unrestricted << unbounded << slacks << integer slacks]
  to obtain [(s0, d')] with [d'] equals [solve(s)(s[d])]. Now all the disequalities 
  in [d'] are of the form [x <> e].  
  - Start with the highest (w.r.t. [<<]) variable disequality that is falsified.
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



let rec model s xl =
  Format.eprintf "\nWarning: model generation is work in progress...@.";
  let s0 = ground s in
    s0

and ground s =
  S.fold 
    (fun e s0 -> 
       let (x, a, rho) = Fact.Equal.destruct e in
       let (q, ml) = Arith.poly_of a in
       let s0' = 
	 Term.Map.add x (Arith.mk_num q) s0 
       in
       let s0'' = 
	 Arith.fold
	   (fun y acc -> 
	      if not(Term.Map.mem y acc) then
		Term.Map.add y (Arith.mk_zero()) acc
	      else 
		acc)
	   a s0'
       in
	 s0'')
    s Term.Map.empty

and diseqs (p, s) =
  ()


(** {6 Inference System} *)

(** Inference system for linear arithmetic. *)
module Infsys0: (Infsys.IS with type e = S.t) = struct

  let ths = Th.Set.singleton Th.la

  type e = S.t
      
  let abstract i a (g, s, p) = 
    assert(i = Th.la);
    assert(Term.is_pure i a);
    let (x, rho), (p, s) = name (p, s) a in
    let e = Fact.Equal.make (x, a, rho) in
    let g = Fact.Input.instantiate e g in
      (g, s, p)
		
  let merge i e (g, s, p) =
    assert(i = Th.la);
    assert(Fact.Equal.is_pure Th.la e);
    let (p, s) = process_equal (p, s) e in
      (g, s, p)

  (** Propagate a derived variable equality [x = y] with [y] canonical. *)
  let propagate e ((g, s, p) as c) =
    assert(Fact.Equal.is_var e);
    if S.is_empty s then c else 
      let (x, y, rho) = Fact.Equal.destruct e in 
      let (a, tau) = S.find s x
      and (b, sigma) = S.find s y in
	if Term.eq a b then c else 
	  let e = Fact.Equal.make (a, b, Jst.dep3 rho tau sigma) in
	  let (p, s) = merge_equal (p, s) e in
	    (g, s, p)

  let dismerge i d (g, s, p) = 
    assert(Fact.Diseq.is_pure i d);
    match is_unsat (p, s) d with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  let (p, s) = process_diseq (p, s) d in
	    (g, s, p)
    
  let propagate_diseq d (g, s, p) =
    assert(Fact.Diseq.is_var d);
    match is_unsat (p, s) d with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  let (p, s) = process_diseq (p, s) d in
	    (g, s, p)
    
  let branch (g, s, p) = 
    try
      let (x, fin) = Finite.disjunction (p, s) in
      let lo = fin.Finite.lo in
      let hi = fin.Finite.hi in
	assert(Mpa.Z.le lo hi);
	[(g, s, p)] (* to do *)
    with
	Not_found -> [(g, s, p)]

  let rec normalize (g, s, p) =
    (g, s, p)


end

(** Tracing inference system. *)
module Infsys: (Infsys.IS with type e = S.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = S.t
       let level = "la"
       let eq = S.eq
       let diff = S.diff
       let pp = S.pp
     end)
