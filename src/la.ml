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
  let is_restricted _ x = is_restricted_var x in
    Arith.Monomials.for_all is_restricted

let is_restricted_equality e =
  is_restricted (Fact.Equal.lhs_of e) &&
  is_restricted (Fact.Equal.rhs_of e)
      
let is_unrestricted a = not (is_restricted a)

let choose_unrestricted =
  let is_unrestr _ z = is_unrestricted z in
    Arith.Monomials.variable_choose is_unrestr

let choose_least = Arith.Monomials.variable_choose Arith.Monomials.is_true
let choose_neg_least = Arith.Monomials.Neg.variable_least_of
let choose_pos_least = Arith.Monomials.Pos.variable_least_of

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
  let (a, b, rho) = e in
    try
      let sl = Arith.solve (a, b) in
      let sl' = solved_normalize sl in
      let inj (a, b) = Fact.Equal.make a b rho in
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


(** [apply e a], for [e] is of the form [x = b], applies the
  substition [x := b] to [a]  *)
let apply1 e = 
  let (x, a, rho) = e in
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
  Trace.msg "foo9" "isolate" (x, e) (Pretty.pair Term.pp Fact.Equal.pp);
  let (a, b, rho) = e in
  try
    let (y, c) = Arith.isolate x (a, b) in
      assert(Term.eq x y);
      Fact.Equal.make x c rho
  with 
      Not_found -> failwith "Fatal Error: arithmetic isolation failed"


(** Check if all variables are interpreted over the integers. *)
let is_diophantine e =
  Fact.Equal.both_sides Arith.is_diophantine e

(** Destruct a disequality [a <> b] over integer terms as [e <> n] with
  [n] an integer. *)
let d_diophantine d =
  assert(Fact.Diseq.both_sides Arith.is_diophantine d);
  let (a, b, rho) = d in
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
    let a_sub_b = Arith.mk_sub a' b' in
    let q' = Arith.constant_of a_sub_b 
    and c' = Arith.nonconstant_of a_sub_b in  (* [q' + c' <> 0] *)
      (c', Mpa.Q.minus q')                    (* ==> [c' <> -q'] *)
  in
    (e, n, rho)


(** Fusing an equality [x = a] into right-hand sides of a solved list. *)
let solved_fuse e =
  let (x, a, rho) = e in
  let rec loop acc = function
    | [] -> acc
    | s :: sl -> 
	let (y, b, tau) = s in
	let b' = Arith.apply (x, a) b in
	let s' = if b == b' then s else Fact.Equal.make y b' (Jst.dep2 rho tau) in
	let acc' = s' :: acc in
	  loop acc' sl
  in
    loop []


(** [mk_diff e] for [e] of the form [a = b] constructs the difference
  [a - b] or [b - a] depending on whether the constant becomes positive. *)
let mk_diff e =
  let (a, b, rho) = e in
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
    let (x, a, _) = e in
      if is_restricted_var x && Mpa.Q.is_zero (Arith.constant_of a) then
	Term.Var.Set.add x zero
      else 
	zero
  let restrict zero e = 
    let (x, a, _) = e in
      if is_restricted_var x then
	Term.Var.Set.remove x zero
      else 
	zero
end


(** Solution set for linear arithmetic *)
module S: Solution.SET with type ext = Term.Var.Set.t = 
  Solution.Make(ZeroIdx)


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


(** Global variable for equality set. *)
let s = ref S.empty

let protect f a =
  let s0 = !s
  and p0 = !Infsys.p in
    try
      s := S.copy !s; Infsys.p := Partition.copy !Infsys.p;
      let b = f a in
	s := s0; Infsys.p := p0;
	b
    with
	exc -> 
	  s := s0; Infsys.p := p0;
	  raise exc

let call_with_configuration (p1, s1) f a =
  let s0 = !s
  and p0 = !Infsys.p in
    try
      s := s1; Infsys.p := p1;
      let b = f a in
	s := s0; Infsys.p := p0;
	b
    with
	exc -> 
	  s := s0; Infsys.p := p0;
	  raise exc


let update e = 
  S.update (!Infsys.p, !s) e

let apply a = S.apply !s a
let find a = Jst.Eqtrans.totalize (S.apply !s) a
let inv a = S.inv !s a
let dep x = S.dep !s x


(** Return domain of interpretation for [a]. *)
let dom a = 
  try
    let (b, rho) = inv a in
      (Arith.dom_of b, rho)
  with
      Not_found -> 
	(try
	   (Arith.dom_of a, Jst.dep0)
	 with
	     Not_found -> (Dom.Real, Jst.dep0))


(** Create fresh {i nonnegative slack} variable *)
let mk_nonneg_slack a rho =
  let (d, tau) = dom a in
  let k = Term.Var.mk_slack None (Var.nonneg d) in
    (k, Jst.dep2 tau rho)
 

(** Create fresh {i zero slack} variable *)
let mk_zero_slack a rho = 
  let k = Term.Var.mk_slack None Var.Zero in
    (k, rho)


(** Create fresh {i rename} variable *)
let mk_rename a rho =
  let v = Name.of_string "v" in
  let (d, tau) = dom a in
  let c = Var.Cnstrnt.mk_real d in
  let u = Term.Var.mk_rename v None c in
    (u, Jst.dep2 rho tau)


(** View solution set [S] as composed of two different 
  solution sets [R; T] with
  - [R] consisting of equalities [x = a] with [x] unrestricted
  - [T] consisting of equlities [k = a] with [k] restricted and all
  variables in [a] are restricted, too. *)
module Dep = struct

  let fold tag f =
    let f' e acc =
      let (x, _, _) = e in
	match tag with
	  | R -> if is_unrestricted_var x then f e acc else acc
	  | T -> if is_restricted_var x then f e acc else acc
    in
      S.Dep.fold !s f'

  let choose tag p =
    let p' ((x, _, _) as e) =
      match tag with
	| R -> if is_unrestricted_var x then p e else false
	| T -> if is_restricted_var x then p e else false
    in
      S.Dep.choose !s p'

  let for_all tag p =
    let p' e =
      let (x, _, _) = e in
	match tag with
	  | R -> if is_unrestricted_var x then p e else true
	  | T -> if is_restricted_var x then p e else true
    in
      S.Dep.for_all !s p'

  let iter tag f =
    let f' e =
      let (x, _, _) = e in
	match tag with
	  | R -> if is_unrestricted_var x then f e
	  | T -> if is_restricted_var x then f e
    in
      S.Dep.iter !s f'

end 


(** [Zero] index. *)
module Zero = struct

  (** [get t] returns the set of [x] with [x = a] in [t] such that
    the constant monomial of [a] is [0]. *)
  let get () = S.ext !s
 
  (** Apply [f e] to all equalities [k = a] in [t] such that [|a| = 0]. *)
  let iter f =
    let f' k =
      try
        f (S.equality !s k) 
      with
	  Not_found -> ()  (* invalid_arg "Fatal Error in Zero iteration" *)
    in
    let zeros = get () in
      Term.Var.Set.iter f' zeros

end
    

(** Iterators for [negdep] index. Hereby, a restricted variable
  [x] is said to {i negatively depend} on [y], if [x = a] is in [T]
  and [x] occurs in [a] with a negative coefficient. *)
module Negdep = struct
  
  (** [negdep t x] contains all [y] such that [y = a] in [t] and
    [x] occurs negatively in [a]. This index is realized as a subindex
    of the dependency structure of equality sets. *)
  let get x = 
    Dep.fold T
      (fun e acc ->
	 if occurs_negatively x e then
	   let y = Fact.Equal.lhs_of e in
	     Term.Set.add y acc
	 else 
	   acc)
      x Term.Set.empty

 
  (** Choose an arbitrary value in the [negdep] index. *)
  let choose x =
    Dep.choose T (occurs_negatively x)


  exception Found

  (** Check if [negdep] is empty for [x]. *)
  let is_empty x = 
    try
      Dep.iter T
	(fun e ->
	   if occurs_negatively x e then
	     raise Found)
	x;
      true
    with
	Found -> false

  
  (** Succedds if [f e] succeeds for all equalities [k = a] such  
    that [y] occurs negatively in [a]. *)
  let forall f y =
    let f' e =  if occurs_negatively y e then f e else true in
      Dep.for_all T f' y
     

  (** Apply [f] to all equalities [k = a] such that [y] occurs
    negatively in [a]. *)
  let iter f y =
    let f' e = 
      if occurs_negatively y e then f e 
    in
      Dep.iter T f' y
  
end 

 
(** [x] is {i unbounded} in [s] if [x] does not occur positively in [t], 
  that is, every coeffient of independent [x] is positive. *)
let is_unbounded a = 
  Negdep.is_empty a 


(** [x = a == y = a'] with [y] unbounded in [s]; otherwise [Not_found] is raised. *)
let try_isolate_rhs_positive_unbounded e =
  let is_unb _ x = is_unbounded x in
  let a = Fact.Equal.rhs_of e in
    if Mpa.Q.is_nonpos (Arith.constant_of a) then
      let y = Arith.Monomials.Pos.variable_choose is_unb a in
	isolate y e
    else 
      raise Not_found


(** [e] is of the form [k = a] with [k] unbounded and [|a|] positive. *)
let is_lhs_unbounded (x, a, _) = 
  try
    is_unbounded x
    && Mpa.Q.is_nonneg (Arith.constant_of a)
  with
      Not_found -> false


(** The gain [gain t x] is the minimum of all [|a|/(-q)]
  such that [y = a] in [t] with [q*x] in [a-], that is, [q < 0]. 
  The second argument is the equality [ y = ... + q * x + ...] 
  with [q < 0] for which the minimum gain is obtained.
  If [x] is {i unbounded} in [s], then [Unbounded] is raised. *)
let rec gain x = 
  let bound = ref false   (* only access [min] when [bound] is set. *)
  and min = ref (Obj.magic (-1)) in
    Negdep.iter
      (fun e -> 
	 let (k, a, _) = e in
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


(* If [a'] contains negative variables, then we see 
   if there is a negative variable [y] in [a'] such that the 
   gain of [y] in [T] is no smaller than the gain of [y] in [k = a'].  *)
let choose_negvar_with_no_smaller_gain a' =
  let c0 = Arith.constant_of a' in
    Arith.Monomials.Neg.variable_choose
      (fun q y ->
	 let g0 = Mpa.Q.div c0 (Mpa.Q.minus q) in (* gain of [y] in [k = a'] *)
	 let (g1, _) = gain y in                  (* gain of [y] in [t] *)
	   Mpa.Q.ge g1 g0)
      a'     


(** [replace s a] one-step replaces dependent variables [x] 
  in [a] with [b] if [x = b] in [s]. *)
let replace a =
  let hyps = ref Jst.dep0 in
  let lookup y = 
    try
      let (b, rho) = apply y in
	hyps := Jst.dep2 rho !hyps; b
    with
	Not_found -> y
  in
  let b = Arith.map lookup a in
    (b, !hyps)

(** Replace dependent variables in [s] with corresponding
  right hand sides (assumed to be canonical). This function
  should only be used in outer loops! *)
let can a =
  let hyps = ref Jst.dep0 in
  let lookup y = 
    try
      let (b, rho) = apply y in
	  hyps := Jst.dep2 rho !hyps; b
    with
	Not_found -> 
	  let (x, rho) = Partition.find !Infsys.p y in
	    hyps := Jst.dep2 rho !hyps; x
  in
  let b = Arith.map lookup a in
    (b, !hyps)

let is_canonical a =
  let (b, _) = can a in
    Term.eq a b

let name a =
  if Term.is_var a then 
    Jst.Eqtrans.id a 
  else
    let (b, rho) = can a in 
      try
	let (x, tau) = inv b in
	  (x, Jst.dep2 rho tau)
      with
	  Not_found ->
	    let (v, tau) = 
	      if Arith.is_zero b then
		mk_zero_slack b rho
	      else if Arith.is_nonneg b = Three.Yes then
		mk_nonneg_slack b rho 
	      else 
		mk_rename b rho
	    in
	    let e = Fact.Equal.make v a (Jst.dep2 rho tau) in
	      update e;
	      (v, tau)

let is_equal a b =
  let (a', rho') = can a 
  and (b', tau') = can b in
    if Term.eq a' b' then
      Some(Jst.dep2 rho' tau')
    else 
      None

(** Test if [a = 0] *)
let is_zero a =
  is_equal a (Arith.mk_zero())


(** Test if disequality [d] is unsatisfiable. *)
let is_diseq_infeasible d = 
  assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
  if S.is_empty !s then None else  
    let (a, b, rho) = d in
    let (a', tau) = can a
    and (b', sigma) = can b in
      if Term.eq a' b' then
	Some(Jst.dep3 rho tau sigma)
      else
	None


(** Check if all variables in [s] are canonical in [p].  *)
let is_canonical (s, p)	=
  S.for_all
    (fun e -> 
       let (x, a, _) = e in
	 Partition.is_canonical p x &&
	 Arith.for_all (Partition.is_canonical p) a)
    s


(** {6 Basic Updates} *)

(** All update functions manipulate configurations [(p, s)]
  consistiting of a variable partitioning [p] and a solution set [s]. *)
type config = Partition.t * S.t

let do_infer = ref false

let update ((k, a, _) as e) =
  Trace.msg "foo6" "Update" e Fact.Equal.pp;
  if is_restricted_var k &&
    Mpa.Q.is_zero (Arith.constant_of a) 
  then
    do_infer := true;                      (* if this update comes from pivoting, flag does not need to be set. *)
  S.update (!Infsys.p, !s) e

let fuse ((x, _, _) as e) = 
  let instantiate_rhs e' = 
    update (Fact.Equal.map_rhs (apply1 e) e') 
  in 
    S.Dep.iter !s instantiate_rhs x
      
(** Fusing a list of solved equalities [t] on rhs followed by updates of [el]. *)
let compose e =
  fuse e;
  let x = Fact.Equal.lhs_of e in           (* drop [x = a] with [x] an *)
    if not(Term.Var.is_fresh Th.la x) then (* internal variable. *)
      update e


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

let rec process_equal e =
  Trace.msg "la'" "Merge" e Fact.Equal.pp;
  compose_solved_list (solve e)


(** A solved form is composed one-by-one, since composing might
  involve resolving an equality in case the equality is restricted. *)
and compose_solved_list = function
  | [] -> ()
  | e :: sl -> compose_solved e sl
   
   
(** Process an equality [e] by trying to solve for an unrestricted
  variable. In this case, the equation is composed to [r]. Otherwise,
  the [e] consists only of unrestricted variables. *)
and compose_solved ((x, a, _) as e) sl = 
  if is_unrestricted_var x then
    begin
      compose e;
      compose_solved_list sl
    end 
  else  
    try
      let y = choose_unrestricted a in
      let e' = isolate y e in
	assert(Term.eq y (Fact.Equal.lhs_of e'));
	let sl' = solved_fuse e' sl in    (* propagate the fliped equality into *)
	  compose e';                    (* solved list [sl] to be processed. *)
	  compose_solved_list sl'
    with
	Not_found -> 
	  assert(is_restricted a);
	  compose_solved_restricted e;
	  compose_solved_list sl


and compose_solved_restricted e = 
  Trace.msg "la'" "Compose_solved_restricted" e Fact.Equal.pp;
  assert(is_restricted_equality e);
  if is_lhs_unbounded e then
    compose_and_cut e
  else 
    try
      compose_and_cut
	(try_isolate_rhs_positive_unbounded e)
    with
	Not_found -> 
	  let (diff, rho) = mk_diff e in                   (* [rho |- diff = 0] *)
	    assert(Mpa.Q.is_nonpos (Arith.constant_of diff));
	    let (k, tau) = mk_zero_slack diff rho in       (* [tau |- k = diff] *)
	      add_to_t (Fact.Equal.make k diff tau); 
	      eliminate_zero_slack (k, Jst.dep2 rho tau)
		 

and eliminate_zero_slack (k, rho) =                 (* [rho |- k = 0] *)
  try
    let (a', rho') = apply k in                     (* [rho' |- k = a'] *)
    let e' = Fact.Equal.make k a' rho' in
    let cmp = Mpa.Q.compare (Arith.constant_of a') Mpa.Q.zero in
      if cmp < 0 then                 (* I. [|a'| < 0] *)
	let tau = Jst.dep2 rho rho' in
	  raise(Jst.Inconsistent(tau))
      else if cmp = 0 then            (* II. [|a'| = 0] *)
	 (try 
	    compose_and_cut
	      (isolate (choose_least a') e');
	    let sigma = Jst.dep2 rho rho' in
	    let e0 = Fact.Equal.make k (Arith.mk_zero()) sigma in
	      compose_and_cut e0
	  with 
	      Not_found -> ())         (* skip *)
      else                             (* III. [|a'| > 0] *)
	let rec repeat_case_iii e' =
	  let (_, a', rho'') = e' in
	    if Arith.Monomials.Neg.is_empty a' then
	      raise(Jst.Inconsistent (Jst.dep3 rho rho' rho''))
	    else 
	      (try
		 let y = choose_negvar_with_no_smaller_gain a' in
		   pivot y;
		   let sigma = Jst.dep3 rho rho' rho'' in
		   let e'' = Fact.Equal.make k (Arith.mk_zero()) sigma in
		     compose_and_cut e''
	       with
		   Not_found -> 
		     assert(not(Arith.Monomials.Neg.is_empty a'));
		     pivot (choose_neg_least a');
		     let e'' = Fact.Equal.map_rhs replace e' in
		       repeat_case_iii e'')
	in
	  repeat_case_iii e'
  with                                (* [S.replace] removes dependent vars. *)
      Not_found ->  (* [k] is not a dependent variable *)
	let e0 = Fact.Equal.make k (Arith.mk_zero()) rho in
	  compose_and_cut e0
	    
and compose_and_cut e = 
  Trace.msg "la'" "Compose" e Fact.Equal.pp; 
  compose e;
  if !Arith.integer_solve 
    && is_diophantine e 
    && is_restricted_equality e
  then
    gomory_cut e 


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
and gomory_cut e =
  assert(is_restricted_equality e);
  Trace.call "gomory" "Gomory cut" e Fact.Equal.pp;
  let (x, a, rho) = e in (* [rho |- x = a]. *)
    if not(Term.Var.is_zero_slack x) then
      let b = Arith.constant_of a
      and ml = Arith.nonconstant_of a in
	if not(Mpa.Q.is_integer b) then      (* Gomory cut only on fractional constant. *)
	  let b' = Mpa.Q.minus (Mpa.Q.def b) in
	  let ml' = Arith.Monomials.mapq Mpa.Q.frac ml in
	  let a' = Arith.mk_addq b' ml' in
	    Trace.exit "gomory" "Gomory cut: nonnegative" a' Term.pp;
	    process_nonneg_restricted (a', rho)


and process_nonneg nn = 
  Trace.msg "la'" "process_nonneg" nn Fact.Nonneg.pp;
  let (a, rho) = Fact.Nonneg.destruct nn in      (* [rho |- a >= 0] *)
    match Arith.is_nonneg a with
      | Three.Yes -> 
	  ()
      | Three.No -> 
	  raise(Jst.Inconsistent(rho))
      | Three.X ->
	  (try
	     (let y = choose_unrestricted a in 
	      let (k, tau) = mk_nonneg_slack a rho in    (* [tau |- k = a] *) 
	      let e = Fact.Equal.make k a (Jst.dep2 rho tau) in
		compose (isolate y e))
	   with
	       Not_found ->
		   assert(is_restricted a);
		   process_nonneg_restricted (a, rho))


and process_nonneg_restricted (a, rho) =                 (* [rho |- a >= 0] *)
  Trace.msg "la'" "process_nonneg_restricted" a Term.pp;
  assert(is_restricted a); 
  let (k, tau) = mk_nonneg_slack a rho in                (* [tau |- k = a] *)
  let e = Fact.Equal.make k a (Jst.dep2 rho tau) in
    process_nonneg_make_feasible e

and process_nonneg_make_feasible e =
  Trace.msg "la'" "process_nonneg_make_feasible" e Fact.Equal.pp;
  assert(is_restricted_equality e);
  let (_, a, rho) = e in     (* Case I: [|a| >= 0] *)
    if Mpa.Q.is_nonneg (Arith.constant_of a) then 
      compose_and_cut e
    else 
      try                    (* Case II: rhs contains unbounded var, say [x], *)
	compose_and_cut      (* solve [e] to obtain [x = b],  now [|b| >= 0]  *)
	  (try_isolate_rhs_positive_unbounded e)
      with
	  Not_found ->
	    try              (* Case III: there is a positive (bounded) [x] in [rhs(e)].*)
	      let x = choose_pos_least a in 
		pivot x;  
		let e' = Fact.Equal.map_rhs replace e in
		  process_nonneg_make_feasible e'
	    with
		Not_found ->       (* Case IV: [a] is unbounded. *)
		  assert(Arith.Monomials.Pos.is_empty a);
		  raise(Jst.Inconsistent(rho))
		

and add_to_t ((k, a, rho) as e) =
  assert(is_restricted_var k && is_restricted a);
  Trace.msg "la'" "Add_to_t" e Fact.Equal.pp;
  if Q.is_nonneg (Arith.constant_of a) then
    compose_and_cut e
  else if Arith.Monomials.Pos.is_empty a then
    raise(Jst.Inconsistent rho)
  else
    try
      let ((y, b, _) as e') =                     (* k = a == y = b *)   
	try_isolate_rhs_positive_unbounded e 
      in 
	if Term.Var.is_zero_slack k then
	  let b' = replace_zero_slacks b in
	  let e'' = Fact.Equal.make y b' rho in    (* [rho |-y=b[k:=0]] *) 
	    compose_and_cut e''
	else 
	  compose_and_cut e'
    with
	Not_found ->  
	  assert(not(Arith.Monomials.Pos.is_empty a));
	  let y = choose_pos_least a in
	    pivot y;
	    let (a', rho') = replace a in          (* [rho' |- a = a'] *)
	      assert(not(Term.subterm y a'));      (* [S.replace] removes dependent vars. *)
	      let rho' = Jst.dep2 rho rho' in      (* [rho' |- k = a'] *)
		add_to_t (Fact.Equal.make k a' rho')


and pivot y =
  assert(not(is_unbounded y));
  try
    let (g, e) = gain y in
    let e' = isolate y e in 
    Trace.msg "la'" "Pivot" (e, e') (Pretty.infix Fact.Equal.pp " ==> " Fact.Equal.pp);
    Trace.msg "la'" "with gain" g Mpa.Q.pp;
      compose e'                        (* no Gomory cuts required for pivoting. *)
  with
      Not_found -> 
	failwith "Fatal Error: Failed pivot step"

(** [infer] sets variables to [0]. This is done by statically analyzeng
  variables to be {i stuck-at-zero} (does not necessarily mean they are
  equal to [0]!). The first round of pivoting
  in maximization does not affect the outcome of the static analysis.
  However, whenever some new equalities of the form k = 0 are generated
  from maximize, then one should compose these and repeat just
  the static analysis (not the maximize).   This need only be
  repeated once since the equalities k = 0 generated by static
  analysis are already closed.  One could make the second static
  analysis incremental. *)
and infer () =
  let stuck_at_zeros = analyze () in  (* 1. find all stuck at zero variables. *)    
    maximize stuck_at_zeros       (* 2. maximize all zeros which are not stuck at zero. *)

and toplevel f a =
  Trace.msg "foo6" "Toplevel" () Pretty.unit;
  do_infer := false;
  f a;
  if !do_infer then infer ()
              (* ADD: infer only needed if [x = a] with [|a| = 0] has been 
                 added other than with pivoting!!! *)


(** The maximize step need only be applied to dependent variables [k] s.t.,
  [isZero(k)].  If [k = a+ + a-] in [t], then repeat: 
  - if [a+] is empty, then it is maximized at [0], and all the variables
  in k = [a-] can be set to [0].  
  - If [a+] contains an unbounded variable, then skip, since it is certainly 
  not equal to [0]. 
  - Otherwise, if [a+] contains a variable [y] s.t., [isZero y] is false, then skip.  
  - Otherwise, pivot the first variable in [a+] in [t], to obtain [k = a']. *)
and maximize stuck_at_zeros =
  Term.Var.Set.iter                   (* should be ordered? *)
    (fun z ->
       assert(Term.is_var z);
       try
	 let (a, rho) = apply z in
	   maximize1 (z, a, rho)
       with
	   Not_found -> ())
    stuck_at_zeros
      
and maximize1 ((k, a, rho) as e) =         (* [rho |- k = a] *)
  let monomial_is_unbounded _ y = is_unbounded y in
    Trace.msg "la'" "Maximize1" e Fact.Equal.pp;
    if Arith.Monomials.Pos.is_empty a then
      (if Mpa.Q.is_zero (Arith.constant_of a) then
	set_to_zero e)
    else if Arith.Monomials.Pos.exists monomial_is_unbounded a then
      ()
    else 
      let y = choose_pos_least a in
	assert(not(is_unbounded y));
	pivot y;
	let (b, tau) = find k in          (* [tau |- k = b] *)
	  maximize1 (k, b, Jst.dep2 rho tau)

and set_to_zero (x, a, rho) =             (* [rho |- x = a] *)
  Arith.Monomials.Neg.iter
    (fun _ y ->
       let e = Fact.Equal.make y (Arith.mk_zero()) rho in
	 Trace.msg "la'" "Set_to_zero" e Fact.Equal.pp;
	 compose e)
    a

and analyze () =
  let stuck_at_zero = analyze_initial () in
  Trace.msg "foo6" "Stuck_at_zero" (Term.Var.Set.elements stuck_at_zero) (Pretty.set Term.pp);
  let stuck_at_zero' = analyze_refine stuck_at_zero in
   Trace.msg "foo6" "Refined Stuck_at_zero" (Term.Var.Set.elements stuck_at_zero') (Pretty.set Term.pp);
    stuck_at_zero'

and analyze_initial () =
  let stuck_at_zeros = ref Term.Var.Set.empty in
    Zero.iter
      (fun ((y, a, _) as e) ->                    (* [|a| = 0] *)
	 assert(Mpa.Q.is_zero(Arith.constant_of a));
	 if Arith.Monomials.Pos.is_empty a then (* [a] is maximized at [0]. *)
	   (if not(Arith.Monomials.Neg.is_empty a) then
	     set_to_zero e)
	 else 
	   stuck_at_zeros := Term.Var.Set.add y !stuck_at_zeros);
    !stuck_at_zeros

and analyze_independent_zeros stuck_at_zeros =
  let current = ref Term.Var.Set.empty in
    Term.Var.Set.iter
      (fun k -> 
	 let (a, _) = apply k in    
	   Arith.Monomials.Neg.iter
	     (fun _ x ->
		current := Term.Var.Set.add x !current)
	     a)
      stuck_at_zeros;
    !current
	
and analyze_refine stuck_at_zeros =
  let independent_zeros = analyze_independent_zeros stuck_at_zeros in
  let not_independent_zeros _ y = not (Term.Var.Set.mem y independent_zeros) in
  let stuck_at_zeros' = 
    Term.Var.Set.fold
      (fun k acc ->
	 let (a, _) = apply k in                        (* [k = a]. *)
	   if Arith.Monomials.Pos.exists not_independent_zeros a then
	     Term.Var.Set.remove k acc
	   else 
	     acc)
      stuck_at_zeros
      stuck_at_zeros
  in
    if stuck_at_zeros == stuck_at_zeros' then
      stuck_at_zeros'
    else 
      analyze_refine stuck_at_zeros'



(* Old analysis

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
and analyze () =
  let stuck_at_zeros = analyze_initial () in
  let stuck_at_zeros' = analyze_refine stuck_at_zeros in
  let stuck_at_zeros'' = analyze_final stuck_at_zeros' in
    (stuck_at_zeros'', zeros)


and analyze_refine stuck_at_zeros =
  let not_is_zero _ y = not(Term.Var.Set.mem y stuck_at_zeros) in
    Term.Var.Set.fold
      (fun y acc ->
	 if (Negdep.forall                                (* choose [y] such that [is_zero y] and *)
	       (fun (_, a, _) ->                          (* for all [k] in [negdep y] and [k = a] in [t] *)
		  Mpa.Q.is_zero (Arith.constant_of a) &&  (* there is an [x] in [a+] such that [not(is_zero x)]. *)
		  Arith.Monomials.Pos.exists not_is_zero a)
	       y)
	 then
	   Term.Var.Set.remove y acc
	 else 
	   acc)
      stuck_at_zeros
      stuck_at_zeros


and analyze_initial () = 
  let stuck_at_zeros = ref Term.Var.Set.empty
  and zeros = ref Term.Var.Set.empty in
    Zero.iter
      (fun e ->                  (* [|a| = 0] *)
	 let a = Fact.Equal.rhs_of e in
	   assert(Mpa.Q.is_zero(Arith.constant_of a));
	   Arith.Monomials.Neg.iter 
	     (fun _ y ->            (* filter out variables maximized at zero. *)
		if Arith.Monomials.Pos.is_empty a then 
		  zeros := Term.Var.Set.add y !zeros
		else 
		  stuck_at_zeros := Term.Var.Set.add y !stuck_at_zeros)
	     a);
    (!stuck_at_zeros, !zeros)


and analyze_final stuck_at_zeros = 
  let is_zero _ y = Term.Var.Set.mem y stuck_at_zeros in
  let current = ref stuck_at_zeros in
    Zero.iter
      (fun (k, a, _) ->                  (* [k = a] with [|a| = 0]. *)
	 if Arith.Monomials.Pos.for_all is_zero a then
	   current := Term.Var.Set.add k !current);
    !current
*)


(** [is_diseq (p, s) a b] iff adding [a = b] to [(p, s)] yields
  inconsistency. *)
let is_diseq a b =
  try
    let (a', rho') = can a
    and (b', tau') = can b in
    let e = Fact.Equal.make a' b' (Jst.dep2 rho' tau') in
    let _ = protect process_equal e in
      None
  with
      Jst.Inconsistent(sigma) -> Some(sigma)

let is_diseq = 
  Jst.Pred2.trace "foo" "is_diseq" is_diseq


(** {6 Extremal Values} *)    

(** Maximize [a] in [s] by systematically eliminating positive 
  monomials in the canonical form [a'] of [a].  It returns either 
  - [(b, rho')] such that [b+] is empty and [rho' |- a = b], or
  - raises [Unbounded] if [a] is unbounded. *)
let rec upper a =
  let rec max_term a (b, rho) =           (* [rho |- b = a] *)
    if Term.is_var a then
      max_var a (b, rho)
    else 
      try
	let x = choose_pos_least a in     (* choose least [x] in [a+]. *)
	let (_, e) = gain x in            (* [y = ... + q*x + ...] with [q < 0].  *)
	let e' = isolate x e in           (* Isolate [x] in [e] and substitute *)
	let (a', rho') = apply1 e' a in   (* [rho' |- a = a'] *)
	let tau = Jst.dep2 rho rho' in
	  max_term a' (b, tau)            (* [tau |- b = a'] *)
      with
	  Not_found -> 
	    assert(Arith.Monomials.Pos.is_empty a);
	    (a, rho)
  and max_var x (b, rho) =                 (* [rho |- x = b] *)
    assert(is_restricted_var x);
    make_dependent x;                      (* make [x] a dependent variable. *)
    try
      let (a, rho') = apply x in           (* [rho' |- x = a] *)
      let tau = Jst.dep2 rho rho' in
	max_term a (b, tau)
    with
	Not_found -> raise Unbounded
  in
  let (b, rho) = can a in
    if Arith.is_num b then
      (b, rho)
    else if is_unrestricted b then 
      raise Unbounded
    else
      protect 
	(max_term b) (a, rho)               (* [rho |- a = b] *)


(** Update [s] such that [x] is a dependent variable whenever 
  this variable occurs in [s] *)
and make_dependent x =
  assert(is_restricted_var x);
  let is_true _ = true in
  try
    let e = Dep.choose T is_true x in     (* choose [y = a] such that [x] occurs in [a] *)
      compose (isolate x e)               (* and solve [y = a] for [x]. *)
  with
      Not_found -> ()
	

(** Either return a term of the form [q - a-] or raise [Unbounded]. *)
and lower a = 
  let (b, rho) = upper (Arith.mk_neg a) in
    (Arith.mk_neg b, rho)


(** Returns an upper bound or raises [Unbounded]. *)
and sup a =
  let (b, rho) = upper a in
    assert(Arith.Monomials.Pos.is_empty b);
    (Arith.constant_of b, rho)


(** Returns a lower bound or raises [Unbounded]. *)
and inf a =
  let (b, rho) = lower a in
    assert(Arith.Monomials.Neg.is_empty b);
    (Arith.constant_of b, rho) 

(** Closed interval with rational endpoints or [Unbounded]. *)
and bounds a =
  let (lo, rho) = inf a 
  and (hi, tau) = sup a in
    (lo, hi, Jst.dep2 rho tau)
    


(** {6 Processing Disequalities} *)

(** Processing disequalities only deals with integer disequalities,
  which are maintained in the form [e <> n] where [n] is a natural 
  number. *) 
and process_diseq d = 
  Trace.msg "la'" "Process" d Fact.Diseq.pp;
  assert(Fact.Diseq.both_sides (Term.is_pure Th.la) d);
  let d = Fact.Diseq.map replace d in     (* replace dependent variables. *)
    match is_diseq_infeasible d with
      | Some(rho) -> 
	  raise(Jst.Inconsistent(rho))
      | None -> 
	  if !Arith.integer_solve &&
	    Fact.Diseq.both_sides Arith.is_diophantine d
	  then
	    process_diophantine_diseq d
	  else 
	    process_nondiophantine_diseq d

and process_nondiophantine_diseq d =
  let d = Fact.Diseq.map name d in
    assert(Fact.Diseq.is_var d);
    Partition.dismerge !Infsys.p d

	
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
and process_diophantine_diseq d =
  assert(Fact.Diseq.both_sides Arith.is_diophantine d);
  Trace.msg "la'" "Diophantine" d Fact.Diseq.pp;
  let (e, n, rho) = d_diophantine d in         (* [rho |- e <> n]. *)
    assert(Arith.is_int e);
    assert(Mpa.Q.is_integer n);
    try
      let (lo, hi, tau) = bounds e in          (* [tau |- lo <= e <= hi] *)
      let n = Mpa.Q.to_z n in    
      let lo = Mpa.Q.ceil lo                    (* strengthen bounds to integers. *)
      and hi = Mpa.Q.floor hi in
	if Mpa.Z.lt n lo || Mpa.Z.gt n hi then  (* case [n < lo] or [n > hi] *)
	  let d = Fact.Diseq.make e (Arith.mk_num(Mpa.Q.of_z n)) rho in
	  process_nondiophantine_diseq d
	else                        (* minimal [min >= lo], maximal [max <= hi] s.t. *)
	  let (min, max, sigma) =      (* [sigma |- forall i in [min..max].  e <> i] *)
	    contiguous_diseq_segment (lo, hi) (e, n)
	  in
	    assert(Mpa.Z.le lo min && Mpa.Z.le max hi);
	    Trace.msg "foo7" "Min" min Mpa.Z.pp;
	    Trace.msg "foo7" "Max" max Mpa.Z.pp;
	    if Mpa.Z.le min lo && Mpa.Z.ge max hi then 
	      let theta = Jst.dep3 rho tau sigma in
		raise(Jst.Inconsistent(theta))
	    else if Mpa.Z.le min lo && Mpa.Z.equal (Mpa.Z.succ max) hi then
	      let theta = Jst.dep3 rho tau sigma in  (* ==> [rho,tau,sigma |- e = hi] *)
	      let e = Fact.Equal.make e (Arith.mk_num (Mpa.Q.of_z hi)) theta in
		process_equal e
	    else if Mpa.Z.equal (Mpa.Z.succ lo) min && Mpa.Z.ge max hi then
	      let theta = Jst.dep3 rho tau sigma in  (* ==> [rho,tau,sigma |- e = lo] *)
	      let e = Fact.Equal.make e (Arith.mk_num (Mpa.Q.of_z lo)) theta in
		process_equal e
	    else 
	      let d = Fact.Diseq.make e (Arith.mk_num (Mpa.Q.of_z n)) rho in
		process_nondiophantine_diseq d
    with
	Unbounded -> 
	  let d = Fact.Diseq.make e (Arith.mk_num n) rho in
	    process_nondiophantine_diseq d
		    

(** returns minimal [min], maximal [max] with
  - [lo <= min <= n <= max <= hi]
  - [forall i in [min..max]: e <> i] *)
and contiguous_diseq_segment (lo, hi) (e, n) =
  assert(Mpa.Z.le lo n && Mpa.Z.le n hi); 
  let hyps = ref Jst.dep0 in
  let is_num_diseq m =       (* test if [e <> m] *)
    match is_diseq e (Arith.mk_num(Mpa.Q.of_z m)) with
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


(** Processing a positivity constraint. *)

let process_pos pp =
  let pp = Fact.Pos.map replace pp in
  let (a, rho) = Fact.Pos.destruct pp in
  let nn = Fact.Nonneg.make (a, rho) 
  and dd = Fact.Diseq.make a (Arith.mk_zero()) rho in 
    process_nonneg nn;
    process_diseq dd


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

  let rec of_var x = 
    try
      (if not(Term.Var.is_int x) then 
	 raise Unbounded
       else 
	 let (hi, _) = sup x
	 and (lo, _) = inf x in
	 let ds = integer_diseqs_between x (lo, hi) in
	   make (Mpa.Q.floor lo, Mpa.Q.floor (Mpa.Q.add lo Mpa.Q.one), ds))
    with
	Unbounded -> raise Not_found
	
  and integer_diseqs_between x (lo, hi) =
    D.Set.fold
      (fun (y, _) acc ->
	 try
	   let (q, _) = d_num y in   (* [y = q] *)
	     if Mpa.Q.is_integer q && Mpa.Q.le lo q && Mpa.Q.le q hi then
	       Zset.add (Mpa.Q.to_z q) acc
	     else 
	       acc
	 with
	     Not_found -> acc)
      (Partition.diseqs !Infsys.p x)
      Zset.empty

      
  (** Return [(q, rho)] such that [rho |- x = q] or raise [Not_found]. *)
  and d_num x =
    let (a, rho) = apply x in
    let p = Arith.d_num a in
      (p, rho)
  
  exception Found of Term.t * t

  (** Find a finite interpretation for one of the variables [x] in [a]. *)
  let of_term a =
    try
      (Term.iter
	 (fun x ->
	    try
	      let fin = of_var x in raise(Found(x, fin))
	    with 
		Not_found -> ())
	 a);
      raise Not_found
    with
	Found(x, fin) -> (x, fin)

  let disjunction ()=
    let of_equal (x, a) = 
      try
	let fin = of_var x in
	  raise(Found((x, fin)))
      with
	  Not_found -> 
	    (try
	       let (x, fin) = of_term a in
		 raise(Found(x, fin))
	     with
		 Not_found -> ())
    in
      try                    
	S.iter 
	  (fun e -> 
	     let (x, a, _) = e in
	       of_equal (x, a)) !s;
	raise Not_found
      with
	  Found(x, fin) -> (x, fin)
end 



(** {6 Inference System} *)

(** Inference system for linear arithmetic. *)
module Infsys0: (Infsys.ARITH with type e = S.t) = struct

  type e = S.t
  
  let current () = !s

  let protected = ref false

  let initialize s0 =
    protected := false;
    s := S.copy s0

  let finalize () = !s

  open Infsys
      
  let abstract a = 
    assert(Term.is_pure Th.la a);
    let (x, rho) = name a in
      G.replace (Fact.Equal.make x a rho) !g
		
  let merge e =
    assert(Fact.Equal.is_pure Th.la e);
    toplevel 
      process_equal
        (Fact.Equal.map can e)

  (** Propagate variable equality [x = y]. *)
  let rec propagate e =
    assert(Fact.Equal.is_var e);
    if not(S.is_empty !s) then
      let (x, y, _) = e in
	if occurs x && occurs y then
	  let ((a, b, _) as e') = Fact.Equal.map find e in
	    if not(Term.eq a b) then
	      toplevel 
		process_equal e'

  and occurs x = true        (* following is incomplete. why? *)
  (* S.is_dependent !s x || S.is_independent !s x *)

  let dismerge d = 
    assert(Fact.Diseq.is_pure Th.la d);
    let d' = Fact.Diseq.map can d in
    match is_diseq_infeasible d' with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  toplevel
	    process_diseq d'
    
  let propagate_diseq d =
    assert(Fact.Diseq.is_var d);
    match is_diseq_infeasible d with
      | Some(rho) ->
	  raise(Jst.Inconsistent(rho))
      | None ->
	  if Fact.Diseq.both_sides Arith.is_diophantine d then
	    toplevel
	      process_diseq d
    
  let branch () = 
    let (x, fin) = Finite.disjunction () in
    let lo = fin.Finite.lo in
    let hi = fin.Finite.hi in
      assert(Mpa.Z.le lo hi);
      raise Not_found (* to do *)
	  
  let normalize _ = ()

  let nonneg nn =
    assert(Fact.Nonneg.is_pure Th.la nn);
    let nn = Fact.Nonneg.map can nn in
    let (a, rho) = Fact.Nonneg.destruct nn in
      match Arith.is_nonneg a with (* cheap test*)
	| Three.Yes -> 
	    ()
	| Three.No -> 
	    raise(Jst.Inconsistent rho)
	| Three.X -> 
	    let nn = Fact.Nonneg.make (a, rho) in
	      toplevel 
		process_nonneg nn

  let pos pp =
    assert(Fact.Pos.is_pure Th.la pp);
    let pp = Fact.Pos.map can pp in
    let (a, rho) = Fact.Pos.destruct pp in
      match Arith.is_pos a with (* cheap test*)
	| Three.Yes -> 
	    ()
	| Three.No ->
	    raise(Jst.Inconsistent rho)
	| Three.X -> 
	    toplevel
	      process_pos pp


end



(** Tracing inference system. *)
module Infsys: (Infsys.ARITH with type e = S.t) =
  Infsys.TraceArith(Infsys0)
    (struct
       type t = S.t
       let level = "la"
       let eq = S.eq
       let diff = S.diff
       let pp = S.pp
     end)


(** {6 Inequality Tests} *)

(** Test if [a < 0] by asserting [a >= 0]. If this fails, then [a < 0] holds. *)
let rec is_neg a =
  if not(Term.is_pure Th.la a) then None else
    try
      let _ = 
	let rho = Jst.axiom (Atom.mk_nonneg a) in
	let nn = Fact.Nonneg.make (a, rho) in
	  protect process_nonneg nn
      in
	None
    with
	Jst.Inconsistent(tau) -> Some(tau)

    
(** [a > 0] iff [-a < 0]. *)
and is_pos a =  
  assert(Term.is_pure Th.la a);
  is_neg (Arith.mk_neg a)
	
  
(** [a >= 0] iff [a > 0] or [a = 0]. *)
and is_nonneg a = 
  assert(Term.is_pure Th.la a);
  match is_pos a with
    | None -> is_zero a
    | res -> res


(** [a <= 0] iff [a < 0] or [a = 0]. *)
and is_nonpos a =
  assert(Term.is_pure Th.la a);
  match is_neg a with
    | None -> is_zero a
    | res -> res


(** Test if [a <= b]. *)
and is_le (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg (Arith.mk_sub b a)


(** Test if [a >= b]. *)
and is_ge (a, b) =
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_nonneg (Arith.mk_sub a b)


(** Test if [a > b]. *)
and is_gt (a, b) = 
  assert(Term.is_pure Th.la a && Term.is_pure Th.la b);
  is_pos (Arith.mk_sub a b)


(** Test if [a < b]. *)
and is_lt (a, b) =
  is_gt (b, a)



(** External functions *)
let can cfg = call_with_configuration cfg can
let upper cfg = call_with_configuration cfg upper
let lower cfg = call_with_configuration cfg lower
let is_equal cfg = call_with_configuration cfg is_equal
let is_nonpos cfg = call_with_configuration cfg is_nonpos
let is_nonnneg cfg = call_with_configuration cfg is_nonneg



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
    (fun (x, a, rho) s0 -> 
       let q = Arith.constant_of a
       and ml = Arith.nonconstant_of a in
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
