
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
open Mpa
open Term
(*i*)


(*s Slack variables *)

let slacks = ref Term.Set.empty
let _ = Tools.add_at_reset (fun () -> slacks := Term.Set.empty)

let is_slack x = Term.Set.mem x !slacks

let mk_slack =
  fun () -> 
    let k = Term.mk_fresh_param "k" None in
    Trace.msg 10 "Slack" k Term.pp;
    slacks := Term.Set.add k !slacks;
    k


(*s Constraint context as a map of term variables to constraints.
 Codomain of constraints consist of an explicit constraint and
 a list of symbolic constraints [(c, a)]. Such symbolic constraints
 denote the constraint [c] minus the current constraint for [a]. *)

type t = {
  find : (Cnstrnt.t * (Cnstrnt.t * Term.t) list) Term.Map.t;
  use : Use.t;
}


let apply s x = Term.Map.find x s.find


let find =
  let mk_real = (Cnstrnt.mk_real, []) in
  fun s x -> 
    try Term.Map.find x s.find with Not_found -> mk_real


(*s The set of slacks in the domain of [s] for which [k] occurs
 in the symbolic constraint. *)

let use s = Use.find s.use


(*s Check if [x] is in the domain. *)

let mem x s = Term.Map.mem x s.find


(*s Check if [x] is either in the domain or a subterm of an implicit constraint. *)

let occurs x s =
  mem x s || not(Term.Set.is_empty (use s x))


(*s [cnstrnt s a] computes best constraint for [a] in [s]. *)

let cnstrnt s a = 
  let rec of_term a =
    match Arith.d_interp a with
      | Some(Sym.Num(q), []) -> Cnstrnt.mk_singleton q
      | Some(Sym.Mult, l) -> Cnstrnt.multl (List.map of_term l)
      | Some(Sym.Add, l) -> Cnstrnt.addl (List.map of_term l)
      | Some(Sym.Expt(n), [x]) -> Cnstrnt.expt n (of_term x)
      | _ -> fst(apply s a)
  in
  try of_term a with Not_found -> Cnstrnt.mk_real
  

(*s [meaning s c] instantiates the symbolic constraint [c] in context [s]. *)

let rec meaning s (c,l) =
  let d = List.fold_right 
	    (fun (e,x) -> Cnstrnt.inter (meaning1 s (e,x))) 
	    l Cnstrnt.mk_real 
  in
  Cnstrnt.inter c d

and meaning1 s (c, a) =
  Cnstrnt.subtract c (cnstrnt s a)


(*s Constraints as a list. *)

let to_list s =
  Term.Map.fold (fun x sc acc -> (x, meaning s sc) :: acc) s.find []


(*s Pretty-printing. *)

let pp_diff fmt (c,a) = 
  Cnstrnt.pp fmt c;
  Pretty.string fmt " - Cnstrnt("; 
  Term.pp fmt a; 
  Pretty.string fmt ")"

let pp fmt s =
  let ppcod fmt (c, l) = 
    Cnstrnt.pp fmt c;
    if l <> [] then
      begin
	Pretty.string fmt " inter ";
	Pretty.infixl pp_diff " inter " fmt l
      end
  in
  let findl = Map.fold (fun x cod acc -> (x,cod) :: acc) s.find [] in
  Pretty.map Term.pp ppcod fmt findl


(*s The empty constraint map. *)

let empty = {
  find = Term.Map.empty;
  use = Use.empty
}

(*s Test for emptyness. *)

let is_empty s = (s == empty)


(*s Extend domain of constraint map. *)

let extend c s =
  if Cnstrnt.is_empty c then
    raise Exc.Inconsistent
  else 
    let k = mk_slack () in
    Trace.msg 3 "Extend(c)" (k,c) Term.pp_in;
    let s' = {s with find = Map.add k (c, []) s.find} in
    (k, s')

(*s [update k c s] updates the explicit constraint of [k] and leaves
 the symbolic constraint unchanged. *)

let update k c s =
  assert(is_slack k);
  let l = try snd(apply s k) with Not_found -> [] in
  {s with find = Term.Map.add k (c,l) s.find}
	

(*s [restrict k s] physically removes [k] from the [find] map and
 updates the [use] lists accordingly. *)

let restrict k s =
  assert(is_slack k);
  try
    let (c,l) = apply s k in  
    Trace.msg 3 "Restrict(c)" k Term.pp;
    {s with 
       find = Term.Map.remove k s.find; 
       use = List.fold_right (fun (_,a) -> Use.remove Arith.fold k a) l s.use}
  with
      Not_found -> s


(*s Normalize constraint such as ['2 * x + 5' in 'c'] 
 to ['x' in '1/2 ** (c -- 5)'], where ['**'], ['--']
 are abstract interval operations for linear multiplication and subtraction. *)

let normalize q p c =           
  if Q.is_zero q && Q.is_one p then
    c
  else if Q.is_zero q then
    Cnstrnt.multq (Q.inv p) c
  else 
    Cnstrnt.multq (Q.inv p) (Cnstrnt.addq (Mpa.Q.minus q) c)

(*s [process (eqs, cnstrnts, s, eqs)] processes the equalities
 [keqs] and the constraints [cnstrnts] in the "to do" argument
  and returns an updated state [s] together with generated equalities [keqs] 
  for slack variables. *)

let rec process (eqs, cnstrnts, s, keqs) =
  match eqs, cnstrnts with
    | [], [] -> 
	(s, keqs)
    | eqs', (a,c) :: cnstrnts' ->
	process (addcnstrnt a c (eqs', cnstrnts', s, keqs))
    | (a, b) :: eqs', cnstrnts' ->
	process (equality (a, b) (eqs', cnstrnts', s, keqs))


(*s Adding a constraint [a in c], where [a] is an arbitrary
 term to the state. *)

and addcnstrnt a c ((eqs, cnstrnts, s, keqs) as acc) =
  if is_slack a then
    refine a c acc
  else 
    match Arith.decompose a with     
      | Arith.Const(q) ->                     (* Case: [k = q] *)
	  if Cnstrnt.mem q c then acc else raise Exc.Inconsistent
      | Arith.One(q, p, k) ->                 (* Case: [q + p * k in c] *)
	  refine k (normalize q p c) acc      (* iff [k in 1/p * (c - q)] *)  
      | Arith.Many(q, p, k, y) ->             (* Case: [q + p * k + y in c] *)
	  let c' = normalize q p c in         (* iff [k in ((1/p*(c-q)) - C(1/p*y)] *)
	  let y' = Arith.mk_multq (Q.inv p) y in  
	  symbolic k (c', y') acc
  

(*s Add a constraint [k in c] to [s]. *)

and refine k c ((eqs, cnstrnts, s, keqs) as acc) = 
  assert(is_slack k);
  Trace.msg 4 "Add(c)" (k, c) pp_in;
   try
     let (d, l) = apply s k in
     match Cnstrnt.cmp c d with
       | Binrel.Disjoint ->
	   raise Exc.Inconsistent
       | (Binrel.Super | Binrel.Same) ->
	   acc
       | Binrel.Sub ->
	   let s' = update k c s in
	   (eqs, cnstrnts, s', keqs)
       | Binrel.Singleton(q) ->
	   const k q acc
       | Binrel.Overlap(cd) ->
	   let s' = update k cd s in
	   (eqs, cnstrnts, s', keqs)
   with
       Not_found ->
	 let s' = update k c s in
	 (eqs, cnstrnts, s', keqs)

(*s Add a symbolic constraint ['(c - cnstrnt(a))'] to [k]. *)

and symbolic k (c, a) ((eqs, cnstrnts, s, keqs) as acc) =
  Trace.msg 0 "Diff(c)" "to do" Pretty.string;
  try
    let (d, l) = apply s k in
    let e = Cnstrnt.subtract c (cnstrnt s a) in
    match Cnstrnt.cmp d e with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | (Binrel.Sub | Binrel.Same) ->
	  let s' = {s with find = Map.add k (d, (c, a) :: l) s.find;
		           use = Use.add Arith.fold k a s.use}
	  in
	  (eqs, cnstrnts, s', keqs)
      | Binrel.Super ->
	  let s' = {s with find = Map.add k (e, (c, a) :: l) s.find;
		           use = Use.add Arith.fold k a s.use}
	  in
	  (eqs, cnstrnts, s', keqs)
      | Binrel.Singleton(q) ->
	  const k q acc
      | Binrel.Overlap(cd) ->
	  let s' = {s with find = Map.add k (cd, (c, a) :: l) s.find;
		            use = Use.add Arith.fold k a s.use}
	  in
	  (eqs, cnstrnts, s', keqs)
  with
      Not_found ->
	acc

and const k q (eqs, cnstrnts, s, keqs) = 
  let n = Arith.mk_num q in
  let s' = restrict k s in
  let keqs' = (k, n) :: keqs in
  (eqs, cnstrnts, s', keqs')


and equality (k, a) ((eqs, cnstrnts, s, keqs) as acc) =
  assert(is_slack k);
  if is_slack a then
    merge k a acc
  else 
    try
      let (c,l) = apply s k in
      let d = cnstrnt s a in
      match Cnstrnt.cmp c d with
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| Binrel.Sub | Binrel.Same ->
	    let cnstrnts' = (a, c) :: cnstrnts in
	    replace k a (eqs, cnstrnts', s, keqs)
	| Binrel.Super ->
	    let s' = {s with find = Map.add k (d,l) s.find} in
	    replace k a (eqs, cnstrnts, s', keqs)
	| Binrel.Overlap(cd) ->
	    let cnstrnts' = (a, cd) :: cnstrnts in
	    let s' = {s with find = Map.add k (cd,l) s.find} in
	    replace k a (eqs, cnstrnts', s', keqs)
	| Binrel.Singleton(q) ->
	    try
	      match Arith.solve is_slack (a, Arith.mk_num q) with
		| None -> 
		    const k q acc
		| Some(k',a') ->
		    let eqs' = (k', a') :: eqs in
		    const k q (eqs', cnstrnts, s, keqs)
	    with
		Exc.Unsolved ->
		  Trace.msg 1 "Solve(c)" "unsolved" Pretty.string;
		  const k q acc
    with
	Not_found ->
	  replace k a acc

and merge k1 k2 ((eqs, cnstrnts, s, keqs) as acc) =
  assert(is_slack k1 && is_slack k2);
  Trace.msg 4 "Merge(c)" (k1, k2) Term.pp_equal;
  let (k1,k2) = Term.orient (k1, k2) in           (*s Eliminate [k2]. *)
  try
    let (c1, l1)= apply s k1 in
    try
      let (c2, l2) = apply s k2 in
      match Cnstrnt.cmp c1 c2 with
       | Binrel.Disjoint ->
	   raise Exc.Inconsistent
       | (Binrel.Super | Binrel.Same) ->
	   let keqs' = (k1, k2) :: keqs in
	   let s' = {s with find = Map.add k2 (c2, l1 @ l2) (Map.remove k1 s.find)} in
	   (eqs, cnstrnts, s', keqs')
       | Binrel.Sub ->
	   let keqs' = (k1, k2) :: keqs in
	   let s' = {s with find = Map.add k2 (c1, l1 @ l2) (Map.remove k1 s.find)} in
	   (eqs, cnstrnts, s', keqs')
       | Binrel.Singleton(q) ->
	   let s' = restrict k1 (restrict k2 s) in
	   let n = Arith.mk_num q in
	   let eqs' = (k1, n) :: (k2, n) :: eqs in
	   (eqs', cnstrnts, s', keqs)
       | Binrel.Overlap(c) ->
	   let keqs' = (k1, k2) :: keqs in
	   let s' = {s with find = Map.add k2 (c, l1 @ l2) (Map.remove k1 s.find)} in
	   (eqs, cnstrnts, s', keqs')  
    with
	Not_found ->
	  let keqs' = (k1, k2) :: keqs in
	  (eqs, cnstrnts, s, keqs')
  with
      Not_found ->
	let keqs' = (k1, k2) :: keqs in
	(eqs, cnstrnts, s, keqs')


(*s Replace [k] with [a] in symbolic constraints;
 may generate new equalities. The resulting state
 does therefor not contain [k] in symbolic constraints
 anymore. *)

and replace k a ((_,_,s,_) as acc) =
  Set.fold
    (fun x ((_,_,s,_) as acc) ->
       try 
	 replace1 x (apply s x) k a acc
       with
	   Not_found -> acc)
    (use s k)
    acc

and replace1 x (c,l) k a ((eqs, cnstrnts, s, keqs) as acc) =
  let (c',l',use') = instantiate x (c,l,s.use) k a s in
  let s' = {s with find = Map.add x (c',l') s.find; use = use'} in
  match Cnstrnt.status c' with
    | Status.Empty ->
	raise Exc.Inconsistent
    | Status.Singleton(q) ->
	const x q (eqs, cnstrnts, s', keqs)
    | _ ->
	(eqs, cnstrnts, s', keqs)

and instantiate x (c,l,use) k a s =  (* instantiate [k] with [a] in [l]. *)
  List.fold_right                    (* update [c] accordingly. *)
    (fun (d,b) (c, l, use) ->
       if Term.subterm k b then
	 let b' = Arith.replace k a b in
	 let c' = Cnstrnt.inter c (meaning1 s (d,b')) in
	 let use' = Use.add Arith.fold x b' (Use.remove Arith.fold x b use) in
	 (c', (d, b') :: l, use')
       else 
	 (c, (d, b) :: l, use))
    l 
    (c, [], use)
	   
	   
(*s Propagate an equality ['k = a'] between a slack
 parameter [k] and a combination [a] of slacks. *)

let rec propagate (k, a) s =
  Trace.msg 6 "Prop(c)" (k, a) pp_equal;
  process ([(k,a)], [], s, [])





	
