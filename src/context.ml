
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
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
open Three
open Mpa
open Sym
(*i*)

(*s Decision procedure state. *)

type t = {
  ctxt : Atom.Set.t;    (* Current context. *)
  p : Partition.t;      (* Variable partitioning. *)
  u : Solution.t;       (* Congruence closure data structure. *)
  a : Solution.t;       (* Arithmetic equality context. *)
  t : Solution.t;       (* Tuple equality context. *)
  bv : Solution.t;      (* Bitvector equality context. *)
  labels : Term.Set.t
}

type index =
  | Partition of Partition.index
  | Theory of Sym.theories


let empty = {
  ctxt = Atom.Set.empty;
  p = Partition.empty;
  u = Solution.empty;
  a = Solution.empty;
  t = Solution.empty;
  bv = Solution.empty;
  labels = Term.Set.empty
}


(*s Accessors. *)

let v_of s = s.p.Partition.v
let d_of s = s.p.Partition.d
let c_of s = s.p.Partition.c


(*s Equality test. *)

let eq s t = 
 Partition.eq s.p t.p &&
 Solution.eq s.u t.u &&
 Solution.eq s.a t.a &&
 Solution.eq s.t t.t &&
 Solution.eq s.bv t.bv


(*s Updating state. *)

let update i s eqs =
  match i with
    | A -> if eqs == s.a then s else {s with a = eqs}
    | T -> if eqs == s.t then s else {s with t = eqs}
    | BV -> if eqs == s.bv then s else {s with bv = eqs}
    | U -> if eqs == s.u then s else {s with u = eqs}

let install i s (p, e) =
  match i with
    | A -> if p == s.p && e == s.a then s else {s with a = e; p = p}
    | T -> if p == s.p && e == s.t then s else {s with t = e; p = p}
    | BV -> if p == s.p && e == s.bv then s else {s with bv = e; p = p}
    | U -> if p == s.p && e == s.u then s else {s with u = e; p = p}

let update_p s p =
  if s.p == p then s else {s with p = p}

let update_v s v = update_p s (Partition.update_v s.p v)
let update_d s d = update_p s (Partition.update_d s.p d)
let update_c s c = update_p s (Partition.update_c s.p c)


(*s Canonical variables module [s]. *)

let v s = V.find s.p.Partition.v

(*s Constraint of [a] in [s]. *)

let cnstrnt s = C.of_term (v_of s, c_of s)


(*s All disequalities of some variable [x]. *)

let deq s = D.deq s.p.Partition.d


(*s Choosing a variable. *)

let choose s = V.choose s.p.Partition.v


(*s Pretty-printing. *)
  
let pp fmt s =
  let pps name sl =   
    if not(Solution.is_empty sl) then
      begin
	Format.fprintf fmt "\n%s:" name;
	Solution.pp fmt sl
      end
  in
  Partition.pp fmt s.p;
  pps "u" s.u;
  pps "a" s.a;
  pps "t" s.t;
  pps "bv" s.bv


(*s Return solution sets. *)

let solutions i s = 
  match i with
    | U -> s.u
    | A -> s.a
    | T -> s.t
    | BV -> s.bv

(*s Parameterized operations on solution sets. *)

let inv i s = Solution.inv (solutions i s)

let apply i s = Solution.apply (solutions i s)

let find i s = Solution.find (solutions i s)

let use i s = Solution.use (solutions i s)

let restrict i x s =
  update i s (Solution.restrict x (solutions i s))

let name i s b =
  Solution.name (b, solutions i s)


(*s Simplifiers. *)

let rec map i =
  match i with
    | A -> Arith.map
    | T -> Tuple.map
    | BV -> Bitvector.map
    | U -> mapu

and mapu ctxt a =
  match a with
    | Var _ -> ctxt(a)
    | App(f, l) ->  
	let l' = mapl ctxt l in
	  if l == l' then a else 
	    mk_app f l'



(*s Variable partitioning. *)

let partition s = (s.p.Partition.v, s.p.Partition.d)

let is_int s = Partition.is_int s.p

let rec is_equal s x y =
  match Partition.is_equal s.p x y with
    | Three.X when is_diseq_constants s x y -> Three.No
    | res -> res

and is_diseq_constants s x y =   (* Disequality of arithmetic constants *)
  try                            (* already tested via constraints. *)
    let a = Solution.apply s.bv x in
    is_const a &&
    let b = Solution.apply s.bv y in
    is_const b &&
    not(Term.eq a b)
  with
      Not_found -> false

(*s [sigma]-normal forms. *)

let sigma s f =
  match f with
    | Arith(op) -> Arith.sigma op
    | Tuple(op) -> Tuple.sigma op
    | Bv(op) -> Bitvector.sigma op
    | _ -> mk_app f


(*s Tracing. *)

let trace (i, str, arg, pp) =
  let name = name_of_theory i in
  Trace.msg name ("Compose(" ^ name ^ ")") arg pp

(* Component-wise solver. *)

let solve e s (a, b) =
  match e with
    | U -> [Term.orient(a, b)]
    | T -> Tuple.solve (a, b)
    | BV -> Bitvector.solve (a, b)
    | A -> 
	let is_var_on_rhs x = 
	  is_var x &&  
	  not(Set.is_empty (Solution.use s.a x)) 
	in
	  try
	    match Arith.solve_for is_var_on_rhs (a, b) with
	      | Some(x', b') -> [(x',b')]
	      | None -> []
	    with
		Exc.Unsolved ->
		  match Arith.solve_for is_var (a, b) with
		    | Some(x', b') -> [(x', b')] 
		    | None -> []
 

(*s Propagation of equalities in theory-specific solution sets  *)

let rec propagate i e s =
  trace(i, "Prop", e, Fact.pp_equal);
  let (x, y, _) = Fact.d_equal e in
  if not(Set.is_empty (use i s x)) then      (* [x] occurs on rhs. *)
    fuse i (Fact.mk_equal x (find i s y) None) s
  else
    try
      let a = apply i s x in
      try
	let b = apply i s y in 
	let e' = Fact.mk_equal a b None in
	if Term.eq a b then s else compose i e' s
      with
	  Not_found ->
	    let e' = Fact.mk_equal y a None in
	      compose i e' (restrict i x s)
    with
	Not_found -> s       (* [x] occurs neither on rhs nor on lhs. *)

and fuse i e s =  
  trace(i, "Fuse", e, Fact.pp_equal);
  let (x, y, _) = Fact.d_equal e in
  install i s 
    (Solution.fuse (map i) (s.p, solutions i s) [(x, y)])

and compose i e s = 
  trace(i, "Compose", e, Fact.pp_equal);
  let (a, b, _) = Fact.d_equal e in
  try
    let sl = solve i s (a, b) in
    let (p', e') = Solution.compose (map i) (s.p, solutions i s) sl in
      install i s (p', e')
  with
      Exc.Unsolved -> 
	Format.eprintf "Warning: Incomplete Solver@.";
	abstract i (a, b) s

and abstract i (a, b) s =
  trace(i, "Abstract", (a, b), Term.pp_equal);
  let e = solutions i s in
  let (x, e') = Solution.name (a, e) in
  let (y, e'') = Solution.name (b, e') in
    update i s e''


(* Lookup terms on rhs of solution sets. *)
    
let lookup s a = 
  match a with
    | Var _ ->
	v s a
    | App(f, _) ->
	let i = theory_of f in
	  try v s (inv i s a) with Not_found -> a


(*s List all constraints with finite extension. *)

let split s  =
  Solution.fold
    (fun _ b acc1 ->
       match b with
	 | App(Builtin(Select), [upd1; j1]) ->
	     V.fold s.p.Partition.v
	     (fun upd2 acc2 ->
		try
		  (match Solution.apply s.u upd2 with
		     | App(Builtin(Update), [_; i2; _]) ->
			 (match is_equal s i2 j1 with
			    | X -> Atom.Set.add (Atom.mk_equal i2 j1) acc2
			    | _ -> acc2)
		     | _ -> 
			 acc1)
		with
		    Not_found -> acc1)
	     upd1 acc1
	 | _ -> acc1)
    s.u
    (C.split s.p.Partition.c)

