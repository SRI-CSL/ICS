
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


(*s Canonical variables module [s]. *)

let v s = V.find s.p.Partition.v

(*s Constraint of [a] in [s]. *)

let rec cnstrnt s a = 
  let ctxt = C.apply (c_of s) in
  match a with  
    | Var _ -> 
	ctxt (v s a)
    | App(Arith(op), xl) ->
	Arith.cnstrnt ctxt a
    | App(Uninterp(f), xl) when Name.eq f Name.mult ->
	Cnstrnt.multl (List.map (cnstrnt s) xl)
    | App(Uninterp(f), [_]) when Name.eq f Name.floor ->
	Cnstrnt.mk_int
    | App(Uninterp(f), [_]) when Name.eq f Name.sin ->
	Cnstrnt.mk_cc Dom.Real (Q.of_int (-1)) (Q.of_int 1)
    | App(Uninterp(f), [_]) when Name.eq f Name.unsigned ->
	Cnstrnt.mk_nat
    | App(Uninterp(f), [x; _])
	when Name.eq f Name.expt && Arith.is_q (Q.of_int 2) x ->
	let c = Cnstrnt.mk_ge Dom.Real Q.zero in
	  c
(*
	(try
	  let d = cnstrnt s x in
	    Cnstrnt.inter c d
	with
	    Not_found -> c)
 *)
    | _ ->
	raise Not_found
	 

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

let solutions e s = 
  match e with
    | U -> s.u
    | A -> s.a
    | T -> s.t
    | BV -> s.bv

(*s Parameterized operations. *)

let inv e s = Solution.inv (solutions e s)
let find e s = Solution.find (solutions e s)
let use e s = Solution.use (solutions e s)


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

let rec sigma s f =
  match f with
    | Arith(op) -> Arith.sigma op
    | Tuple(op) -> Tuple.sigma op
    | Bv(op) -> Bitvector.sigma op
    | Uninterp _ -> mk_app f


(*s Basic functions for updating the [u] solution set. *)

let umap f a = 
  match a with
    | Var _ -> f(a)
    | App(g, l) ->  mk_app g (mapl f l)

let u_fuse (x, a) s =
  assert (is_var x);
  let (p', u') =  Solution.fuse umap (s.p, s.u) [(x, a)] in
  {s with p = p'; u = u'}

let u_compose (x, a) s =
  assert (is_var x);
  let (p', u') =  Solution.compose umap (s.p, s.u) [(x, a)] in
  {s with p = p'; u = u'}

  
(*s Abstracting a term [a] in theory [i]
 by introducing a new name for it. *)

let rec extend s a =
  assert(not(is_var a));
  let x = Term.mk_fresh_var (Name.of_string "v") None in 
    Trace.msg "tac" "Extend" (x, a) Term.pp_equal;
    let f = sym_of a in
      match theory_of f with
	| U ->
	    let s' = 
	      (match f, args_of a with      (* Eliminate division using: *)
		 | Uninterp(div), [y; z]    (* [x = y / z  <=> y = x * z *] *)
		     when Name.eq Name.div div ->
		     let (x, z) = Term.orient (x, z) in
		     let a' =  mk_app Sym.mult [x; z] in
		       u_compose (y, a') s
		 | _ ->
		     {s with u = Solution.union (x, a) s.u})
	    in
	      (x, s')
	| T ->
	    (x, {s with t = Solution.union (x, a) s.t})
	| BV ->
	    (x, {s with bv = Solution.union (x, a) s.bv})
	| A ->
	    try
	      match arith_solve s (x, a) with
		| None -> assert false
		| Some(x', b') -> 
		    let (p', a') = 
		      assert(is_var x');
		      Solution.compose Arith.map (s.p, s.a) [(x', b')] 
		    in
		      (x, {s with p = p'; a = a'})
	      with
		  Exc.Unsolved ->
		    let a' = Solution.union (x, a) s.a in
		      (x, {s with a = a'})

and arith_solve s (a, b) = 
  let is_var_on_rhs x = 
    is_var x && 
    not(Set.is_empty (Solution.use s.a x)) 
  in
  try
    Arith.solve_for is_var_on_rhs (a, b)
  with
      Exc.Unsolved -> 
	Arith.solve_for is_var (a, b)


(*s Adding equalities/disequalities/constraints to partition. *)

let merge e s =
  Trace.msg "tac" "Merge" e Fact.pp_equal;
  {s with p = Partition.merge e s.p}

let add c s =
  Trace.msg "tac" "Add" c Fact.pp_cnstrnt; 
  {s with p = Partition.add c s.p}

let diseq d s = 
  Trace.msg "tac" "Diseq" d Fact.pp_diseq;
  {s with p = Partition.diseq d s.p}


(*s Close. *)

let maxclose = ref 10 (* no bound given (as long as there is no overflow) *)

let rec close s =
  (repeat 0 close1 &&& normalize) s

and close1 n s =
  Trace.msg "tac" "Close" n Pretty.number;
  let (vfocus, dfocus, cfocus) = Partition.changed s.p in
    (deduce_from_a (Solution.changed s.a) &&&
     (*  deduce_from_u (Solution.changed s.u) &&& *)
     deduce_from_v vfocus &&&
     deduce_from_d dfocus &&&
     deduce_from_c cfocus)
    s

and repeat loops f s =
  let t = f loops s in
    if is_confluent t then 
      t
    else if loops < !maxclose then
      repeat (loops + 1) f t
    else 
      begin
	Format.eprintf "\nWarning: Upper bound reached.@.";
	t
      end

and is_confluent s = 
  let (vfocus, dfocus, cfocus) = Partition.changed s.p in
  Set.is_empty vfocus &&
  dfocus = [] &&
  Set.is_empty cfocus &&
  Set.is_empty (Solution.changed s.a)


and (&&&) f g s = g (f s)

and deduce_from_v focus s =
  Set.fold 
    (fun x -> 
       let e = (x, V.find s.p.Partition.v x) in 
       Trace.msg "tac" "Deduce(v)" e Term.pp_equal;
       u_prop e &&&
       arith_prop e &&&
       tuple_prop e &&&
       bv_prop e &&&
       array_prop e)
    focus
    {s with p = {s.p with Partition.v = V.reset (v_of s)}}

and u_prop (x, y) s =
  u_fuse (x, y) s

and tuple_prop (x, y) s =
  Trace.msg "t" "Prop" (x, y) Term.pp_equal;
  let fuse (x, y) s = 
    Trace.msg "t" "Fuse" (x, y) Term.pp_equal;
    assert(is_var x);
    let (p', t') = Solution.compose Tuple.map (s.p, s.t) [(x, y)] in
    {s with t = t'; p = p'}
  in
  let compose (a, b) s =
    Trace.msg "t" "Compose" (a, b) Term.pp_equal;
    let (p', t') = Solution.compose Arith.map (s.p, s.t) (Tuple.solve (a, b)) in
    {s with t = t'; p = p'}
  in
  if not(Set.is_empty (Solution.use s.t x)) then   (* [x] occurs on rhs. *)
    fuse (x, Solution.find s.t y) s
  else
    try
      let a = Solution.apply s.t x in
      try
	let b = Solution.apply s.t y in
	if Term.eq a b then s else compose (a, b) s
      with
	    Not_found ->
	      let s' = {s with t = (Solution.restrict x s.t)} in
	      compose (y, a) s'
    with
	Not_found -> s

and bv_prop (x, y) s = 
  Trace.msg "bv" "Prop" (x, y) Term.pp_equal;
  let fuse (x, y) s =  
    Trace.msg "bv" "Fuse" (x, y) Term.pp_equal;
    let (p', bv') = Solution.compose Bitvector.map (s.p, s.bv) [(x, y)] in
    {s with bv = bv'; p = p'}
  in
  let compose (a, b) s =   
    Trace.msg "bv" "Compose" (a, b) Term.pp_equal;
    let (p', bv') = Solution.compose Arith.map (s.p, s.bv) (Bitvector.solve (a, b)) in
    {s with bv = bv'; p = p'}
  in
  if not(Set.is_empty (Solution.use s.bv x)) then   (* [x] occurs on rhs. *)
    fuse (x, Solution.find s.bv y) s
  else
    try
      let a = Solution.apply s.bv x in
      try
	let b = Solution.apply s.bv y in
	if Term.eq a b then s else compose (a, b) s
      with
	    Not_found ->
	      let s' = {s with bv = (Solution.restrict x s.bv)} in
	      compose (y, a) s'
    with
	Not_found -> s

and arith_prop (x, y) s =
  Trace.msg "a" "Prop" (x, y) Term.pp_equal;
  if not(Set.is_empty (Solution.use s.a x)) then   (* [x] occurs on rhs. *)
    arith_fuse (x, Solution.find s.a y) s
  else
    try
      let a = Solution.apply s.a x in
      try
	let b = Solution.apply s.a y in 
	if Term.eq a b then s else arith_compose (a, b) s
      with
	  Not_found ->
	    let s' = {s with a = (Solution.restrict x s.a)} in
	    arith_compose (y, a) s'
    with
	Not_found -> s            (* [x] occurs neither on rhs nor on lhs. *)

and arith_fuse (x, y) s =  
 Trace.msg "a" "Fuse" (x, y) Term.pp_equal;
  assert(is_var x);
  let (p', a') = Solution.compose Arith.map (s.p, s.a) [(x, y)] in
    {s with a = a'; p = p'}

and arith_compose (a, b) s = 
  Trace.msg "a" "Compose" (a, b) Term.pp_equal;
  try
    match arith_solve s (a, b) with
      | None -> s
      | Some(x', b') ->  
	  Trace.msg "a" "Solve" (x', b') Term.pp_equal;
	  assert(is_var x');
	  let (p', a') = Solution.compose Arith.map (s.p, s.a) [x', b'] in
	  {s with a = a'; p = p'}
  with
      Exc.Unsolved ->  
	Trace.msg "a" "Abstract" (a, b) Term.pp_equal;
	let (x, sa') = Solution.name (a, s.a) in
	let (y, sa'') = Solution.name (b, sa') in
	{s with a = sa''}



(*s From the equality [x = y] and the facts
 [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
 and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
 that [z1 = k2]. *)

and array_prop (x, y) s =
  select_update_prop (x, y)
    (select_update_prop (y, x) s)

and select_update_prop (x, y) s =
  Set.fold
    (fun z1 s1 -> 
       let v1 = s1.p.Partition.v and u1 = s1.u in
       try
	 let (upd1, j1) = d_select (Solution.apply u1 z1) in
	 if not(V.is_equal v1 y j1) then
	   s1
	 else 
	   V.fold v1
	     (fun z2 s2  -> 
		let v2 = s2.p.Partition.v and u2 = s2.u in
		try
		  let (a2, i2, k2) = d_update (Solution.apply u2 z2) in
		  if V.is_equal v2 x i2 then
		    let e' = Fact.mk_equal (V.find v2 z1) (V.find v2 k2) None in
		    merge e' s2
		  else
		    s2
		with
		    Not_found -> s2)
	     (V.find v1 upd1)
	     s1
       with
	   Not_found -> s1)
    (Solution.use s.u x)
    s

and compactify = ref true

and normalize s =
  if !compactify then
    let xs = Partition.removable s.p in
      Trace.msg "tac" "Normalize" (Set.elements xs) (Pretty.set Term.pp);
      let xs' = Set.filter (fun x -> not (Solution.mem s.u x)) xs in
      let p' = Partition.restrict xs' s.p in
	{s with p = p'}
  else
    s

and deduce_from_d focus s =
   List.fold_right 
     (fun d ->
	Trace.msg "tac" "Deduce(d)" d Fact.pp_diseq;
	cnstrnt_diseq d &&&
        array_diseq d)
     focus
     {s with p = {s.p with Partition.d = D.reset (d_of s)}}

and cnstrnt_diseq d s =
  let c = c_of s in
  let c' = C.diseq d c in
  if C.eq c c' then s else 
    let p' = {s.p with Partition.c = c'} in
      {s with p = p'}

(*s Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [z1 = select(u,j')], [z2 = update(a,i',x)],
 [i = i'], [j = j'], [u = z2], it follows that
 [z1 = z3], where [z3 = select(a,j)]. *)

and array_diseq d s =
  Trace.msg "tac" "Array diseq" d Fact.pp_diseq;
  let (i,j,_) = Fact.d_diseq d in
  select_update_diseq (i, j)
    (select_update_diseq (j, i) s)

and select_update_diseq (i, j) s =
  Set.fold
   (fun z1 s1 -> 
      Trace.msg "foo" "z1" z1 Term.pp;
      Trace.msg "foo" "s1" s1.u Solution.pp;
      try
	let (u, j') = d_select (Solution.apply s1.u z1) in 
        Trace.msg "foo" "u" u Term.pp;
        Trace.msg "foo" "j'" j' Term.pp;
	if not(V.is_equal (v_of s1) j j') then
	  s1
	else 
	  V.fold (v_of s1)
	    (fun z2 s2  ->
	       Trace.msg "foo" "z2" z2 Term.pp;
	       try
		 let (a,i',x) = d_update (Solution.apply s2.u z2) in
		 if not(V.is_equal (v_of s2) i i') then
		   s2
		 else 
		   let u3 = Term.mk_app (Uninterp(Name.of_string "select")) [a;j] in
		   try
		     let z3 = Solution.inv s2.u u3 in 
		     let e' = Fact.mk_equal (V.find (v_of s2) z1) (V.find (v_of s2) z3) None in
		     merge e' s2
		   with
		       Not_found ->
			 let z3 = Term.mk_fresh_var (Name.of_string "v") None in
			 let s3 = {s2 with u = Solution.union (z3, u3) s2.u} in
			 let e3 = Fact.mk_equal (V.find (v_of s3) z1) (V.find (v_of s3) z3) None in
			 merge e3 s3
	       with
		   Not_found -> s2)
	    (V.find (v_of s1) u)
	    s1
      with
	  Not_found -> s1)
   (Solution.use s.u j)
   s

and d_update a =
  match a with 
    | App(Uninterp(op), [b; i; x]) 
        when Name.eq op Name.update ->
	(b, i, x)
    | _ ->
	raise Not_found

and d_select a =
  match a with 
    | App(Uninterp(op), [a; i]) 
	when Name.eq op Name.select ->
	(a, i)
    | _ ->
	raise Not_found

and deduce_from_u focus s =
  Trace.msg "tac" "Deduce(u)" (Set.elements focus) (Pretty.set Term.pp);
  let s = {s with a = Solution.reset s.u} in
  Set.fold
    (fun x acc ->
       try
	 let a = Solution.apply s.u x in
	 deduce_cnstrnt (x, a) s 
       with
	   Not_found -> acc)
    focus
    s

(*s Deduce new facts from changes in the linear arithmetic part. *)

and deduce_from_a focus s = 
  Trace.msg "tac" "Deduce(a)" (Set.elements focus) (Pretty.set Term.pp);
  let s = {s with a = Solution.reset s.a} in
  Set.fold
    (fun x acc ->
       let y =  V.find (v_of s) x in
	 try
	   let a = Solution.apply s.a y in
	     deduce_cnstrnt (y, a) 
	       (deduce_linearize (y, a) acc)
	 with
	     Not_found -> acc)
    focus
    s
 

and deduce_cnstrnt (x, a) s =
  let c = s.p.Partition.c in
  let c' = C.deduce (x, a) s.p.Partition.c in
  if C.eq c c' then s else
    let p' = {s.p with Partition.c = c'} in
      {s with p = p'}
  

and deduce_linearize (x, a) s =
  match Arith.d_num a with
    | None -> s
    | Some(q) ->
	Set.fold 
	   (fun y s ->
	      match Solution.find s.u y with
		| App(Uninterp(op), zl) when Name.eq op Name.mult ->
		    (match zl with
		       | [z] ->
			   arith_compose (y, Arith.mk_num q) s
		       | [z1; z2] when Term.eq z1 x ->    (* [y = q * z2] *)
			   arith_compose (y, Arith.mk_multq q z2) s
		       | [z1; z2] when Term.eq z2 x ->    (* [y = z1 * q] *)
			   arith_compose (y, Arith.mk_multq q z1) s
		       | _ ->
			   s)  (* to be done. *)
		| _ ->
		    s)
	(Solution.use s.u x)
	s


(*s Deduce new facts from changes in the constraint part. *)

and deduce_from_c focus s =
  let s = {s with p = {s.p with Partition.c = C.reset (c_of s)}} in
  Set.fold
    (fun x s -> 
       assert(is_var x);
       Trace.msg "tac" "Deduce(c)" x Term.pp;
       try
	 let i = C.apply (c_of s) (V.find (v_of s) x) in
	 singleton_infer (x, i)
            (diseq_infer (x, i)
                (equal_infer (x, i) s))
       with
	   Not_found -> s)
    focus
    s

and equal_infer (x, i) s =
  try
    let b = Solution.apply s.a x in
    let c' = C.deduce (x, b) s.p.Partition.c in
    let p' = {s.p with Partition.c = c'} in
    {s with p = p'}
  with
      Not_found ->
	Set.fold
	  (fun y s ->
	     try
	       let b = Solution.apply s.a y in
	       let c' = C.deduce (y, b) s.p.Partition.c in
	       let p' = {s.p with Partition.c = c'} in
	       {s with p = p'}
	     with
		 Not_found -> s)
	  (Solution.use s.a x)
	  s
	       
and diseq_infer (x, i) s =
  match Cnstrnt.d_singleton i with
    | None -> s
    | Some(q) ->
	let j = Cnstrnt.mk_diseq q in
	Set.fold
	  (fun y s ->
	     let c' = C.add (Fact.mk_cnstrnt y j None) s.p.Partition.c in
	     let p' = {s.p with Partition.c = c'} in
	     {s with p = p'})
	  (D.deq s.p.Partition.d x)
	  s
  
and singleton_infer (x, i) s =
  match Cnstrnt.d_singleton i with
    | None ->
	s
    | Some(q) ->
	assert(is_var x);
	let (p', a') = Solution.compose Arith.map (s.p, s.a) [x, Arith.mk_num q] in
	{s with a = a'; p = p'}

(*s List all constraints with finite extension. *)

let split s  =
  Solution.fold
    (fun _ b acc1 ->
       try
	 let (upd1, j1) = d_select b in
	 V.fold s.p.Partition.v
	   (fun upd2 acc2 ->
	      try
		let (_,i2,_) = d_update (Solution.apply s.u upd2) in
		match is_equal s i2 j1 with
		  | X -> Atom.Set.add (Atom.mk_equal i2 j1) acc2
		  | Yes -> acc2
		  | No -> acc2
	      with
		  Not_found -> acc1)
	   upd1
	   acc1
       with
	   Not_found -> acc1)
    s.u
    (C.split s.p.Partition.c)


(*s Compression. *)

let compress s = s
