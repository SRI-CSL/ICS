
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



(*s Canonical variables module [s]. *)

let v s = V.find s.p.Partition.v

(*s Constraint of [a] in [s]. *)

let cnstrnt s = C.cnstrnt s.p.Partition.c

(*s All disequalities of some variable [x]. *)

let deq s = D.deq s.p.Partition.d


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
    | Theories.Uninterp -> s.u
    | Theories.Interp(Interp.A) -> s.a
    | Theories.Interp(Interp.T) -> s.t
    | Theories.Interp(Interp.BV) -> s.bv

(*s Parameterized operations. *)

let inv e s = Solution.inv (solutions e s)
let find e s = Solution.find (solutions e s)
let use e s = Solution.use (solutions e s)


(*s Variable partitioning. *)

let partition s = (s.p.Partition.v, s.p.Partition.d)

let is_int s = Partition.is_int s.p
let is_equal s = Partition.is_equal s.p



(*s Sigmatization. [sigma s f l] encodes the following 
 array axioms: [select(update(a,i,x),j)] reduces to [x] if [i = j] 
 and to  [select(a,j)] if [i <> j]. *)

exception Found of Term.t

let rec sigma s f l =
  match Sym.destruct f with
    | Sym.Interp(Sym.Arith(op)) -> Arith.sigma op l
    | Sym.Interp(Sym.Tuple(op)) -> Tuple.sigma op l
    | Sym.Interp(Sym.Bv(op)) -> Bitvector.sigma op l 
    | Sym.Uninterp _ ->
	let a = mk_app f l in
	try
	  sigma_select_update s a
	with
	    Not_found -> a

and sigma_select_update s a =     (* throws [Not_found] if no pattern matches. *)
  let (upd, j) = d_select a in       (* otherwise returns simplified term. *)
  try 
    V.iter s.p.Partition.v
      (fun upd' ->
	 try
	   let (a, i, x) = d_update (Solution.apply s.u upd') in
	   match Partition.is_equal s.p i j with
	     | Three.Yes -> raise (Found x)
	     | Three.No -> raise (Found(mk_app Sym.mk_select [a; j]))
	     | Three.X -> ()
	 with
	     Not_found -> ())
      (v s upd);
    raise Not_found
  with
      Found(b) -> b

  
(*s Abstracting a term [a] in theory [i]
 by introducing a new name for it. *)

let rec extend s a =
  let x = Term.mk_fresh_var (Name.of_string "v") None in 
  Trace.msg "context" "Extend" (x, a) Term.pp_equal;
  match Theories.index a with
    | Theories.Uninterp ->
	(x, {s with u = Solution.union (x, a) s.u})
    | Theories.Interp(Interp.T) ->
	(x, {s with t = Solution.union (x, a) s.t})
    | Theories.Interp(Interp.BV) ->
	(x, {s with bv = Solution.union (x, a) s.bv})
    | Theories.Interp(Interp.A) ->
	try
	  match arith_solve s (x, a) with
	    | None -> assert false
	    | Some(x',b') -> 
		let (p', a') = Solution.compose Arith.map (s.p, s.a) [(x', b')] in
		(x, {s with p = p'; a = a'})
	with
	    Exc.Unsolved ->
	      (x, {s with a = Solution.union (x, a) s.a})

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

let merge e s = {s with p = Partition.merge e s.p}

let add c s = {s with p = Partition.add c s.p}

let diseq d s = {s with p = Partition.diseq d s.p}


(*s Close. *)

let rec close s =
  (repeat close1 &&& normalize) s

and close1 s =
  Trace.msg "context" "Close" () (fun _ _ -> ());
  let vfocus = V.changed s.p.Partition.v
  and dfocus = D.changed s.p.Partition.d
  and afocus = Solution.changed s.a
  and cfocus = C.changed s.p.Partition.c
  in
  Trace.msg "foo" "Afocus" (Set.elements afocus) (Pretty.set Term.pp);
  (deduce afocus &&&
   prop vfocus &&&
   diseqs dfocus &&&
   infer cfocus)
  s

and repeat f s =
  let t = f s in
  if is_confluent t then t else repeat f t

and is_confluent s = 
  Set.is_empty (V.changed s.p.Partition.v) &&
  Set.is_empty (Solution.changed s.a) &&
  D.changed s.p.Partition.d = [] &&
  Set.is_empty (C.changed s.p.Partition.c)


and (&&&) f g s = g (f s)

and prop focus s =
  Set.fold 
    (fun x -> 
       let e = (x, V.find s.p.Partition.v x) in 
       Trace.msg "context" "Prop" e Term.pp_equal;
       u_prop e &&&
       arith_prop e &&&
       tuple_prop e &&&
       bv_prop e &&&
       array_prop e)
    focus
    {s with p = {s.p with Partition.v = V.reset (v_of s)}}

and u_prop (x, y) s =
  let rec map f a =
    if is_var a then
      f(a)
    else 
      let g, l = Term.destruct a in
      Term.mk_app g (Term.mapl f l)
  in
  let (p', u') =  Solution.fuse map (s.p, s.u) [(x, y)] in
  {s with p = p'; u = u'}
  

and tuple_prop (x, y) s = 
  let a = norm (s.p, s.t) x in
  let b = norm (s.p, s.t) y in 
   if Term.eq a b then
     s
   else 
     let r = Tuple.solve (a, b) in
     let (p', t') = Solution.compose Tuple.map (s.p, s.t) r in
     {s with p = p'; t = t'}

and norm (p, e) a = 
  try 
    Solution.apply e a 
  with 
      Not_found -> V.find p.Partition.v a

and bv_prop (x, y) s =
  let a = Solution.find s.bv x in
  let b = Solution.find s.bv y in 
  if Term.eq a b then
    s
  else 
    let r = Bitvector.solve (a, b) in
    let (p', bv') = Solution.compose Bitvector.map (s.p, s.bv) r in
    {s with p = p'; bv = bv'}

and arith_prop (x, y) s =
  Trace.msg "a" "Prop" (x, y) Term.pp_equal;
  if not(Set.is_empty (Solution.use s.a x)) then   (* [x] occurs on rhs. *)
    arith_fuse (x, y) s
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
 let (p', a') = Solution.compose Arith.map (s.p, s.a) [(x, y)] in
 {s with a = a'; p = p'}

and arith_compose (a, b) s = 
  try
    match arith_solve s (a, b) with
      | None -> s
      | Some(x', b') ->  
	  Trace.msg "a" "Solve" (x', b') Term.pp_equal;
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
	 if not(V.eq v1 y j1) then
	   s1
	 else 
	   V.fold v1
	     (fun z2 s2  -> 
		let v2 = s2.p.Partition.v and u2 = s2.u in
		try
		  let (a2, i2, k2) = d_update (Solution.apply u2 z2) in
		  if V.eq v2 x i2 then
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

and normalize s = s

and diseqs focus s =
   List.fold_right 
     (fun d ->
	cnstrnt_diseq d &&&
        array_diseq d)
     focus
     {s with p = {s.p with Partition.d = D.reset (d_of s)}}

and cnstrnt_diseq d s =
  let c' = C.diseq d s.p.Partition.c in
  let p' = {s.p with Partition.c = c'} in
  {s with p = p'}

(*s Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [z1 = select(u,j')], [z2 = update(a,i',x)],
 [i = i'], [j = j'], [u = z2], it follows that
 [z1 = z3], where [z3 = select(a,j)]. *)

and array_diseq d s =
  Trace.msg "context" "Array diseq" d Fact.pp_diseq;
  let (i,j,_) = Fact.d_diseq d in
  select_update_diseq (i, j)
    (select_update_diseq (i, j) s)

and select_update_diseq (i, j) s =
  Set.fold
   (fun z1 s1 -> 
      try
	let (u, j') = d_select (Solution.apply s1.u z1) in
	if not(V.eq (v_of s1) j j') then
	  s1
	else 
	  V.fold (v_of s1)
	    (fun z2 s2  ->
	       try
		 let (a,i',x) = d_update (Solution.apply s2.u z2) in
		 if not(V.eq (v_of s2) i i') then
		   s2
		 else 
		   let u3 = Term.mk_app Sym.mk_select [a;j] in
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

and deduce focus s = 
  Trace.msg "context" "Deduce" (Set.elements focus) (Pretty.set Term.pp);
  let s = {s with a = Solution.reset s.a} in
  let c' = 
    Set.fold
      (fun x acc ->
	 let y = V.find (v_of s) x in
	 let e = (y, Solution.find s.a y) in
	 C.deduce e (acc, s.p.Partition.d))
      focus
      s.p.Partition.c
  in
  let p' = {s.p with Partition.c = c'} in
  {s with p = p'}

and infer focus s =
  let s = {s with p = {s.p with Partition.c = C.reset (c_of s)}} in
  s
(*
  Set.iter
    (fun x s -> 
       Trace.msg "context" "Consistent" x Term.pp;
       try
	 match Cnstrnt.d_singleton (C.apply (c_of s) (V.find (v_of s) x)) with
	   | None -> ()
	   | Some(q) ->
	       let (p', a') = Solution.compose Arith.map (s.p, s.a) [x, Arith.mk_num q] in
	       {s with a = a'; p = p'}
       with
	   Not_found -> ())
    focus;
  s
 *)

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
		match Partition.is_equal s.p i2 j1 with
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
