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

open Term
open Three
open Mpa
open Sym
open Context
open Th

type rule = Context.t -> Context.t

type 'a transform = Context.t * 'a -> Context.t * 'a


(** Variable abstract facts *)
let rec abstract_equal (s, e) =
  Trace.msg "rule" "Abstract" e Fact.pp_equal;
  let (a, b, _) = Fact.d_equal e in
  let (s', x') = abstract_toplevel_term (s, a) in
  let (s'', y') = abstract_toplevel_term (s', b) in
  let e' = Fact.mk_equal x' y' None in
    (s'', e')

and abstract_diseq (s, d) = 
  Trace.msg "rule" "Abstract" d Fact.pp_diseq;
  let (a, b, _) = Fact.d_diseq d in
  let (s', x') = abstract_toplevel_term (s, a) in
  let (s'', y') = abstract_toplevel_term (s', b) in
  let d' = Fact.mk_diseq x' y' None in
    (s'', d')
 
and abstract_cnstrnt (s, c) =  
  Trace.msg "rule" "Abstract" c Fact.pp_cnstrnt;
  let (a, i, _) = Fact.d_cnstrnt c in
  let (s', a') = abstract_toplevel_term (s, a) in
  let c' = Fact.mk_cnstrnt a' i None in
    (s', c')

and abstract_toplevel_term (s, a) =
  abstract_term u (s, a)

and abstract_term i (s, a) =
  match a with
    | Var _ -> 
	(s, a)
    | App(f, al) ->
	let j = Th.of_sym f in
	let (s', al') = abstract_args j (s, al) in
	let a' = if Term.eql al al' then a else sigma s f al' in
	  if i = u || i = arr || i <> j then
	    try
	      let x' = inv j s' a' in
	      (s', v s' x')
	    with 
		Not_found -> name j (s', a')
	  else 
	    (s', a')
	    
and abstract_args i (s, al) =
  match al with
    | [] -> 
	(s, [])
    | b :: bl ->
	let (s', bl') = abstract_args i (s, bl) in
	let (s'', b') = abstract_term i (s', b) in
	  if Term.eq b b' && bl == bl' then
	    (s'', al)
	  else 
	    (s'', b' :: bl')


(** Adding a variable equality to a partition. *)
let merge e s = 
  Trace.msg "rule" "Merge" e Fact.pp_equal;
  update (Partition.merge e (p_of s)) s

 
(** Adding a variable disequality to a partition. *)   
let diseq d s = 
  Trace.msg "rule" "Diseq" d Fact.pp_diseq;
  update (Partition.diseq d (p_of s)) s
    

(** Adding a constraint to a partition. *)   
let add c s = 
  Trace.msg "rule" "Add" c Fact.pp_cnstrnt;
  update (Partition.add c (p_of s)) s

let infer x i s =
  if Cnstrnt.eq i Cnstrnt.mk_real then s else 
    let c = Fact.mk_cnstrnt (v s x) i None in
      add c s
	  
(** Sequential composition *)
let (&&&) f g x = g (f x)

(** Applying a rule [f] for all equalities [x = a] in theory-specific solution set [s]
  of index [i] such that [x] is equivalent to some [y] in the changed set [ch]. *)
let fold i ch f =
  Set.fold 
    (fun x s -> 
       try 
	 f (equality i s (v s x)) s
       with 
	   Not_found -> s)
    ch

(** Applyfing rule [f e] for all equalities [x = y] such that [x] is in the changed
 set [ch] and accumulating the results. *)
let foldv ch f =
  Set.fold
    (fun x s -> 
       try f (V.equality (v_of s) x) s 
       with Not_found -> s)
    ch

(** Applying rule [f d] for all disequalities [d] in [s] of the form [x <> y] where [x]
  is in [ch], and accumulating the results. *)
let foldd ch f =
  Set.fold 
    (fun x s ->
       try
	 let dl = D.disequalities (d_of s) x in
	   List.fold_right f dl s
       with
	   Not_found -> s)
    ch

let foldc ch f = 
  Set.fold
   (fun x s -> 
      try f (Partition.cnstrnt (p_of s) x) s
      with Not_found -> s)
    ch


(** Propagating merged equalities into theory-specific solution sets. *)
let rec propagate_star ch = 
  foldv ch propagate

and propagate e =           
  (Context.propagate Th.la e &&&
   Context.propagate Th.p e &&&
   Context.propagate Th.bv e &&&
   Context.propagate Th.cop e &&&
   Context.fuse Th.u e &&&
   Context.fuse Th.pprod e &&&
   Context.fuse Th.app e &&&
   Context.fuse Th.arr e &&&
   Context.fuse Th.bvarith e)


(** Deduce new constraints from changes in the linear arithmetic part. *)
let rec arith_star (chla, chc) =
  fold Th.la chla arith_local &&&
  fold Th.la chla arith_cut &&&
  foldc chc arith_cnstrnt

and arith_cnstrnt c s =
  Trace.msg "rule" "Arith_cnstrnt" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
    match Cnstrnt.d_singleton i with
      | Some(q) ->
	  let n = Arith.mk_num q in
	  let e = Fact.mk_equal x n None in
	    Context.propagate Th.la e s
      | None ->
	  (try
	     let b = apply Th.la s x in
	     let e = Fact.mk_equal x b None in
	       arith_propagate e s
	   with
	       Not_found ->
		 (folduse Th.la x
		    (fun (y, b) s ->
		       let e = Fact.mk_equal y b None in
			 arith_propagate e s)
		    s))

(** Local propagation of constraints in an arithmetic equality. *)
and arith_local e s = 
  Trace.msg "rule" "Arith_local" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Arith(Num(q)), []) -> 
	  infer x (Cnstrnt.mk_singleton q) s
      | App(Arith(Multq(q)), [y]) -> 
	  assert(not(Q.is_zero q));
	  let i = try c s x with Not_found -> Cnstrnt.mk_real in
	  let j = try c s y with Not_found -> Cnstrnt.mk_real in
	    infer x (Cnstrnt.multq q j)
	      (infer y (Cnstrnt.multq (Q.inv q) i) s)
      | App(Arith(Add), ml) ->
	  let i = Arith.cnstrnt (c s) b in
	    arith_ints x ml (infer x i s)
      | _ ->
	  s 

and arith_cut e s =           
  Trace.msg "rule" "Arith_cut" e Fact.pp_equal;
  let (x, a, _) = Fact.d_equal e in             (* [x = a]. *)
    try
      let i = c s x in                          (* [x in i]. *)
	if Cnstrnt.is_unbounded i then s else 
	  let (q, z, a') = Arith.destructure a  in  (* [q * z + a' = a]. *)
	    folduse Th.la z
	      (fun (y, b) s ->                      (* [y = b] *)
		 if Term.eq x y then s else
		   try
		     Trace.msg "cut" "Candidate" (e, (y, b)) (Pretty.pair Fact.pp_equal Term.pp_equal);
		     let j = c s y in            (* [y in j]. *)
		       if Cnstrnt.is_unbounded j then s else 
			 let (p, z', b') = Arith.destructure b in
			   if not(Term.eq z z') then s else (* [p * z + b' = b]. *)
			     cut z (x, q, a', i) (y, p, b', j) s 
		   with
		       Not_found -> s)
	      s   
    with 
	Not_found -> s

(** Global propagation of constraints in pairs of arithmetic equalities.
  From 
  - [x = a], [y = b] in [la] with [a == q * z + a'], [b == p * z + b'], and 
  - [x in i], [y in j] in [c] 
  
  one concludes
  - [x in q/p ** (j -- cb') ++ ca'], where [ca'] is the constraint assoc. with [a']
  - [x in p/q ** (i -- ca') ++ cb']
  - [z in 1/(p*q) ** (k /\ l)] with [k == p**(i - ca')], [l == q**(j - cb')]
*)
and cut z (x, q, a', i) (y, p, b', j) s =
  Trace.msg "foo" "Cut" 
    (Arith.mk_add (Arith.mk_multq q z) a',
     Arith.mk_add (Arith.mk_multq p z) b')
    (Pretty.pair Term.pp Term.pp);
  Trace.msg "foo" "with" ((x, i), (y, j)) 
    (Pretty.pair Term.pp_in Term.pp_in);
  let pq = Q.mult p q
  and ca' = Arith.cnstrnt (c s) a'
  and cb' = Arith.cnstrnt (c s) b' in
  let cx' = 
    Cnstrnt.add (Cnstrnt.multq (Q.div q p) (Cnstrnt.subtract j cb')) ca'
  and cy' =
    Cnstrnt.add (Cnstrnt.multq (Q.div p q) (Cnstrnt.subtract i ca')) cb'
  and cz' = 	  
    let k = Cnstrnt.linear (p, i) (Q.minus p, ca') 
    and l = Cnstrnt.linear (q, j) (Q.minus q, cb') in
      match Cnstrnt.cmp k l with
	| Binrel.Disjoint ->
	    raise Exc.Inconsistent
	| (Binrel.Sub | Binrel.Same) ->
	    Cnstrnt.multq (Q.inv pq) k
	| Binrel.Super ->
	    Cnstrnt.multq (Q.inv pq) l
	| Binrel.Singleton(r) ->
	    Cnstrnt.mk_singleton (Q.div r pq)
	| Binrel.Overlap(kl) ->
	    Cnstrnt.multq (Q.inv pq) kl
  in
    infer x cx'
      (infer y cy'
	 (infer z cz' s))


(** Fourier-Motzkin elimination:
  - From [u = a], [v = b] in [Th.la], [u in i], [v in j] in [c],
  and [q * a = b] one may conclude 
  [u in 1/q * j] and [v in [q * i]
  - From [u = q * x + a'], [v = p * x + b'] in [Th.la]
  and [u in i], [v in j] in [c] one can conclude
  [p * a' - q * b' in p * i - q * j]
*)

and arith_fme u a s =                   (* [u = a]. *)
  Trace.msg "rule" "Arith_fme" (u, a) Term.pp_equal;
  try
    let i = c s u in               (* [u in i] *)
    let (q, x, a') = Arith.destructure a  in (* [q * x + a' = a]. *)
      folduse Th.la x
	(fun (v, b) s ->           (* [v = b] *)
	   if Term.eq u v then s else
	     try
	       let j = c s v in    (* [v in j]. *)
		 (try
		    let q = Arith.multiple (a, b) in     (* [q * a = b]. *)
		    let i' = Cnstrnt.multq (Q.inv q) j in
		    let j' = Cnstrnt.multq q i in
		      infer u i'        (* thus [u in 1/q * j] *)
			(infer v j' s)  (* and  [v in q * i] *)
		  with
		      Not_found ->      (* [q * y + b' = b] *) 
			let (p, y, b') = Arith.destructure b in
			  if not (Term.eq x y) ||
			    not (complementary (q, x, i) (p, y, j))
			  then s 
			  else 
			    let c =     (* [c = p * a' - q * b'] *)
			      Can.term s (Arith.mk_linear (p, a') (Q.minus q, b'))
			    and k =     (* [k = p * i - q * j] *)
			      Cnstrnt.subtract
				(Cnstrnt.multq p i)
				(Cnstrnt.multq q j)
			    in
			    let (c, k) = Arith.normalize (c, k) in
			      if Cnstrnt.eq k Cnstrnt.mk_real then s else 
				begin
				  Trace.msg "fme" "Generate" (c, k) Term.pp_in;
				  (match c with
				     | Var _ -> 
					 infer c k s
				     | App(Arith(Num(q)), []) ->
					 if Cnstrnt.mem q k then s else raise Exc.Inconsistent
				     | _ ->
					 if !fme then
					   let (s', x') = name Th.la (s, c) in
					     infer x' k s'
					 else
					   s)
				end)
	     with
		 Not_found -> s)
         s
  with
      Not_found -> s

and fme = ref true

(** Quick test for complementary constraints for which a Fourier-Motzkin
  cut might produce a new result. *)
and complementary (q, x, i) (p, x, j) =
  assert(not(Q.is_zero q) && not(Q.is_zero p));
  let eqsign = 
    (Q.gt q Q.zero && Q.gt p Q.zero) ||  
    (Q.lt q Q.zero && Q.lt p Q.zero)
  in
  match Cnstrnt.bounds i, Cnstrnt.bounds j with
    | Cnstrnt.Unbounded _, _ -> false
    | _, Cnstrnt.Unbounded _ -> false
    | Cnstrnt.Lower _, Cnstrnt.Lower _ when eqsign -> false
    | Cnstrnt.Upper _, Cnstrnt.Upper _ when eqsign -> false
    | _ -> true

and arith_ints x ml s = 
  Trace.msg "rule" "Arith_ints" (x, ml) (Pretty.pair Term.pp (Pretty.list Term.pp)); 
  let not_is_int m =
    not (Arith.is_int (c s) m)
  in
  let ml = Arith.mk_neg x :: ml in
    match List.filter not_is_int ml with
      | [] -> s
      | [Var _ as x] -> 
	  infer x Cnstrnt.mk_int s
      | [App(Arith(Num(q)), [])] -> 
	  if Q.is_integer q then s else raise Exc.Inconsistent
      | [App(Arith(Multq(q)), [x])] ->
	  if Q.is_integer q then infer x Cnstrnt.mk_int s else s
      | ql -> 
	  let a = Can.term s (Arith.mk_addl ql) in
	    match Arith.d_num a with
	      | Some(q) ->
		  if Q.is_integer q then s else raise Exc.Inconsistent
	      | _ ->
		  if is_var a then infer a Cnstrnt.mk_int s else s   
		                  (* do not introduce new names for now. *)


and arith_propagate e s =
  Trace.msg "rule" "Arith_propagate" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
  let ml = Arith.mk_neg x :: Arith.monomials b  in
  let extend (yl, zl) s =                      (* [yl + zl = 0]. *)
    let a = Can.term s (Arith.mk_addl yl) in   (* generate: [k = yl] and [k in -C(zl)] *)
      if Arith.is_num a || Arith.is_multq a then 
	s 
      else
	let i' = Cnstrnt.multq Q.negone (Arith.cnstrnt_of_addl (c s) zl) in
	  if is_var a then
	    infer a i' s
	  else
	    if !fme then
	      let (s', x') = name Th.la (s, a) in
		infer x' i' s'
	    else 
	      s

  (* Propagate constraints for [ml = 0] for each variable [x] in [b].
     Suppose [b] is of the form [pre + q * x + post'], then
     [x in -1/q * (j + k)] is derived, where [pre in j] and
     [post' in k]. Following should be optimized. *)
  and propagate ml = 
  let rec loop j post s = 
    match post with
      | [] -> s
      | m :: post' ->
	  let (q, x) = Arith.mono_of m in
	  let qinv = Q.inv q in
	  let k =  Arith.cnstrnt_of_addl (c s) post' in
          let j' = 
	    try Cnstrnt.add (Cnstrnt.multq qinv (c s x)) j 
	    with Not_found -> Cnstrnt.mk_real in
          let i' = Cnstrnt.multq (Q.minus qinv) (Cnstrnt.add j k) in
	  let s' = infer x i' s in
	    loop j' post' s'
  in
    loop Cnstrnt.mk_zero ml
  and is_unbound = function
    | App(Arith(Num _), []) ->
	false
    | App(Arith(Multq(_)), [x]) ->
	(try Cnstrnt.is_unbounded (c s x) with Not_found -> true)
    | x ->
	(try Cnstrnt.is_unbounded (c s x) with Not_found -> true)
  in
    match List.partition is_unbound ml with
      | yl, [] ->                               (* subcase: all monomials unbound. *)
	  s
      | [], zl ->                               (* subcase: all monomials bound. *)
	  propagate zl s 
      | [(Var _ as y)], zl ->                   (* subcase: only one variable is unbound. *)
	  let i = Cnstrnt.multq Q.negone (Arith.cnstrnt_of_addl (c s) zl) in
	    infer y i s
      | [App(Arith(Multq(q)), [y])], zl ->      (* subcase: [q*y + zl = 0] with [y] bound, [zl] unbound *)
	  let i = Arith.cnstrnt_of_addl (c s) zl in  (* thus: [y in -1/q ** C(zl)] *)
	  let i' = Cnstrnt.multq (Q.minus (Q.inv q)) i in
	    infer y i' s
      | yl, zl ->   
          extend (yl, zl) s


(** Propagating into power products. *)
let rec nonlin_star (che, chc, cha) =
  fold Th.pprod che nonlin_deduce &&&
  foldc chc nonlin_cnstrnt &&&
  fold Th.la cha nonlin_linearize


and nonlin_deduce e s =
  Trace.msg "rule" "Nonlin_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Pp(op), yl) -> 
	  let j = Pp.tau (c s) op yl in
	  let s = 
	    match op, yl with
	      | Expt(2), [y]              (* [x = x^2]. *)
		  when Term.eq (v s x) (v s y) ->
		  infer (v s y) (Cnstrnt.mk_cc Dom.Int Q.zero Q.one) s
	      | _ ->
		  s
	  in
	    (try
	       let i = c s x in
		 (match Cnstrnt.cmp i j with
		    | Binrel.Same -> s
		    | Binrel.Disjoint -> raise Exc.Inconsistent
		    | Binrel.Sub -> s
		    | Binrel.Super -> infer x i s
		    | Binrel.Singleton(q) -> infer x (Cnstrnt.mk_singleton q) s
		    | Binrel.Overlap(ij) -> infer x ij s)
	     with
		 Not_found -> infer x j s)
      | _ -> s

and nonlin_cnstrnt c s = 
  Trace.msg "rule" "Nonlin_cnstrnt" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
    folduse Th.pprod x
      (fun (y, b) s ->
	 match b with
	   | App(Pp(op), xl) ->
	       (try
		  let i = Pp.tau (Context.c s) op xl in
		    infer y i s
		with
		    Not_found -> s)
	   | _ ->
	       s)
      s

and nonlin_linearize e s =
  Trace.msg "rule" "Nonlin_linearize" e Fact.pp_equal;
  let (x, _, _) = Fact.d_equal e in              
    folduse Th.pprod x
      (fun (y, b) s ->
	 (match b with           
	    | App(Pp(Expt(n)), [z]) ->         (* [y = z^n] in [pprod] and [z = q] in [la] *)  
		(match Arith.d_num (find Th.la s z) with
		   | None -> s
		   | Some(q) -> 
		       let e' = Fact.mk_equal (v s y) (Arith.mk_num (Q.expt q n)) None in
			 Context.compose Th.la e' s)		
	    | App(Pp(Mult), [b1; b2]) ->    (* [y = z1^n1 * z2^n2]. *)
		let (z1, n1) = Pp.destruct b1 
		and (z2, n2) = Pp.destruct b2 in
		  (match Arith.d_num (find Th.la s z1), Arith.d_num (find Th.la s z2) with
		     | Some(q1), Some(q2) ->
			 let r = Q.mult (Q.expt q1 n1) (Q.expt q2 n2) in
			 let e' = Fact.mk_equal (v s y) (Arith.mk_num r) None in
			   Context.compose Th.la e' s
		(*     | Some(q1), None ->
			 let (s', z) =  if is_var b2 then (s, b2) else name Th.pprod (s, b2) in
			 let e' = Fact.mk_equal (v s y) (Arith.mk_multq (Q.expt q1 n1) z) None in
			   Context.compose Th.la e' s'
		     | None, Some(q2) ->
			 let (s', z) = if is_var b1 then (s, b1) else name Th.pprod (s, b1) in
			 let e' = Fact.mk_equal (v s y) (Arith.mk_multq (Q.expt q2 n1) z) None in
			   Context.compose Th.la e' s' *)
		     | _ ->
			 s)
	    | _ ->
		s))
      s
		       	

(** Propagate variable equalities and disequalities into array equalities. *)
let rec arrays_star (che, chd) =
  fold arr che arrays_equal &&&  
  foldd chd arrays_diseq


(** From the equality [x = y] and the facts
 [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
 and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
 that [z1 = k2]. *)

and arrays_equal e s =
  Trace.msg "rule" "Array_equal" e Fact.pp_equal;
  let (x, y, prf) = Fact.d_equal e in
    arrays_equal1 (x, y, prf)
      (arrays_equal1 (y, x, prf) s)

and arrays_equal1 (x, y, prf) s =
    Set.fold
      (fun z1 s1 -> 
	 match apply Th.arr s1 z1 with 
	   | App(Arrays(Select), [upd1; j1])
	       when is_equal s1 y j1 = Three.Yes ->
	       Context.fold s1
		 (fun z2 s2  -> 
		    match apply Th.arr s2 z2 with 
		      | App(Arrays(Update), [a2; i2; k2])
			  when is_equal s2 x i2 = Three.Yes -> 
			  let e' = Fact.mk_equal (v s2 z1) (v s2 k2) None in
			    update (Partition.merge e' (p_of s2)) s2
		      | _ -> s2)
		 upd1 s1
	   | _ -> s1)
      (use Th.arr s x)
      s


(** Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [z1 = select(upd, j')], [z2 = update(a,i',x)],
 [i = i'], [j = j'], [upd = z2], it follows that
 [z1 = z3], where [z3 = select(a,j)]. *)

and arrays_diseq d s =
  Trace.msg "rule" "Array_diseq" d Fact.pp_diseq;
  let (i, j, prf) = Fact.d_diseq d in
    arrays_diseq1 (i, j, prf)
      (arrays_diseq1 (j, i, prf) s)

and arrays_diseq1 (i, j, prf) s =
  Set.fold
   (fun z1 s1 -> 
      match apply Th.arr s1 z1 with
	| App(Arrays(Select), [upd; j']) 
	    when is_equal s1 j j' = Three.Yes ->
	    Context.fold s 
	      (fun z2 s2 ->
		 match apply Th.arr s2 z2 with
		   | App(Arrays(Update), [a; i'; _])
		       when is_equal s2 i i' = Three.Yes ->
		       let (s', z3) = name Th.arr (s, Arr.mk_select a j) in
		       let e' = Fact.mk_equal (v s2 z1) (v s2 z3) None in
		       let p' = Partition.merge e' (p_of s2) in
			 update p' s'
		   | _ -> s2)
	      upd s1
	| _ -> s)    
    (use Th.arr s j)
    s


(** Propagate variable equalities and disequalities into array equalities. *)
let rec bvarith_star (chbva, chbv) =
  fold bv chbv bvarith_propagate &&&
  fold bvarith chbva bvarith_deduce

and bvarith_propagate e s =
  Trace.msg "rule" "Bvarith" e Fact.pp_equal;
  let (x, bv, prf) = Fact.d_equal e in
  Set.fold
    (fun u s ->
       try
	 match apply bvarith s u with
	   | App(Bvarith(Unsigned), [x'])
	       when Term.eq x x' ->
	       let ui = Bvarith.mk_unsigned bv in
	       let (s', a') = abstract_term la (s, ui) in
	       let e' = Fact.mk_equal (v s' u) a' None in
		 Context.compose la e' s'
	   | _ ->
	       s 
       with
	   Not_found -> s)
    (use bvarith s x)
    s
 
and bvarith_deduce e s = 
  Trace.msg "rule" "Bvarith_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Bvarith(Unsigned), [y]) ->   (* [x = unsigned(y)] *)          
	  infer x Cnstrnt.mk_nat s
      | _ -> 
	  s


(** Deduce new facts from changes in the constraint part. *)
let rec diseq_star ch =
  foldc ch diseq_cnstrnt
	       
and diseq_cnstrnt c s = 
  Trace.msg "rule" "diseq_cnstrnt" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
  match Cnstrnt.d_singleton i with
    | None -> s
    | Some(q) ->
	let j = Cnstrnt.mk_diseq q in
	  Set.fold
	    (fun y s ->
	       let c' = Fact.mk_cnstrnt y j None in
		 update (Partition.add c' (p_of s)) s)
	    (d s x) s
  

(** Normalization step. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)

let compactify = ref true

let normalize s = 
  let filter = 
    Set.filter 
      (fun x -> 
	 not (mem u s x) &&      (* left-hand sides of these solution sets. *)
	 not (mem pprod s x) &&  (* are not kept in canonical form. *)
	 not (mem app s x) &&
         not (mem arr s x) &&
	 not (mem bvarith s x))
  in
  if !compactify then
    let xs = !V.removable in
    let xs' = filter xs in  
      if Set.is_empty xs' then s else 
	begin
	  Trace.msg "rule" "GC" (Term.Set.elements xs') (Pretty.set Term.pp);
	  let p' = Partition.restrict xs' (p_of s) in
	    update p' s
	end 
  else 
    s
	
	
(** [close s] applies the rules above until the resulting state is unchanged. *)
	

let maxclose = ref 10

exception Maxclose

let rec close s =
  let n = ref 0 in
  let s = ref s in
    try
      while not(Changed.stable()) do 
	let ch = Changed.save () in
	Changed.reset ();
	s := close1 ch !s;
	n := !n + 1;
	if !n > !maxclose && !maxclose >= 0 then
	  raise Maxclose
      done;
      normalize !s
    with
	Maxclose ->
	  Format.eprintf "\nUpper bound %d reached.@." !maxclose;
	  !s

and close1 ch =
  Trace.msg "rule" "Close" ch Changed.pp;
  let chv = Changed.in_v ch in
  let chd = Changed.in_d ch in
  let chc = Changed.in_c ch in 
  let ch i = Changed.in_eqs i ch in 
    propagate_star chv &&&
    arith_star (ch la, chc) &&&
    diseq_star chc &&&                     (* Propagate constraints into diseqs. *)
    nonlin_star (ch pprod, chc, ch la) &&& (* Propagate into Power products. *)
    arrays_star (chv, chd) &&&             (* Propagate into arrays. *)
    bvarith_star (ch bvarith, ch bv)       (* Propagate into arithmetic interps. *)
 









