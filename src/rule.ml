
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
 * 
 * Author: Harald Ruess, N. Shankar
 *)

open Term
open Three
open Mpa
open Sym
open Context
open Th

type rule = Context.t -> Context.t

type 'a transform = Context.t * 'a -> Context.t * 'a


(** Extend with fresh variable equality. *)

let extend =  
  let v = Name.of_string "v" in
    (fun (s, a) ->
       assert(not(is_var a));
       let x = Var(Var.mk_fresh v None) in
       let i = Th.of_sym (sym_of a) in
       let e = Fact.mk_equal x a None in
	 (union i e s, x))


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
		Not_found -> extend (s', a')
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
  (compose Th.la e &&&
   compose Th.p e &&&
   compose Th.bv e &&&
   compose Th.cop e &&&
   fuse Th.u e &&&
   fuse Th.pprod e &&&
   fuse Th.app e &&&
   fuse Th.arr e &&&
   fuse Th.bvarith e)


(** Deduce new constraints from changes in the linear arithmetic part. *)
let rec arith_star (chla, chc) =
  fold Th.la chla arith_fme &&&
  foldc chc arith_cnstrnt

and arith_cnstrnt c s =
  let (x, i, _) = Fact.d_cnstrnt c in
    folduse Th.la x
      (fun (y, b) s ->
	 match b with
	   | App(Arith(op), xl) ->
	       (try
		  let i = Arith.tau (Context.c s) op xl in
		    infer y i s
		with
		    Not_found -> s)
	   | _ ->
	       s)
      s
 
and arith_fme e s = 
  Trace.msg "rule" "Arith_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Arith(Num(q)), []) -> 
	  infer x (Cnstrnt.mk_singleton q) s
      | App(Arith(Multq(q)), [y]) -> 
	  (try infer x (Cnstrnt.multq q (c s y)) s with Not_found -> s)
      | App(Arith(Add), ml) ->
	  let i = Arith.cnstrnt (c s) b in
	    ints x ml 
	      (fme x b
		 (infer x i s))
      | _ ->
	  s 

and fme x b s =
  try
    let i1 = c s x in
    let dom1 = Cnstrnt.dom_of i1 in
    let (lo1, hi1) = Cnstrnt.endpoints_of i1 in
    let (q1, z1, ml1) = Arith.destructure b in
      folduse Th.la z1
	(fun (y, b) s ->
	   if Term.eq y x then s else
	     let (q2, z2, ml2) = Arith.destructure b in
	       if not(Term.eq z1 z2) then s else
		 try
		   let i2 = c s y in
		   let dom2 = Cnstrnt.dom_of i2 in
		   let (lo2, hi2) = Cnstrnt.endpoints_of i2 in
		   let dom = Dom.Real (* Dom.union dom1 dom2 *) in
		     (match Q.sign q1, Q.sign q2 with
			| Sign.Pos, Sign.Pos ->
			    less dom (q2, lo2, ml2) (q1, hi1, ml1)
			      (less dom (q1, lo1, ml1) (q2, hi2, ml2) s)
			| Sign.Pos, Sign.Neg ->
			    less dom (q2, hi2, ml2) (q1, hi1, ml1)
			      (less dom (q1, lo1, ml1) (q2, lo2, ml2) s)
			| Sign.Neg, Sign.Pos ->
			    less dom (q1, hi1, ml1) (q2, hi2, ml2)
			      (less dom (q2, lo2, ml2) (q1, lo1, ml1) s)
			| Sign.Neg, Sign.Neg ->
			    less dom (q1, hi1, ml1) (q2, lo2, ml2)
			      (less dom (q2, hi2, ml2) (q1, lo1, ml1) s)
			| _ ->
			    s)
		 with
		     Not_found -> s)
	s      
  with
      Not_found -> s

	
(** Generate an inequality [1/q1 * (p1 - m1) < 1/q2 * (p2 - m2). *)	
and less dom (q1, ep1, m1) (q2, ep2, m2) s = 
  let (extq1, alpha) = Endpoint.destruct ep1 in
  let (extq2, beta) = Endpoint.destruct ep2 in
    match Extq.destruct extq1, Extq.destruct extq2 with
      | Extq.Posinf, Extq.Posinf -> 
	  s
      | Extq.Posinf, _ -> 
	  raise Exc.Inconsistent
      | Extq. Neginf, Extq.Neginf -> 
	  s
      | _, Extq.Neginf -> 
	  raise Exc.Inconsistent
      | Extq.Inject(p1), Extq.Inject(p2) ->
	  let a = Arith.mk_addq (Q.div p1 q1)
		    (Arith.mk_multq (Q.minus (Q.inv q1)) m1) in
	  let b = Arith.mk_addq (Q.div p2 q2)
		    (Arith.mk_multq (Q.minus (Q.inv q2)) m2) in
	  let a_sub_b = Can.term s (Arith.mk_sub a b) in
	  let i' = Cnstrnt.mk_lower dom (Q.zero, alpha && beta) in
	    (match a_sub_b with
	       | App(Arith(Num(q)), []) -> 
		   if Cnstrnt.mem q i' then s else raise Exc.Inconsistent
	       | App(Arith(Multq(q)), [x']) ->
		   infer x' (Cnstrnt.multq (Q.inv q) i') s
	       | _ ->
		   let (s', x') = name Th.la (s, a_sub_b) in
		     Trace.msg "fme" "fme" (a_sub_b, i') Term.pp_in;
		     infer x' i' s')
      | _ ->
	  s

(* Problem: x <= y, y <= z, z <= x. Here, [name] above generates a fresh
 variable. Probably, something smarter is needed, such as looking for a
 rhs such that a linear multiple of it matches the current term. *)

and ints x ml s = 
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
			 Trace.msg "foo6" "Lin" e' Fact.pp_equal;
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
	

let maxclose = ref 999999999

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
	if !n > !maxclose then
	  raise Maxclose
      done;
      normalize !s
    with
	Maxclose ->
	  Format.eprintf "\nUpper bound %d reached.@." !maxclose;
	  !s

and close1 ch =
  Trace.msg "close" "Close" ch Changed.pp;
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
 









