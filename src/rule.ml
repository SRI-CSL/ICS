
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
open Context
open Th
(*i*)

type rule = Context.t -> Context.t

type 'a transform = Context.t * 'a -> Context.t * 'a


(*s Extend with fresh variable equality. *)

let extend =  
  let v = Name.of_string "v" in
    (fun (s, a) ->
       assert(not(is_var a));
       let x = Var(Var.mk_fresh v None) in
       let i = Th.of_sym (sym_of a) in
       let e = Fact.mk_equal x a None in
	 (union i e s, x))


(*s Variable abstract an atom *)

(*s Abstraction. *)

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
  let (s', a') = abstract_term la (s, a) in  (* not necessarily a variable. *)
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
	  if i = u || i <> j then
	    try (s', v s' (inv j s' a')) with Not_found -> extend (s', a')
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



(*s Adding equalities/disequalities/constraints to partition. *)

let merge e s = 
  Trace.msg "rule" "Merge" e Fact.pp_equal;
  update (Partition.merge e (p_of s)) s
    
let diseq d s = 
  Trace.msg "rule" "Diseq" d Fact.pp_diseq;
  update (Partition.diseq d (p_of s)) s
    
let add c s = 
  Trace.msg "rule" "Add" c Fact.pp_cnstrnt;
  let (a, i, _) = Fact.d_cnstrnt c in
    if is_var a then
      update (Partition.add c (p_of s)) s
    else 
      let x' = Var(Var.mk_slack None) in
      let c' = Fact.mk_cnstrnt x' i None in
      let e' = Fact.mk_equal x' a None in
	Trace.msg "foo" "Slackify" e' Fact.pp_equal;
	let s' = update (Partition.add c' (p_of s)) s in  (* install constraint first. *) 
	  compose la e' s'
	  
(*s Sequential composition *)

let (&&&) f g x = g (f x)

(*s Applying a rule [f] for all equalities [x = a] in theory-specific solution set [s]
 of index [i] such that [x] is equivalent to some [y] in the changed set [ch]. *)

let fold i ch f =
  Set.fold 
    (fun x s -> 
       try 
	 f (equality i s (v s x)) s
       with 
	   Not_found -> s)
    ch

(*s Applyfing rule [f e] for all equalities [x = y] such that [x] is in the changed
 set [ch] and accumulating the results. *)

let foldv ch f =
  Set.fold
    (fun x s -> 
       try 
	 f (V.equality (v_of s) x) s
       with 
	   Not_found -> s)
    ch

(*s Applying rule [f d] for all disequalities [d] in [s] of the form [x <> y] where [x]
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
      try 
	f (Partition.cnstrnt (p_of s) x) s
      with 
	  Not_found -> s)
    ch


(*s Propagating merged equalities into theory-specific solution sets. *)

let rec prop_star ch = foldv ch prop

and prop e =           
  (compose la e &&&
   compose p e &&&
   compose bv e &&&
   compose cop e &&&
   fuse u e &&&
   fuse pprod e &&&
   fuse app e &&&
   fuse arr e &&&
   fuse bvarith e)

and compose i e s =
  Trace.msg "rule" "Compose" e Fact.pp_equal;
  let (x, y, _) = Fact.d_equal e in
    if not(Set.is_empty (use i s x)) then      (* [x] occurs on rhs. *)
      let e' = Fact.mk_equal x (find i s y) None in
	fuse i e' s
    else
      try
	let a = apply i s x in
	  try
	    let b = apply i s y in 
	      if Term.eq a b then s else 
		let e' = Fact.mk_equal a b None in
		  Context.compose i e' s
	  with
	      Not_found ->
		let e' = Fact.mk_equal y a None in
		  Context.compose i e' (Context.restrict i x s)
	  with
	      Not_found -> s        (* [x] occurs neither on rhs nor on lhs. *)




(*s Deduce new facts from changes in the linear arithmetic part. *)

let rec arith_star ch = 
  fold la ch arith

and arith e =   
  Trace.msg "rule" "Arith" e Fact.pp_equal;
  arith_deduce e
 
and arith_deduce e s = 
  Trace.msg "rule" "Arith_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Arith(op), xl) -> 
	  Deduce.of_linarith x (op, xl) s
      | _ -> s


(*s Groebner basis completion for power products. *)

let groebner_completion = ref false

let rec pprod_star (che, chc) s =
  (groebner_star che &&&
   fold pprod che pproduct_deduce &&&
   foldc chc pproduct_cnstrnt) s

and groebner_star ch s =
  if not(!groebner_completion) then s else
    fold pprod ch groebner s

and groebner e s =
  if not(!groebner_completion) then s else
    let (x, pp, _) = Fact.d_equal e in
      Fact.Equalset.fold
	(fun e' s ->
	   let (y, qq, _) = Fact.d_equal e' in   
	   let (p, q, gcd) = Pp.gcd pp qq in  (* now [x * p = gcd] and [y * q = gcd] *)
	     if Pp.is_one gcd || Pp.is_one p || Pp.is_one q then 
	       s
	     else
	       let xp = Pp.mk_mult (v s x) p
	       and yq = Pp.mk_mult (v s y) q in
		 try
		   let u1 = Context.inv pprod s xp in
		   let u2 = Context.inv pprod s yq in
		     match Context.is_equal s u1 u2 with
		       | Three.Yes ->
			   s
		       | Three.No -> 
			   raise Exc.Inconsistent
		       | Three.X ->
			   let e'' = Fact.mk_equal (v s u1) (v s u2) None in  
			     Trace.msg "rule" "Groebner" e'' Fact.pp_equal;
			     merge e'' s
		     with
			 Not_found -> s)
	(cuts e s)
	s
      
and cuts e s =  
  let (_, pp, _) = Fact.d_equal e in
  let es =
    Set.fold 
      (fun y acc1 -> 
	 (Set.fold
	    (fun u acc2 ->
	       try 
		 let e' = equality pprod s u in
		   Fact.Equalset.add e' acc2
	       with
		   Not_found -> acc2)
	    (use pprod s y)
	    acc1))
      (vars_of pp) 
      Fact.Equalset.empty
  in
    Fact.Equalset.remove e es

and pproduct_deduce e s =
  Trace.msg "rule" "Nonlin_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Pp(op), xl) -> 
	  Deduce.of_pprod x (op, xl) s
      | _ -> s

and pproduct_cnstrnt c s = 
  Trace.msg "rule" "Nonlin_cnstrnt" c Fact.pp_cnstrnt;
  s


(*s Propagate variable equalities and disequalities into array equalities. *)

let arrays_star (che, chd) s = s
(*
  fold arr che Arrays.propagate &&&  
  foldd chd Arrays.diseq
*)

(*s Deduce new facts from changes in the constraint part. *)

let rec cnstrnt_star ch =
  foldc ch cnstrnt

and cnstrnt c = 
  Trace.msg "rule" "Cnstrnt" c Fact.pp_cnstrnt;
  (cnstrnt_singleton c &&&
   cnstrnt_diseq c &&&
   cnstrnt_equal la c &&&
   cnstrnt_equal pprod c) 
     
and cnstrnt_equal th c s =
  Trace.msg "rule" "Cnstrnt(=)" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
  try
    let b = apply th s x in
    let e = Fact.mk_equal x b None in
      Deduce.deduce e s
  with
      Not_found ->
	Set.fold
	(fun y s ->
	   try
	     let b = apply th s y in
	     let e = Fact.mk_equal y b None in
	       Deduce.deduce e s
	   with
	       Not_found -> s)
	(use th s x) s

	       
and cnstrnt_diseq c s = 
  Trace.msg "rule" "Cnstrnt(<>)" c Fact.pp_cnstrnt;
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
  
and cnstrnt_singleton c s = 
  Trace.msg "rule" "Cnstrnt(single)" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
  match Cnstrnt.d_singleton i with
    | None -> s
    | Some(q) ->
	let e = Fact.mk_equal x (Arith.mk_num q) None in
	  compose la e s


(*s Normalization step. Remove all variables [x] which are are scheduled
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
      Trace.msg "rule" "GC" (Term.Set.elements xs') (Pretty.set Term.pp);
      let p' = Partition.restrict xs' (p_of s) in
	update p' s
  else 
    s
	
	
(*s [close s] applies the rules above until the resulting state is unchanged. *)
	

let maxclose = ref 10 (* value -1 is unbounded *)

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
	  Format.eprintf "\nPossible incompleteness: Upper bound %d reached.@."                         !maxclose;
	  !s

and close1 ch =
  Trace.msg "close" "Close" ch Changed.pp;
  let chv = Changed.in_v ch in
  let chd = Changed.in_d ch in
  let chc = Changed.in_c ch in 
  let ch i = Changed.in_eqs i ch in 
    prop_star chv &&&
    arith_star (ch la) &&&
    cnstrnt_star chc &&&
    pprod_star (ch pprod, chc) &&&  (* Propagate into Power products. *)
    arrays_star (chv, chd)        (* Propagate into arrays. *)
 
