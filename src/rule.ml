
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
open Theories
(*i*)

type rule = Context.t -> Context.t

type 'a transform = Context.t * 'a -> Context.t * 'a


(*s Extend with fresh variable equality. *)

let extend =  
  let v = Name.of_string "v" in
    (fun (s, a) ->
       assert(not(is_var a));
       let x = Var(Var.mk_fresh v None) in
       let i = theory_of (sym_of a) in
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
  let (s', a') = abstract_term A (s, a) in  (* not necessarily a variable. *)
  let c' = Fact.mk_cnstrnt a' i None in
    (s', c')

and abstract_toplevel_term (s, a) =
  abstract_term U (s, a)

and abstract_term i (s, a) =
  match a with
    | Var _ -> 
	(s, a)
    | App(f, al) ->
	let j = theory_of f in
	let (s', al') = abstract_args j (s, al) in
	let a' = if Term.eql al al' then a else sigma s f al' in
	  if i = U || i <> j then
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
      let x' = Term.mk_fresh_var (Name.of_string "k") None in
      let c' = Fact.mk_cnstrnt x' i None in
      let e' = Fact.mk_equal x' a None in
	Trace.msg "foo" "Slackify" e' Fact.pp_equal;
	let s' = update (Partition.add c' (p_of s)) s in  (* install constraint first. *) 
	  compose A e' s'
	  
(*s Sequential composition *)

let (&&&) f g x = g (f x)

(*s Propagating merged equalities. *)

let rec prop e =
  Trace.msg "rule" "Prop" e Fact.pp_equal;
  (propagate_theories e &&&
   Arrays.propagate e)

and propagate_theories e = 
  (propagate A e &&&
   propagate T e &&&
   propagate BV e &&&
   propagate S e &&&
   fuse U e)

and propagate i e s =
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
		  compose i e' s
	  with
	      Not_found ->
		let e' = Fact.mk_equal y a None in
		  compose i e' (restrict i x s)
	  with
	      Not_found -> s        (* [x] occurs neither on rhs nor on lhs. *)



(*s Propagating newly generated disequalities. *)

let rec diseqs d =
  Trace.msg "rule" "Diseqs" d Fact.pp_diseq;
  (diseqs_partition d &&&
   Arrays.diseq d)

and diseqs_partition d s =
  update (Partition.diseq d (p_of s)) s

(*s Deduce new facts from changes in the linear arithmetic part. *)

let rec arith e =   
  Trace.msg "rule" "Arith" e Fact.pp_equal;
  (arith_deduce e &&&
   arith_linearize e)
 
and arith_deduce e s = 
  Trace.msg "rule" "Arith_deduce" e Fact.pp_equal;
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Arith(op), xl) -> 
	  Deduce.of_linarith x (op, xl) s
      | _ -> s

and arith_linearize e s = 
  Trace.msg "rule" "Arith_linearize" e Fact.pp_equal;
  let (x, a, _) = Fact.d_equal e in  (*s For [x = q] and [y = z1 * x] add [y = z1 * q] *)
    match find A s a with             (*s and [x = y]. *)
      | App(Arith(Num(q)), []) ->
	  Set.fold
	  (fun y s -> 
	     match find U s y with
	       | App(Builtin(Mult), zl) -> 
		   linearize_mult (x, q) (y, zl) s
	       | App(Builtin(Div), [z1; z2]) ->
		   linearize_div (x, q) (y, z1, z2) s
	       | App(Builtin(Expt), [n; z]) ->
		   linearize_expt (x, q) (y, n, z) s
	       | _ -> s)
	  (use U s x) s
      | _ -> s

and linearize_mult (x, q) (y, zl) s = 
  let infer y (q, z) = 
    let e' = Fact.mk_equal (v s y) (Arith.mk_multq q z) None in  
      compose A e' s
  in
    match zl with
    | [z1; z2] when Term.eq z1 x ->       (* [y = q * z2] *)
	infer y (q, z2)
    | [z1; z2] when Term.eq z2 x ->       (* [y = z1 * q] *)
	infer y (q, z1)
    | _ ->  s

and linearize_div (x, q) (y, z1, z2) s = (* [x = q] is known  and [y = z1 / z2] in [u]. *)
  let infer y (q, z) = 
    let e' = Fact.mk_equal (v s y) (Arith.mk_multq q z) None in
      compose A e' s
  in
    if Term.eq x z2 && not(Q.is_zero q) then   (* deduce [y = 1/q * z1]. *)
      infer y (Q.inv q, z1)
    else 
      s

and linearize_expt (x, q) (y, n, z) s =   (* [x = q] is known  and [y = z^n ] in [u]. *)
  let n_is_two  =
    match find A s n with
      | App(Arith(Num(q)), []) -> Q.equal q Q.two 
      | _ -> false
  in
  if Term.eq x z && n_is_two then         (* deduce [y = q^2]. *)
    let e' = Fact.mk_equal (v s y) (Arith.mk_num (Mpa.Q.mult q q)) None in 
      compose A e' s
  else
    s


(*s Deduce facts from changes in the [u] structure. *)

let rec uninterp e =  
  Trace.msg "rule" "Uninterp" e Fact.pp_equal;
  uninterp_deduce e &&&
  uninterp_div e

and uninterp_deduce e s =
  let (x, a, _) = Fact.d_equal e in
    match a with
      | App(Builtin(op), xl) -> 
	  Deduce.of_builtin x (op, xl) s
      | _ ->
	  s

and uninterp_div e s =         (* if [v1 = x/y] then deduce [v2 = x] with [v2 = v1 * y]. *)
  let (v1, b, _) = Fact.d_equal e in
    match b with 
      | App(Builtin(Div), [x; y]) ->
	  let b = Term.mk_app (Sym.Builtin(Sym.Mult)) [v1; y] in
	  let (s', v2) = name U (s, b) in
	  let e' = Fact.mk_equal v2 x None in
	    merge e' s
      | _ ->
	  s
		
		       
and uninterp_mult e s =              (* if [x = b] in [u], [b] is of the form [y1 * ... * yn], *)
  let (x, b, _) = Fact.d_equal e in     (* and [z = z1 * ... * x' * zm] in [u], then add *)
  let x' = v s x in                       (* [z = z1 * ... * y1 * ... * yn * ... * zm]. *)
    match b with                       
      | App(Builtin(Mult), _) ->
	  Set.fold 
	  (fun z s ->
	     try
	       let b =  apply U s z in
		 match b with
		   | App(Builtin(Mult), zl) when List.exists (Term.eq x') zl ->
		       let b' = mult_replace zl x b in
			 if Term.eq b b' then s else 
			   let e' = Fact.mk_equal z b' None in
			     union U e' s
		   | _ -> s
		 with
		     Not_found -> s)
	  (use U s x') s
      | _ -> s

and mult_replace xl y z =  (* Replace [y] in the list of variables [xl] by [z]. *)
  failwith "to do"
    
 

(*s Deduce new facts from changes in the constraint part. *)


let rec cnstrnt c = 
  Trace.msg "rule" "Cnstrnt" c Fact.pp_cnstrnt;
  (cnstrnt_singleton c &&&
   cnstrnt_diseq c &&&
   cnstrnt_equal A c &&&
   cnstrnt_equal U c) 
     
and cnstrnt_equal th c s =
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
  let (x, i, _) = Fact.d_cnstrnt c in
  match Cnstrnt.d_singleton i with
    | None -> s
    | Some(q) ->
	let e = Fact.mk_equal x (Arith.mk_num q) None in
	  compose A e s


(*s Normalization step. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)

let compactify = ref true

let normalize s = 
  let filter = Set.filter (fun x -> not (mem U s x)) in
  if !compactify then
    let xs = !V.removable in
    let xs' = filter xs in  
      Trace.msg "rule" "Restrict" (Term.Set.elements xs') (Pretty.set Term.pp);
      let p' = Partition.restrict xs' (p_of s) in
	update p' s
  else 
    s
	
	
(*s [close s] applies the rules above until the resulting state is
  unchanged. *)
	
let aclose =
  Set.fold 
    (fun x s -> 
       try 
	 arith (equality A s (v s x)) s
       with 
	   Not_found -> s)
    
let uclose = 
  Set.fold
    (fun x s -> 
       try 
	 uninterp (equality U s x) s
       with 
	   Not_found -> s)

let vclose =
  Set.fold
    (fun x s -> 
       try 
	 prop (V.equality (v_of s) x) s
       with 
	   Not_found -> s)

let dclose ch s = s
   (* List.fold_right diseqs focus s' *)

let cclose = 
 Set.fold
   (fun x s -> 
      try 
	cnstrnt (Partition.cnstrnt (p_of s) x) s
      with 
	  Not_found -> s)


let maxclose = ref 10 (* value -1 is unbounded *)

exception Maxclose

let rec close s =
  let n = ref 0 in
  let s = ref s in
    try
      while not(Changed.stable()) do 
	let ch = Changed.save () in
	Changed.reset ();
	assert (Changed.stable());
	s := close1 ch !s;
	n := !n + 1;
	if !n > !maxclose then
	  raise Maxclose
      done;
      normalize !s
    with
	Maxclose ->
	  Format.eprintf "\nPossible incompleteness: Upper bound %d reached.@." !maxclose;
	  !s

and close1 ch =
  Trace.msg "close" "Close" ch Changed.pp;
  aclose (Changed.in_eqs A ch) &&&
  uclose (Changed.in_eqs U ch) &&&
  vclose (Changed.in_v ch) &&&
  dclose (Changed.in_d ch) &&&
  cclose (Changed.in_c ch)
