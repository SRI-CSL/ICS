
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
(*i*)


(*s Adding equalities/disequalities/constraints to partition. *)

let merge e s =
  Trace.msg "tac" "Merge" e Fact.pp_equal;
  update_p s (Partition.merge e s.p)


let diseq d s = 
  Trace.msg "tac" "Diseq" d Fact.pp_diseq;
  update_p s (Partition.diseq d s.p)


let add c s =
  Trace.msg "tac" "Add" c Fact.pp_cnstrnt;
  let (a, i, _) = Fact.d_cnstrnt c in
    if is_var a then
      update_p s (Partition.add c s.p)
    else 
      let x' = Term.mk_fresh_var (Name.of_string "k") None in
      let e' = Fact.mk_equal x' a None in
      let c' = Fact.mk_cnstrnt x' i None in
      let s' = update_p s (Partition.add c' s.p) in  (* install constraint first. *)
	compose A e' s'


(*s Composition. *)

let (&&&) f g s = g (f s)


(*s Propagating merged equalities. *)

let rec propagate e =
  Trace.msg "tac" "Prop" e Fact.pp_equal;
  propagate_theories e &&&
  propagate_array e

and propagate_array e s =
  install U s (Arrays.propagate e (s.p, s.u))

and propagate_theories e =
  Context.propagate A e &&&
  Context.propagate T e &&&
  Context.propagate BV e &&&
  Context.fuse U e


and fuse_u e s =                                    (* fuse in [u] with builtin simplification. *)
  let (x, y, _) = Fact.d_equal e in                 (* Normalization may yield terms involving. *)
  let varrepl z = if Term.eq x z then y else z in   (* nested uninterpreted and interpreted terms. *) 
  Set.fold
    (fun x acc ->
       try
	 let b = apply U acc x in
	 let (acc', b') = 
	   match b with
	     | App(Builtin(op), xl) ->
		 let xl' = mapl varrepl xl in
		   if xl == xl' then (acc, b) else 
		     (abstract_term U acc (Sig.sigma acc op xl'))
	     | App(f, xl) ->
		 let xl' = mapl varrepl xl in
		   if xl == xl' then (acc, b) else 
		     (acc, mk_app f xl')
	     | Var _ -> (* should not happen. *)
		 (acc, varrepl b)
	 in
	   install U acc' (Solution.update (x, b') (acc'.p, acc'.u))
       with
	   Not_found -> acc)
    (use U s x)
    s


(*s Propagating newly generated disequalities. *)

let rec diseqs d =
  Trace.msg "tac" "Diseqs" d Fact.pp_diseq;
  diseqs_cnstrnt d &&&
  diseqs_array d

and diseqs_cnstrnt d s =
  update_c s (C.diseq d (c_of s))

and diseqs_array d s =
  install U s (Arrays.diseq d (s.p, s.u))


(*s Deduce new facts from changes in the linear arithmetic part. *)

let rec arith e =  
  Trace.msg "tac" "Arith" e Fact.pp_equal;
    deduce e  (* &&&
    linearize e *)   (* linearize is now subsumed by fuse_u. *)
 
and deduce e s =
  let (_, a, _) = Fact.d_equal e in
  if is_var a then s else
    let i = Sym.theory_of (sym_of a) in
    let (v', c', e') = Deduce.deduce e (v_of s, c_of s, solutions i s) in
      update i (update_c (update_v s v') c') e'

and linearize e s =
  let (x, a, _) = Fact.d_equal e in
  match a with
    | App(Arith(Num(q)), []) ->
	Set.fold 
	   (fun y s -> match Solution.find s.u y with
	      | App(Builtin(Mult), zl) -> 
		  linearize_mult (x, q) (y, zl) s
	      | _ -> 
		  s)
	(Solution.use s.u x) s
    | _ -> s

and linearize_mult (x, q) (y, zl) s = 
  match zl with
    | [z] ->
	let e' = Fact.mk_equal y (Arith.mk_num q) None in
	  compose A e' s
    | [z1; z2] when Term.eq z1 x ->       (* [y = q * z2] *)
	let e' = Fact.mk_equal y (Arith.mk_multq q z2) None in
	  compose A e' s
    | [z1; z2] when Term.eq z2 x ->       (* [y = z1 * q] *)
	let e' = Fact.mk_equal y (Arith.mk_multq q z1) None in
	  compose A e' s
    | _ -> 
	s  (* to be done. *)


(*s Deduce facts from changes in the [u] structure. *)

let uninterp e s = 
  Trace.msg "tac" "Uninterp" e Fact.pp_equal;
  deduce e s
 

(*s Deduce new facts from changes in the constraint part. *)


let rec cnstrnt c =
  Trace.msg "tac" "Cnstrnt" c Fact.pp_cnstrnt;
  cnstrnt_singleton c &&&
  cnstrnt_diseq c &&&
  cnstrnt_equal A c &&&
  cnstrnt_equal U c
     
and cnstrnt_equal th c s =
  let (x, i, _) = Fact.d_cnstrnt c in
  try
    let b = apply th s x in
    let e = Fact.mk_equal x b None in
      deduce e s
  with
      Not_found ->
	Set.fold
	  (fun y s ->
	     try
	       let b = apply th s y in
	       let e = Fact.mk_equal y b None in
		 deduce e s
	     with
		 Not_found -> s)
	  (use th s x)
	  s
	       
and cnstrnt_diseq c s = 
  let (x, i, _) = Fact.d_cnstrnt c in
  match Cnstrnt.d_singleton i with
    | None -> s
    | Some(q) ->
	let j = Cnstrnt.mk_diseq q in
	Set.fold
	  (fun y s ->
	     update_c s (C.add (Fact.mk_cnstrnt y j None) s.p.Partition.c))
	  (D.deq s.p.Partition.d x)
	  s
  
and cnstrnt_singleton c s = 
  let (x, i, _) = Fact.d_cnstrnt c in
  match Cnstrnt.d_singleton i with
    | None ->
	s
    | Some(q) ->
	install A s (Solution.compose Arith.map (s.p, s.a) [x, Arith.mk_num q])


(*s Normalization step. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)

let compactify = ref true

let normalize s =
  if not(!compactify) then s else
    let xs = Partition.removable s.p in
    let xs' = Set.filter (fun x -> not (Solution.mem s.u x)) xs in  
      Trace.msg "tac" "Normalize" (Set.elements xs') (Pretty.set Term.pp);
      update_p s (Partition.restrict xs' s.p)


(*s [close s] applies the rules above until the resulting state is
 unchanged. *)

let maxclose = ref 10 (* value -1 is unbounded *)

let rec close s =
  (repeat 0 close1 &&& normalize) s

and close1 n s =
  Trace.msg "tac" "Close" n Pretty.number;
  (adeduce (Solution.changed s.a) &&&
   udeduce (Solution.changed s.u) &&&
   vdedude (Partition.changed_v s.p) &&&
   ddeduce (Partition.changed_d s.p) &&&
   cdeduce (Partition.changed_c s.p)) s

and repeat numloops f s =
  let t = f numloops s in
    if Context.eq s t then 
      t
    else if numloops < !maxclose then
      repeat (numloops + 1) f t
    else 
      begin
	Format.eprintf "\nPossible incompleteness: Upper bound %d reached.@." !maxclose;
	t
      end

and adeduce focus s = 
  let s' = update A s (Solution.reset s.a) in
  Set.fold
    (fun x s ->
       let y = v s x in
	 try 
	   let a = Solution.apply s.a y in
	   let e = Fact.mk_equal y a None in
	   arith e s 
	 with Not_found -> s)
    focus 
    s'

and vdedude focus s =
  let s' = update_p s (Partition.reset_v s.p) in
  Set.fold 
    (fun x -> 
       let y = v s x in
       let e = Fact.mk_equal x y None in
	 propagate e)
    focus s'

and ddeduce focus s =
  let s' = update_p s (Partition.reset_d s.p) in
   List.fold_right diseqs focus s'
	

and cdeduce focus s =
  let s' = update_p s (Partition.reset_c s.p) in
    Set.fold
      (fun x s -> 
	 try
	   let y = v s x in
	   let i = C.apply (c_of s) y in
	   let c = Fact.mk_cnstrnt y i None in
	     cnstrnt c s
	 with
	     Not_found -> s)
      focus
      s'

and udeduce focus s =
  let s' = update U s (Solution.reset s.u) in
    Set.fold
      (fun x s ->
	 try
	   let y = apply U s x in
	   let e = Fact.mk_equal x y None in
	     uninterp e s
	 with
	     Not_found -> s)
      focus
      s'

