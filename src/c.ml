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
 * Author: Harald Ruess
 *)

open Term
open Sym
open Mpa

type t = {
  c: (Cnstrnt.t * Fact.justification option) Var.Map.t;
  use : Use.t;
}

let empty = {
  c = Var.Map.empty;
  use = Use.empty
}

let eq s t = (s.c == t.c)

let cnstrnts s = s.c

let to_list s =
  Var.Map.fold 
    (fun x (i, _) acc -> (x, i) :: acc) 
    s.c []

let pp fmt s =
  let l = to_list s in
    if l <> [] then
      begin
	Format.fprintf fmt "\nc:";
	Pretty.map Var.pp Cnstrnt.pp fmt l
      end

let changed = ref Set.empty

let apply s = function
  | Var(x) -> Var.Map.find x s.c
  | App _ -> raise Not_found

let use s = Use.find s.use

let cnstrnt s x =
  let (i, prf) = apply s x in
    Fact.mk_cnstrnt x i prf

let mem a s = 
  match a with
    | Var(x) -> Var.Map.mem x s.c
    | App _ -> false

let of_term s =
 let lookup s x = fst(apply s x) in
   Cnstrnt.of_term (lookup s)

let of_addl s =
  let lookup s x = fst(apply s x) in
    Cnstrnt.of_addl (lookup s)


(** [update x c s] updates the constraint map with the constraint [x in c] and modifies
  the use lists accordingly. In addition, all implied inequalities are added. As a 
  side-effect, {!C.changed} is updated. *)


let update a c prf s =
  match a with
    | Var(x) ->
	Trace.msg "c" "Update" (a, c) (Pretty.infix Term.pp " in " Cnstrnt.pp);
	changed := Term.Set.add a !changed;
	let use' =
	  try
	    let (d,_) = apply s a in
	      Cnstrnt.varfold (Use.remove a) d s.use
	  with
	      Not_found -> s.use
	in
	  {s with 
	     c = Var.Map.add x (c, prf) s.c;
	     use = Cnstrnt.varfold (fun b -> Use.add a b) c use'}
    | _ -> 
	s

(** Restrict the map. *)
let restrict a s =
  match a with
    | Var(x) ->
	(try
	   let (i, _) = apply s a in
	     Trace.msg "c" "Restrict" a Term.pp;
	     changed := Term.Set.remove a !changed;
	     {s with 
		c = Var.Map.remove x s.c;
		use = Cnstrnt.varfold (fun b -> Use.remove a b) i s.use}
	 with
	     Not_found -> s)
    | _ -> 
	s

(** Adding a new constraint together with implied constraints *)
let add interp is_inconsistent cnstrnt s =
  let es = ref Fact.Equalset.empty in
  let rec add1 cnstrnt s =
    Trace.msg "c" "Add" cnstrnt Fact.pp_cnstrnt;
    let (x, i, prf1) = Fact.d_cnstrnt cnstrnt in   (* [prf1 |- x in i]. *)
      if Cnstrnt.is_empty i then
	raise Exc.Inconsistent
      else if is_inconsistent x i then
	raise Exc.Inconsistent
      else
	try
	  let (j, prf2) = apply s x in    (* [prf2 |- x in j]. *)
	    (match Cnstrnt.cmp i j with
	       | Cnstrnt.Disjoint -> 
		   raise Exc.Inconsistent
	       | (Cnstrnt.Same | Cnstrnt.Super) -> 
		   s
	       | Cnstrnt.Sub -> 
		   install x i prf1 s
	       | Cnstrnt.Overlap ->
		   let ij = Cnstrnt.inter i j in
		   let prf = Fact.mk_rule "inter" [prf1; prf2] in
		     install x ij prf s)
	with
	    Not_found -> 
	      install x i prf1 s

  and install x c prf s =
    let (as', c') = Cnstrnt.d_equalities c in
      es :=                          (* install new equalities *)
        Term.Set.fold 
	  (fun a -> 
	     let e = Fact.mk_equal x a None in
	       Fact.Equalset.add e)
	  as' !es;
      let s = update x c' prf s in
      let ineqs =
	try
	  let a = interp x in
	    Cnstrnt.equal a c' @ Cnstrnt.implied c'
	with
	    Not_found -> Cnstrnt.implied c'
      in
	List.fold_right
	  (fun i s ->
	     Trace.msg "c1" "Implied" i Arith.pp_ineq;
	     match i with
	       | Arith.True -> 
		   s
	       | Arith.False -> 
		   raise Exc.Inconsistent
	       | Arith.Less(x', alpha, a') -> 
		   let c' = Cnstrnt.mk_less Dom.Real (a', alpha) in
		   let cnstrnt' = Fact.mk_cnstrnt x' c' None in
		     add1 cnstrnt' s
	       | Arith.Greater(x', alpha, a') -> 
		   let c' = Cnstrnt.mk_greater Dom.Real (alpha, a') in
		   let cnstrnt' = Fact.mk_cnstrnt x' c' None in
		     add1 cnstrnt' s)
	  ineqs
	  s
  in
  let s = add1 cnstrnt s in
    (!es, s)


let merge interp is_inconsistent el s =
  let es = ref Fact.Equalset.empty in
  let add x c prf s = 
    let cnstrnt = Fact.mk_cnstrnt x c prf in
    let (es', s') = add interp is_inconsistent cnstrnt s in
      es := Fact.Equalset.union es' !es;
      s'
  in
  let rec merge1 e s =
    let (x, a, prf) = Fact.d_equal e in    (* [prf |- x = a] *)
      try
	let (c, prf1) = apply s x in       (* [prf1 |- x in c] *)
	  (try
	     let (d, prf2) = apply s a in   (* [prf2 |- a in d] with [a <<< x] *)
	     let s = restrict x s in
	       if Cnstrnt.occurs a c then 
		 begin
		   s  (* incompleteness? *)
		 end 
	       else 
		 add a (Cnstrnt.inter c d) None s
	   with 
	       Not_found ->
		 if is_var a then
		   add a c prf1 (restrict x s)
		 else 
		   infer e (propagate e s))
      with
	  Not_found ->
	    infer e
	      (propagate e s)

  and propagate e s =     (* replace [x] by [a] in constraints *)
    let (x, a, prf) = Fact.d_equal e in
    Term.Set.fold
      (fun y s ->
	 try 
	   let (c, _) = apply s y in
	   let c' = Cnstrnt.replace x a c in
	     if c == c' then s else add y c' None s
	 with
	     Not_found -> s)
      (use s x) s

  and infer e s =  
    Trace.msg "foo" "C.infer" e Fact.pp_equal;
    let (x, a, prf1) = Fact.d_equal e in
      match a with
	| App(Arith(Num(q)), []) ->
	    restrict x s
	| _ ->
	    let s = 
	      try
		add x (of_term s a) None s
	      with
		  Not_found -> s
	    in
	      try
		let rec greater a l s =
		  match l with
		    | Cnstrnt.Low.Neginf -> s
		    | Cnstrnt.Low.Bound(alpha, b) ->
			ineq (Arith.mk_greater (a, alpha, b)) s
		and less a h s =
		  match h with
		    | Cnstrnt.High.Posinf -> s
		    | Cnstrnt.High.Bound(b, beta) ->
			ineq (Arith.mk_less (a, beta, b)) s
		and ineq ie s =
		  Trace.msg "foo" "Ineq" ie Arith.pp_ineq;
		  match ie with
		    | Arith.True -> 
			s
		    | Arith.False -> 
			raise Exc.Inconsistent
		    | Arith.Less(x, beta, b) ->
			let c = Cnstrnt.mk_less Dom.Real (b, beta) in
			  add x c None s
		    | Arith.Greater(x, alpha, a) ->
			let c = Cnstrnt.mk_greater Dom.Real (alpha, a) in
			  add x c None s
		in
		let (c, prf2) = apply s x in
		  Cnstrnt.fold
		    (fun (l, h) s -> greater a l (less a h s))
		    c s
	      with
		  Not_found -> s
      

(*
  (** Infer constraints from [x = a]. Constraints
    are propagated for all variables in [a]. Suppose [a] is of the 
    form [pre + q * y + post], then 
    [y in 1/q * (cpre - cpost)] 
    is derived, where [(x - pre) in cpre], with [cpre] an explicit constraint 
    and [post' in cpost], with [cpost]  is a symbolic constraint. *)
  and infer e s =  
    Trace.msg "foo" "C.infer" e Fact.pp_equal;
    let rec inferloop cpre ml s = 
      Trace.msg "foo" "C.inferloop" (cpre, ml) (Pretty.pair Cnstrnt.pp (Pretty.list Term.pp));
      if Cnstrnt.is_full cpre then  (* no further constraints derivable *)
	s
      else match ml with
	| [] -> s
	| m :: post ->    
	    let (q, y) = Arith.mono_of m in
	    let cpost =  try of_addl s post with Not_found -> Cnstrnt.mk_real in
	    let c = Cnstrnt.multq (Q.inv q) (Cnstrnt.subtract cpre cpost) in
	    let s = add y c None s in
	      try   
		let d = Cnstrnt.numeric_of (fst(apply s y)) in
		let cpre' = Cnstrnt.add cpre (Cnstrnt.multq (Q.minus q) d) in
		  inferloop cpre' post s
	      with
		  Not_found -> s
    in
    let (x, a, prf) = Fact.d_equal e in
      match a with
	| App(Arith(Num(q)), []) ->
	    restrict x s
	| App(Arith(Multq(q)), [y]) -> 
	    assert(not(Q.is_zero q));
	    let c = try fst(apply s x) with Not_found -> Cnstrnt.mk_real 
	    and d = try fst(apply s y) with Not_found -> Cnstrnt.mk_real in
	      add x (Cnstrnt.multq q d) None
		(add y (Cnstrnt.multq (Q.inv q) c) None s)
	| _ ->
	    (try
	       let (c, _) = apply s x in
	       let s = 
		 try 
		   add x (Cnstrnt.inter c (of_term s a)) None s
		 with 
		     Not_found -> s
	       in    
	       let c = Cnstrnt.numeric_of c in
	       let (q, ml) = Arith.poly_of a in
		 if ml = [] then s else 
		   inferloop (Cnstrnt.addq (Q.minus q) c) ml s
	     with
		 Not_found ->
		   (try
		      add x (of_term s a) None s
		    with
			Not_found -> s))
*)
  in
  let s = List.fold_right merge1 el s in
    (!es, s)


(** Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let diseq d s =
  Trace.msg "c1" "Diseq" d Fact.pp_diseq;
  s

	
(** Split. *)
let split s =
  Var.Map.fold
    (fun x (c, prf) acc ->
       if Cnstrnt.is_finite c then
	 failwith "to do"
       else 
	 acc)
    s.c Atom.Set.empty

