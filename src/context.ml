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
open Th

(** Decision procedure state. *)

type t = {
  mutable ctxt : Atom.Set.t;      (* Current context. *)
  mutable p : Partition.t;        (* Variable partitioning. *)
  eqs : Solution.t Th.Array.arr;  (* Theory-specific solution sets. *)
  mutable c : C.t;                (* Constraints. *)
  mutable upper : int;            (* Upper bound on fresh variable index. *)
}

let empty = {
  ctxt = Atom.Set.empty;
  p = Partition.empty;
  eqs = Array.create Solution.empty;
  c = C.empty;
  upper = 0
} 


(** Accessors for components of partitioning. *)

let ctxt_of s = s.ctxt
let p_of s = s.p
let v_of s = Partition.v_of s.p
let d_of s = Partition.d_of s.p
let c_of s = s.c
let eqs_of s = Array.get s.eqs
let upper_of s = s.upper


(** Access equalities, constraints, disequalities. *)

let cnstrnt_of s = C.cnstrnt s.c

let deqs_of s = Partition.disequalities s.p

(** Equality test. Do not take upper bounds into account. *)

let eq s t =              
  Partition.eq s.p t.p &&
  Array.for_all2 
    (fun eqs1 eqs2 -> 
       Solution.eq eqs1 eqs2) 
    s.eqs t.eqs


(** Destructive updates. *)

let extend a s = 
  (s.ctxt <- Atom.Set.add a s.ctxt; s)

let install s i eqs = 
  (Array.set s.eqs i eqs; s)

let update s i (p, eqs) =
  s.p <- p;
  Array.set s.eqs i eqs;
  s


(** Shallow copying. *)

let copy s = {
  ctxt = s.ctxt;
  p = Partition.copy s.p;
  eqs = Array.copy s.eqs;
  c = s.c;
  upper = s.upper}


(** Canonical variables module [s]. *)
	       
let v s = V.find (v_of s)
	    
let c s x = fst(C.apply s.c x)
	    
let d s = D.deq (d_of s)
	    
let fold s f x = V.fold (v_of s) f (v s x)


(** Parameterized operations on solution sets. *)

let mem i s = Solution.mem (eqs_of s i)
		
let use i s = Solution.use (eqs_of s i)

let apply i s = Solution.apply (eqs_of s i)

let find i s = Solution.find (eqs_of s i)

let rec inv i s = Solution.inv (eqs_of s i)


(** Constraint of [a] in [s]. *)

let cnstrnt s =
  Cnstrnt.of_term
    (fun x -> 
       (try
	  c s (v s x)
	with
	    Not_found -> 
	      (try
		 (match Arith.d_num (apply Th.la s x) with
		    | Some(q) -> Cnstrnt.mk_singleton q
		    | None -> Cnstrnt.mk_equal x)
	       with
		   Not_found -> 
		     Cnstrnt.mk_equal x)))

let is_int s a =
  try Cnstrnt.is_int (c s) a with Not_found -> false


(** Choosing a variable. *)

let choose s = V.choose (v_of s)


(** Pretty-printing. *)
 
let pp fmt s =
  let pps i sl =   
    if not(Solution.is_empty sl) then
      Solution.pp i fmt sl
  in
    Partition.pp fmt s.p;
    Array.iter (fun i eqs -> pps i eqs) s.eqs;
    C.pp fmt s.c


let equality i s = Solution.equality (eqs_of s i)


(** Variable partitioning. *)

let rec is_equal s x y =
  match  Term.is_equal x y with
    | Three.X -> Partition.is_equal s.p x y
    | res -> res


(** [sigma]-normal forms. *)

let sigma s f =
  match f with
    | Arith(op) -> Arith.sigma op
    | Product(op) -> Tuple.sigma op
    | Bv(op) -> Bitvector.sigma op
    | Coproduct(op) -> Coproduct.sigma op
    | Fun(op) -> Apply.sigma op
    | Pp(op) -> Pp.sigma op
    | Arrays(op) -> Arr.sigma op
    | Bvarith(op) -> Bvarith.sigma op
    | Uninterp _ -> mk_app f

(** Folding over the use list. *)

let folduse i x f s = 
  Set.fold
    (fun y s ->
       try
	 let b = apply i s y in
	   f (y, b) s
       with
	   Not_found -> s)
    (use i s x)
    s


(* Component-wise solver. Only defined for fully interpreted theories. *)

let rec solve i s e =
  let (a, b, _) = Fact.d_equal e in
    if !integer_solve && Th.eq i Th.la &&
      Cnstrnt.is_diophantine (c s) a &&
      Cnstrnt.is_diophantine (c s) b
    then
      zsolve e
    else
      Th.solve i e

and zsolve e =
  let (a, b, _) = Fact.d_equal e in 
  let sl = Arith.zsolve (a, b) in
    List.map (fun (x, c) -> Fact.mk_equal x c None) sl

and integer_solve = ref false

let fuse i e s =
  let is_inconsistent _ = false in
    update s i (Solution.fuse i is_inconsistent (s.p, eqs_of s i) [e])

let rec compose i e s =
  let is_inconsistent e =
    (Th.eq i Th.la) &&
    let (x, a, _) = Fact.d_equal e in
      try Cnstrnt.notin a (c s x) with Not_found -> false
  in
  let (a, b, prf) = Fact.d_equal e in
  let a' = find i s a 
  and b' = find i s b in
  let e' = Fact.mk_equal a' b' None in
    try
      let sl' = solve i s e' in
	let s' = update s i (Solution.compose i is_inconsistent (s.p, eqs_of s i) sl') in
	let s'' = if Th.eq i Th.la then cmerge sl' s' else s' in
	  s''
    with
	Exc.Unsolved ->   (* Incomplete Solver *)
	  ignore i e s

and cmerge el s =
  Trace.msg "rule" "Cmerge" el (Pretty.list Fact.pp_equal);
  let (es', c') = C.merge (apply Th.la s) (is_inconsistent s) el s.c in
    s.c <- c'; 
    Fact.Equalset.fold (compose Th.la) es' s

and is_inconsistent s x c =
  try
    let a = apply Th.la s x in
      Cnstrnt.notin a c
  with
      Not_found -> false

and ignore i e s =
  let (a, b, prf) = Fact.d_equal e in
  let (x', ei') = Solution.name i (a, eqs_of s i) in
  let (y', ei'') = Solution.name i (b, ei') in
  let e' = Fact.mk_equal x' y' None in
  let p' = Partition.merge e' s.p in
    s.p <- p'; s


(** Propagating a variable equality into all the other solution sets. *)
let propagate e s =           
  (compose Th.la e
     (cmerge [e]
	(compose Th.p e
	   (compose Th.bv e
	      (compose Th.cop e
		 (fuse Th.u e
		    (fuse Th.pprod e
		       (fuse Th.app e
			  (fuse Th.arr e
			     (fuse Th.bvarith e s))))))))))
     

(* Return a name for a nonvariable term. *)

let name i (s, b) =
  let (x', ei') = Solution.name i (b, eqs_of s i) in
    (install s i ei', x')

(* Lookup terms on rhs of solution sets. *)
    
let rec lookup s a = 
  match a with
    | Var _ ->
	v s a
    | App(f, _) ->
	let i = Th.of_sym f in
	  try 
	    let x = 
	      if Th.eq i Th.pprod then
		inv_pprod s a 
	      else 
		inv i s a
	    in
	      v s x
	  with 
	      Not_found -> a

(** Search for largest match on rhs. For example, if [a] is
 of the form [x * y] and there is an equality [u = x^2 * y],
 then [inv_pprod s a] returns [u * x] if there is no larger
 rhs which matches [a]. *)

and inv_pprod s a =           
  let rec usea acc = 
    match a with
      | App(Pp(Mult), xl) ->
	  (List.fold_left 
	     (fun acc' x -> 
		let (x', _) = Pp.destruct x in
		  Set.union (use Th.pprod s x') acc')
	     acc xl)
      | App(Pp(Expt(_)), [x]) -> 
	  use Th.pprod s x
      | _ -> 
	  acc
  in
  let lookup =
    Set.fold
      (fun x acc ->
	 try
	   let b = apply Th.pprod s x in
	     (match acc with
	       | None ->        (* [lcm = p * a = q * b = q * x] *)
		   let (p, q, lcm) = Pp.lcm (a, b) in
		     if Pp.is_one p then Some(q, x, b) else None
	       | Some(_, _, b') when Pp.cmp b b' <= 0 ->
		   acc
	       | _ ->
		   let (p, q, lcm) = Pp.lcm (a, b) in
		     if Pp.is_one p then Some(q, x, b) else acc)
	 with
	     Not_found -> acc)
      (usea Set.empty)
      None
  in
    match lookup with
      | Some(q, x, _) -> 
	  let a' = Pp.mk_mult q (v s x) in
	    inv_pprod s a'
      | None ->
	  a

(** List all constraints with finite extension. *)

let rec split s =
  Atom.Set.union 
    (split_cnstrnt s) 
    (split_arrays s)

and split_cnstrnt s = 
  C.split (c_of s)

and split_arrays s = 
  Solution.fold
    (fun _ (b,_) acc1 ->
       match b with
	 | App(Arrays(Select), [upd1; j1]) ->
	     V.fold (v_of s)
	     (fun upd2 acc2 ->
		try
		  (match apply arr s upd2 with
		     | App(Arrays(Update), [_; i2; _]) ->
			 (match is_equal s i2 j1 with
			    | X -> Atom.Set.add (Atom.mk_equal (i2, j1)) acc2
			    | _ -> acc2)
		     | _ -> 
			 acc1)
		with
		    Not_found -> acc1)
	     upd1 acc1
	 | _ -> acc1)
    (eqs_of s arr)
    Atom.Set.empty


(** Merging a variable equality *)

let rec merge e s =
  let p' = Partition.merge e s.p in
    s.p <- p';
    cmerge [e] s

let diseq d s =
  let p' = Partition.diseq d (p_of s) in
    s.p <- p'; s    


(** Adding a constraint. *)

let rec add c s =
  Trace.msg "rule" "Add" c Fact.pp_cnstrnt;
  let (es', c') = C.add (apply Th.la s) (is_inconsistent s) c s.c in
    s.c <- c';
    let s = Fact.Equalset.fold (compose Th.la) es' s in
      infer c s

and infer c s =
  let (x, c, prf) = Fact.d_cnstrnt c in
    folduse Th.la x
      (fun (y, b) s ->
	 let e = Fact.mk_equal y b None in
	   cmerge [e] s)
      s


(** Garbage collection. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)

let gc s =
  let xs = !V.removable in
  let filter = 
    Set.filter 
      (fun x -> 
	 not (mem u s x) &&      (* left-hand sides of these solution sets. *)
	 not (mem pprod s x) &&  (* are not kept in canonical form. *)
	 not (mem app s x) &&
         not (mem arr s x) &&
	 not (mem bvarith s x))
  in
  let xs' = filter xs in  
    if Set.is_empty xs' then s else
      let p' = Partition.restrict xs s.p in
	s.p <- p'; s


(** Administration of changed sets. For each of component [v], [d], [c] of the
 partition there is such a set stored in respective global variables [V.changed],
 [D.changed], and [C.changed]. Here, we define the change sets for the theory-specific
 solution sets. In addition, functions for saving, resetting, and restoring are provided. *)

module Changed = struct

  type t = Term.Set.t * Term.Set.t * Term.Set.t * Term.Set.t Array.arr

  let reset () =
    Solution.Changed.reset ();
    V.changed := Set.empty;
    D.changed := Set.empty;
    C.changed := Set.empty

  let save () =
    (!V.changed, 
     !D.changed, 
     !C.changed, 
     Solution.Changed.save ())

  let restore (v, d, c, e) =
    V.changed := v;
    D.changed := d;
    C.changed := c;
    Solution.Changed.restore e
    
  let stable () =
    !V.changed = Set.empty &&
    !D.changed = Set.empty &&
    !C.changed = Set.empty &&
    Solution.Changed.stable () 

  let in_v (v, _, _, _) = v
  let in_d (_, d, _, _) = d
  let in_c (_, _, c, _) = c
  let in_eqs i (_, _, _, e) = Array.get e i

  let pp fmt (v, d, c, e) =
    let ppset str xs = 
      if not(Set.is_empty xs) then
	begin
	  Format.fprintf fmt "\n%s: " str;
	  Pretty.set Term.pp fmt (Set.elements xs) 
	end 
    in
      ppset "v" v; ppset "d" d; ppset "c" c;
      Array.iter (fun i -> ppset (Th.to_string i)) e
 
end
  

(** Update rules work on the following global variables together with the index
 for creating new variables. Within a [protect] environment, updates are performed
 destructively. Global variables are protected. *)

let protect f s =
  let k' = !Var.k in
  let r' = !V.removable in
  let ch' = Changed.save () in
    try
      Var.k := s.upper;
      Changed.reset ();
      V.removable := Term.Set.empty;
      let s' = f (copy s) in
	s'.upper <- !Var.k;
	Var.k := k';
	V.removable := r';
	Changed.restore ch';
	s'
    with
      | exc ->
	  Var.k := k';
	  V.removable := r';
	  Changed.restore ch';
	  raise exc


(** {6 Canonization} *)

module Can = struct

  (* Don't use [find] for uninterpreted theory. *)

  let rec fnd th s a  =
    if Th.eq th Th.u || Th.eq th Th.app then  
      a
    else if Th.is_fully_interp th then
      let b = find th s a in  (* Context.find *)
	b
    else 
      findequiv th s a
	
  and findequiv th s a =
    try
      choose s
	(fun x ->
	   try Some(apply th s x) 
	   with Not_found -> None)
	a
    with
	Not_found -> a
	
  (** {6 Canonization of terms} *)
	
  let rec term s =
    Trace.func "canon" "Term" Term.pp Term.pp
      (can s)
      
  and can s a =
    match a with
      | Var _ ->
	  v s a
      | App(Sym.Arith(op), al) ->
	  arith s op al
      | App(Sym.Bvarith(Sym.Unsigned), [x]) ->
	  unsigned s x
      | App(Sym.Pp(op), xl) ->
	  pprod s op xl
      | App(f, al) ->
	  let th = Th.of_sym f in
	  let interp x = fnd th s (can s x) in
	  let al' = mapl interp al in
	  let a' = if al == al' then a else sigma s f al' in
	    lookup s a'

  and pprod s op al =   
    match op, al with
      | Expt(n), [x] ->
	  lookup s (Sig.mk_expt n (fnd Th.la s (can s x)))
      | Mult, xl ->
	  lookup s (Sig.mk_multl (Term.mapl (fun x -> fnd Th.la s (can s x)) xl))
      | _ ->
	  assert false
	  
  and unsigned s x =
    lookup s (Bvarith.mk_unsigned (fnd Th.bv s (can s x)))
      
  and arith s op l =       (* special treatment for arithmetic *)
    match op, l with       (* for optimizing memory usage. *)
      | Sym.Num(q), [] -> 
	  lookup s (Arith.mk_num q)
      | Sym.Multq(q), [x] -> 
	  let y = can s x in
	    lookup s (Arith.mk_multq q (fnd Th.la s y))
      | Sym.Add, [x; y] -> 
	  let a' = fnd Th.la s (can s x) 
	  and b' = fnd Th.la s (can s y) in
	    lookup s (Arith.mk_add a' b')
      | Sym.Add, _ :: _ :: _ -> 
	  let f a = fnd Th.la s (can s a) in
	  let l' =  mapl f l in
	    lookup s (Arith.mk_addl l')
      | _ ->  
	  let str = "Ill-formed term " ^ 
		    (Pretty.to_string Sym.pp (Sym.Arith(op))) ^
		    (Pretty.to_string (Pretty.list Term.pp) l)
	  in
	    failwith str
	      
  let eq s a b =
    Term.eq (term s a) (term s b)
      

(** {6 Canonization and normalization of atoms} *)

  let rec atom s = 
    Trace.func "canon" "Atom" Atom.pp Atom.pp
      (function 
	 | Atom.True -> Atom.True
	 | Atom.Equal(a, b) -> equal s (a, b)
	 | Atom.Diseq(a, b) -> diseq s (a, b)
	 | Atom.Less(a, kind, b) ->  less s (a, kind, b)
	 | Atom.Greater(a, kind, b) -> greater s (a, kind, b)
	 | Atom.In(a, d) -> cnstrnt s (a, d)
	 | Atom.False -> Atom.False)
      
  and equal s (a, b) = 
    let x' = can s a and y' = can s b in
      match is_equal s x' y' with
	| Three.Yes ->
	    Atom.mk_true
	| Three.No -> 
	    Atom.mk_false
	| Three.X -> 
	    let (x'', y'') = crossmultiply s (x', y') in
	      Atom.mk_equal (x'', y'')
 
  and diseq s (a, b) =
    let x' = can s a and y' = can s b in
      match is_equal s x' y' with
	| Three.Yes -> 
	    Atom.mk_false
	| Three.No -> 
	    Atom.mk_true
	| Three.X ->
	    let (x'', y'') = crossmultiply s (x', y') in
	      Atom.mk_diseq (x'', y'')

  and crossmultiply s (a, b) =
    let (a', b') = crossmultiply1 s (a, b) in
      if Term.eq a a' && Term.eq b b' then
	(a, b)
      else 
	let (a'', b'') = crossmultiply s (a', b') in
	  (can s a'', can s b'')

  and crossmultiply1 s (a, b) =
    let da = Pp.denumerator a in
    let db = Pp.denumerator b in
    let (_, _, d) = Pp.lcm (da, db) in
      if Pp.is_one d then (a, b) else
	(Sig.mk_mult a d, Sig.mk_mult b d)

  and less s (x, beta, b) =   (* [x <(=) b] *)
    let x = can s x
    and b = fnd Th.la s (can s b) in (* use arithmetic interp if possible *)
      try                           
	let c = c s x in
	let d = Cnstrnt.mk_less Dom.Real (b, beta) in
	  (match Cnstrnt.cmp c d with
	     | Cnstrnt.Super -> 
		 Atom.mk_less (x, beta, b)
	     | (Cnstrnt.Sub | Cnstrnt.Same) ->
		 Atom.mk_true
	     | Cnstrnt.Disjoint ->
		 Atom.mk_false
	     | Cnstrnt.Overlap ->
		 Atom.mk_less (x, beta, b))
      with
	  Not_found ->
	    Atom.mk_less (x, beta, b)
	    
  and greater s (x, alpha, a) =  (* [x >(=) a] *)
    let x = can s x
    and a =  fnd Th.la s (can s a) in
      try                           
	let c = c s x in
	let d = Cnstrnt.mk_greater Dom.Real (alpha, a) in
	  (match Cnstrnt.cmp c d with
	     | Cnstrnt.Super -> 
		 Atom.mk_greater (x, alpha, a)
	     | (Cnstrnt.Sub | Cnstrnt.Same) ->
		 Atom.mk_true
	     | Cnstrnt.Disjoint ->
		 Atom.mk_false
	     | Cnstrnt.Overlap ->
		 Atom.mk_greater (x, alpha, a))
      with
	  Not_found ->
	    Atom.mk_greater (x, alpha, a)

  and cnstrnt s (a, d) =
    let a = can s a in
      Atom.mk_in (a, d)

  let eq s a b =
    Term.eq (term s a) (term s b)

end


(** {6 Abstraction} *)

module Abstract = struct

  let rec equal (s, e) =
    let (a, b, _) = Fact.d_equal e in
    let (s', x') = toplevel_term (s, a) in
    let (s'', y') = toplevel_term (s', b) in
    let e' = Fact.mk_equal x' y' None in
      (s'', e')

  and diseq (s, d) =
    let (a, b, _) = Fact.d_diseq d in
    let (s', x') = toplevel_term (s, a) in
    let (s'', y') = toplevel_term (s', b) in
    let d' = Fact.mk_diseq x' y' None in
      (s'', d')
 
  and cnstrnt (s, c) =  
    let (a, i, _) = Fact.d_cnstrnt c in
    let (s', a') = toplevel_term (s, a) in
    let c' = Fact.mk_cnstrnt a' i None in
      (s', c')

  and toplevel_term (s, a) =
    term u (s, a)

  and term i (s, a) =
    match a with
      | Var _ -> 
	  (s, a)
      | App(f, al) ->
	  let j = Th.of_sym f in
	  let (s', al') = args j (s, al) in
	  let a' = if Term.eql al al' then a else sigma s f al' in
	    if i = u || i = arr || i <> j then
	      try
		let x' = inv j s' a' in
		  (s', v s' x')
	      with 
		  Not_found -> name j (s', a')
	    else 
	      (s', a')
	    
  and args i (s, al) =
    match al with
      | [] -> 
	  (s, [])
      | b :: bl ->
	  let (s', bl') = args i (s, bl) in
	  let (s'', b') = term i (s', b) in
	    if Term.eq b b' && bl == bl' then
	      (s'', al)
	    else 
	      (s'', b' :: bl')
end 


	
(** Sequential composition *)
let (&&&) f g x = g (f x)

module Fold = struct

  (** Applying a rule [f] for all equalities [x = a] in theory-specific solution set [s]
    of index [i] such that [x] is equivalent to some [y] in the changed set [ch]. *)
  let equal i f =
    Set.fold 
      (fun x s -> 
	 try f (equality i s (v s x)) s with Not_found -> s)
      
  (** Applying rule [f d] for all disequalities [d] in [s] of the form [x <> y] where [x]
    is in [ch], and accumulating the results. *)
  let diseq f =
    Set.fold 
      (fun x s -> 
	 try List.fold_right f (deqs_of s x) s with Not_found -> s)

  let cnstrnt f = 
    Set.fold
      (fun x s -> 
	 try f (C.cnstrnt s.c x) s with Not_found -> s)

end 


module Arrays = struct

  (** Propagate variable equalities and disequalities into array equalities. *)
  let rec propagate ch =
    Fold.equal Th.arr equal (Changed.in_v ch) &&&  
    Fold.diseq diseq (Changed.in_d ch)
      

  (** From the equality [x = y] and the facts
    [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
    and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
    that [z1 = k2]. *)

  and equal e s =
    Trace.msg "rule" "Array_equal" e Fact.pp_equal;
    let (x, y, prf) = Fact.d_equal e in
      equal1 (x, y, prf)
	(equal1 (y, x, prf) s)

  and equal1 (x, y, prf) s =
    Set.fold
      (fun z1 s1 -> 
	 match apply Th.arr s1 z1 with 
	   | App(Arrays(Select), [upd1; j1])
	       when is_equal s1 y j1 = Three.Yes ->
	       fold s1
		 (fun z2 s2  -> 
		    match apply Th.arr s2 z2 with 
		      | App(Arrays(Update), [a2; i2; k2])
			  when is_equal s2 x i2 = Three.Yes -> 
			  let e' = Fact.mk_equal (v s2 z1) (v s2 k2) None in
			    merge e' s2
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

  and diseq d s =
    Trace.msg "rule" "Array_diseq" d Fact.pp_diseq;
    let (i, j, prf) = Fact.d_diseq d in
      diseq1 (i, j, prf)
	(diseq1 (j, i, prf) s)

  and diseq1 (i, j, prf) s =
    Set.fold
      (fun z1 s1 -> 
	 match apply Th.arr s1 z1 with
	   | App(Arrays(Select), [upd; j']) 
	       when is_equal s1 j j' = Three.Yes ->
	       fold s 
		 (fun z2 s2 ->
		    match apply Th.arr s2 z2 with
		      | App(Arrays(Update), [a; i'; _])
			  when is_equal s2 i i' = Three.Yes ->
			  let (s', z3) = name Th.arr (s2, Arr.mk_select a j) in
			  let e' = Fact.mk_equal (v s2 z1) (v s2 z3) None in
			    merge e' s'
		      | _ -> s2)
		 upd s1
	   | _ -> s)    
      (use Th.arr s j)
      s
end


module Bvarith = struct

  (** Propagate variable equalities and disequalities into array equalities. *)
  let rec propagate ch =
    Fold.equal Th.bv equal (Changed.in_eqs Th.bv ch) &&&
    Fold.equal Th.bvarith deduce (Changed.in_eqs Th.bvarith ch)
      
  and equal e s =
    Trace.msg "rule" "Bvarith" e Fact.pp_equal;
    let (x, bv, prf) = Fact.d_equal e in
      Set.fold
	(fun u s ->
	   try
	     match apply bvarith s u with
	       | App(Bvarith(Unsigned), [x'])
		   when Term.eq x x' ->
		   let ui = Bvarith.mk_unsigned bv in
		   let (s', a') = Abstract.term la (s, ui) in
		   let e' = Fact.mk_equal (v s' u) a' None in
		     compose Th.la e' s'
	       | _ ->
		   s 
	     with
		 Not_found -> s)
	(use bvarith s x)
	s
	
  and deduce e s = 
    Trace.msg "rule" "Bvarith_deduce" e Fact.pp_equal;
    let (x, b, _) = Fact.d_equal e in
      match b with
	| App(Bvarith(Unsigned), [y]) ->   (* [x = unsigned(y)] *)      
	    let c = Fact.mk_cnstrnt (v s x) Cnstrnt.mk_nat None in
	      add c s
	| _ -> 
	    s
end


module Nonlin = struct

  let rec propagate ch =
    (deduce (Changed.in_eqs Th.pprod ch)) &&&
    (linearize (Changed.in_eqs Th.la ch)) &&&
    (cnstrnt (Changed.in_c ch))

  and linearize ch s =
    let occs = 
      Set.fold 
	(fun x -> 
	   (Set.union (use Th.pprod s x))) 
	ch Set.empty
    in
      Set.fold 
	(fun x s -> 
	   try linearize1 (equality Th.pprod s x) s 
	   with Not_found -> s)
	occs s

  and linearize1 e s =
    let (x, a, prf) = Fact.d_equal e in
    let b = 
      Pp.fold
	(fun x n -> 
	   Sig.mk_mult 
	   (Sig.mk_expt n (find Th.la s x)))
	a
	Pp.mk_one
    in
    let (s', b') = Abstract.toplevel_term (s, b) in
    let e' = Fact.mk_equal (v s x) b' None in
      merge e' s'

  and deduce ch s =
    Set.fold 
      (fun x s -> 
	 try deduce1 (equality Th.pprod s x) s 
	 with  Not_found -> s)
      ch s

  and deduce1 e s =
    let (x, b, _) = Fact.d_equal e in
      try
	let c = Pp.cnstrnt (c s) b in
	let cnstrnt = Fact.mk_cnstrnt (v s x) c None in
	  add cnstrnt s
      with
	  Not_found -> s

  and cnstrnt ch =
    Set.fold
      (fun x s -> 
	 try cnstrnt1 (cnstrnt_of s x) s
	 with Not_found -> s)
    ch

  and cnstrnt1 c =
    let (x, i, _) = Fact.d_cnstrnt c in
      folduse Th.pprod x
	(fun (y, b) -> deduce1 (Fact.mk_equal y b None))

end
		


(** {6 Confluence} *)

let maxclose = ref 20
let compactify = ref true


module Rule = struct

  (** Propagating merged equalities into theory-specific solution sets. *)
  let prop =
    Set.fold
      (fun x s -> 
	 try 
	   propagate (V.equality (v_of s) x) s 
	 with 
	     Not_found -> s)

  let normalize s =
    if !compactify then gc s else s
	
  (** [close s] applies the rules above until the resulting state is unchanged. *)
 
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
    prop (Changed.in_v ch) &&& 
    Bvarith.propagate ch &&&           (* Propagate into arithmetic interps. *)
    Arrays.propagate ch                (* Propagate into arrays. *)

end

(** {6 Adding new atoms} *)

module Status = struct

  type 'a t = 
    | Valid 
    | Inconsistent
    | Ok of 'a

  let pp pp fmt = function
    | Valid -> Format.fprintf fmt ":valid"
    | Inconsistent -> Format.fprintf fmt ":unsat"
    | Ok(x) -> Format.fprintf fmt ":ok "; pp fmt x

end

module Process = struct

  let rec atom s =
    Trace.func "shostak" "Process" Atom.pp (Status.pp pp)
      (fun atom ->
	 try
	   match Can.atom s atom with
	     | Atom.True -> 
		 Status.Valid
	     | Atom.False ->
		 Status.Inconsistent
	     | Atom.Equal(a, b) ->
		 let e = Fact.mk_equal a b Fact.mk_axiom in
		   Status.Ok(merge_atom atom e s)
	     | Atom.Diseq(a, b) -> 
		 let d = Fact.mk_diseq a b Fact.mk_axiom in
		   Status.Ok(diseq_atom atom d s)
	     | Atom.Less(x, kind, a) ->
		 let c = Cnstrnt.mk_less Dom.Real (a, kind) in
		   Status.Ok(add_atom atom (Fact.mk_cnstrnt x c Fact.mk_axiom) s)
	     | Atom.Greater(x, kind, a) ->
		 let c = Cnstrnt.mk_greater Dom.Real (kind, a) in
		   Status.Ok(add_atom atom (Fact.mk_cnstrnt x c Fact.mk_axiom) s)
	     | Atom.In(a, d) ->
		 let c = Fact.mk_cnstrnt a (Cnstrnt.mk_dom d) Fact.mk_axiom in
		   Status.Ok(add_atom atom c s)
	   with 
	       Exc.Inconsistent -> 
		 Status.Inconsistent)
      
  and merge_atom a e =  
    protect
      (fun s  ->
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', e') = Abstract.equal (s, e) in
	 let s'' = merge e' s' in
	   Rule.close s'')
      
  and add_atom a c = 
    protect
      (fun s  -> 
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', c') = Abstract.cnstrnt (s, c) in
	 let s'' = add c' s' in
	   Rule.close s'')

  and diseq_atom a d =
    protect
      (fun s  ->   
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', d') = Abstract.diseq (s, d) in
	 let s'' = diseq d' s' in
	   Rule.close s'')

end
 
let add = Process.atom
