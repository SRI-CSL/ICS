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


(** {6 Decision procedure state} *)

type t = {
  mutable ctxt : Atom.Set.t;      (* Current context. *)
  mutable p : Partition.t;        (* Variable partitioning. *)
  eqs : Solution.t Th.Array.arr;  (* Theory-specific solution sets. *)
  mutable sl : Sl.t;              (* Slack equalities. *)
  mutable upper : int;            (* Upper bound on fresh variable index. *)
}


let empty = {
  ctxt = Atom.Set.empty;
  p = Partition.empty;
  eqs = Array.create Solution.empty;
  sl = Sl.empty;
  upper = 0
} 


(** {6 Accessors} *)

let ctxt_of s = s.ctxt
let p_of s = s.p
let v_of s = Partition.v_of s.p
let d_of s = Partition.d_of s.p
let c_of s = Partition.c_of s.p
let eqs_of s = Array.get s.eqs
let sl_of s = s.sl
let upper_of s = s.upper


(** Equality test. Do not take upper bounds into account. *)
let eq s t =              
  Partition.eq s.p t.p &&
  (Array.for_all2 
    (fun eqs1 eqs2 -> 
       Solution.eq eqs1 eqs2) 
    s.eqs t.eqs) &&
  Sl.eq s.sl t.sl


(** Destructive updates. *)
let update s i eqs =
  (Array.set s.eqs i eqs; s)


(** Shallow copying. *)
let copy s = {
  ctxt = s.ctxt;
  p = Partition.copy s.p;
  eqs = Array.copy s.eqs;
  sl = s.sl;
  upper = s.upper
}

(** Canonical variables module [s]. *)
	       
let v s x = fst(Partition.v s.p x)

let d s x = List.map fst (Partition.d s.p (v s x))

let c s a = 
  let rec term a =
    match a with
      | Var _ -> fst(Partition.c s.p a)
      | Term.App(Arith(op), xl) -> arith op xl
      | Term.App(Pp(op), xl) -> pprod op xl
      | Term.App(Bvarith(op), [x]) -> bvarith op x
      | a -> raise Not_found
  and arith op al = 
    try
      match op, al with
	| Num(q), [] -> Sign.of_q q
	| Multq(q), [x] -> Sign.multq q (term x)
	| Add, [x; y] -> Sign.add (term x) (term y)
	| Add, xl -> Sign.addl (List.map term xl)
	| _ -> assert false
      with
	  Not_found -> Sign.T
  and bvarith op a =
    match op with
      | Unsigned -> Sign.Nonneg
  and pprod op al =
    try
      match op, al with
	| Expt(n), [x] -> Sign.expt n (try term x with Not_found -> Sign.T)
	| Mult, [] -> Sign.of_q Q.one
	| Mult, [x] -> term x
	| Mult, [x; y] -> Sign.mult (term x) (term y)
	| Mult, xl -> Sign.multl (List.map term xl)
	| _ -> assert false
      with
	  Not_found -> Sign.T
  in
    term a

let sl s = Sl.find s.sl
  
let fold s f x = 
  let y = v s x in
    V.fold (v_of s) f y


(** Parameterized operations on solution sets. *)

let mem i s = Solution.mem (eqs_of s i)
	
let use i s = Solution.use (eqs_of s i)

let apply i s = Solution.apply (eqs_of s i)

let find i s = Solution.find (eqs_of s i)

let inv i s = Solution.inv (eqs_of s i)


(** Constraint of [a] in [s]. *)

let dom s a =
  let rec of_term = function
    | App(Arith(op), xl) -> of_arith op xl
    | App(Pp(op), xl) -> of_pprod op xl
    | App(Bvarith(op), xl) -> of_bvarith op xl
    | a -> if is_intvar a then Dom.Int else if is_realvar a then Dom.Real else raise Not_found
  and of_arith op xl =
    match op, xl with
      | Num(q), [] ->
	  if Q.is_integer q then Dom.Int else Dom.Real
      | Multq(q), [x] -> 
	  if Q.is_integer q && of_term x = Dom.Int then Dom.Int else Dom.Real
      | Add, xl -> 
	  if List.for_all (fun x -> Dom.eq (of_term x) Dom.Int) xl then Dom.Int else Dom.Real
      | _ -> 
	  Dom.Real
  and of_pprod op xl = 
    match op, xl with
      | Mult, _ -> 
	  if List.for_all (fun x -> Dom.eq (of_term x) Dom.Int) xl then Dom.Int else Dom.Real
      | Expt(n), [x] ->
	  if n >= 0 && of_term x = Dom.Int then Dom.Int else Dom.Real
      | _ ->
	  Dom.Real
  and of_bvarith op xl =
    match op, xl with
      | Unsigned, [_] -> Dom.Int
      | _ -> Dom.Real
  in
    of_term a

let is_int s a = 
  try Dom.eq (dom s a) Dom.Int with Not_found -> false


(* Return a name for a nonvariable term. *)
let name i (s, b) =
  let (x', ei') = Solution.name i (b, eqs_of s i) in
    (update s i ei', x')



(** Pretty-printing. *)
 
let pp fmt s =
  let pps i sl =   
    if not(Solution.is_empty sl) then
      Solution.pp i fmt sl
  in
    Partition.pp fmt s.p;
    Array.iter (fun i eqs -> pps i eqs) s.eqs;
    if not(Sl.is_empty s.sl) then
      Sl.pp fmt s.sl

let equation i s = Solution.equality (eqs_of s i)


(** Variable partitioning. *)

let rec is_equal s x y =
  match  Term.is_equal x y with
    | Three.X -> Partition.is_equal s.p x y
    | res -> res

(** Constraint of a term. *)

let rec cnstrnt s a =
  try
    c s a 
  with
      Not_found ->
	c s (apply Th.la s a)


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
    let choose s = V.choose (v_of s) in
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
      | Var _ -> v s a 
      | App(Sym.Arith(op), al) -> arith s op al
      | App(Sym.Bvarith(Sym.Unsigned), [x]) -> unsigned s x
      | App(Sym.Pp(op), xl) -> pprod s op xl
      | App(f, al) ->
	  let th = Th.of_sym f in
	  let interp x = fnd th s (can s x) in
	  let al' = mapl interp al in
	  let a' = if al == al' then a else Th.sigma f al' in
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
	 | Atom.In(a, d) -> sign s (a, d)
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

  and sign s (a, i) =
    let a' = can s a in
      try
	let j = cnstrnt s a' in
	let k = Sign.inter i j in
	  if Sign.eq k j then Atom.mk_true 
	  else if k = Sign.F then Atom.mk_false
	  else Atom.mk_in (a', k)
      with
	  Not_found -> Atom.mk_in (a', i)
	    
  let eq s a b =
    Term.eq (term s a) (term s b)

end

let can = Can.term

(** {6 Abstraction} *)


module Abstract = struct

  let rec equal (s, e) =
    let (a, b, _) = Fact.d_equal e in
      match a, b with
	| Var _, Var _ -> (s, e)
	| Var _, App(f, _) ->
	    let i = Th.of_sym f in
	    let (s', y') = term i (s, b) in
	    let e' = Fact.mk_equal a y' None in
	      (s', e')
	| App(f, _), Var _ ->
	    let i = Th.of_sym f in
	    let (s', x') = term i (s, a) in
	    let e' = Fact.mk_equal b x' None in
	      (s', e')
	| App(f, _), App(g, _) -> 
	    let i = Th.of_sym f and j = Th.of_sym g in
	    let k = u in
	    let (s', x') = term k (s, a) in
	    let (s'', y') = term k (s', b) in
	    let e' = Fact.mk_equal x' y' None in
	      (s'', e')

  and diseq (s, d) =
    let (a, b, _) = Fact.d_diseq d in
    let (s', x') = term u (s, a) in
    let (s'', y') = term u (s', b) in
    let d' = Fact.mk_diseq x' y' None in
      (s'', d')

  and cnstrnt (s, i) =
    let (a, i, _) = Fact.d_cnstrnt i in
    let (s', x') = term la (s, a) in
    let l' = Fact.mk_cnstrnt  x' i None in
      (s', l')

  and toplevel_term (s, a) =
    term u (s, a)

  and term i (s, a) =
    match a with
      | Var _ -> 
	  (s, a)
      | App(f, al) ->
	  let j = Th.of_sym f in
	  let (s', al') = args j (s, al) in
	  let a' = if Term.eql al al' then a else Th.sigma f al' in
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


      

(* Processing an equality. *)

let rec equality e s =
  Trace.msg "rule" "Assert" e Fact.pp_equal;
  let (a, b, _) = Fact.d_equal e in
    match a, b with
      | Var _, Var _ -> 
	  merge e s 
      | App(f, _), _ -> 
	  compose (Th.of_sym f) e s
      | _, App(f, _) -> 
	  compose (Th.of_sym f) e s

and merge e s =
  let (x, y, prf) = Fact.d_equal e in
    match Partition.is_equal s.p x y with
      | Three.Yes -> 
	  s
      | Three.No -> 
	  raise Exc.Inconsistent
      | Three.X ->
	  Trace.msg "rule" "Merge" e Fact.pp_equal;
	  let (ch', p') = Partition.merge e s.p in
	    s.p <- p';
	    let s' = propagate e s in
	    let s'' = arrays_equal e s' in
	      close ch' s''

(** Propagating a variable equality into all the other solution sets. *)
and propagate e s =           
  compose Th.la e
    (compose Th.p e
       (compose Th.bv e
	  (compose Th.cop e
	     (fuse Th.u e
		(fuse Th.pprod e
		   (fuse Th.app e
		      (fuse Th.arr e
			 (fuse Th.bvarith e s)))))))) 


(** From the equality [x = y] and the facts
  [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
  and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
  that [z1 = k2]. *)

and arrays_equal e s =
  let equal (x, y, prf) s =
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
  in
  let (x, y, prf) = Fact.d_equal e in
    equal (x, y, prf)
      (equal (y, x, prf) s)

and bvarith_equal e s =
  let (x, bv, prf) = Fact.d_equal e in
    Set.fold
      (fun u s ->
	 try
	   (match apply bvarith s u with
	      | App(Bvarith(Unsigned), [x'])
		  when Term.eq x x' ->
		  let ui = Bvarith.mk_unsigned bv in
		  let (s', a') = Abstract.term la (s, ui) in
		  let e' = Fact.mk_equal (v s' u) a' None in
		    compose Th.la e' s'
	      | _ ->
		  s )
	 with
	     Not_found -> s)
      (use bvarith s x)
      s

and nonlin_equal e s =
  Trace.msg "rule" "Nonlin" e Fact.pp_equal;
  let rec linearize occs s =
    Set.fold 
      (fun x s -> 
	 try 
	   let a = apply Th.pprod s x in
	   let b = Sig.map (find Th.la s) a in
	     Trace.msg "foo" "Inst" b Term.pp;
	     if Term.eq a b then s else 
	       let (s', b') = Abstract.toplevel_term (s, b) in
	       let e' = Fact.mk_equal (v s' x) b' None in
		 merge e' s'
	 with
	     Not_found -> s)
      occs s
  in
  let (x, _, _) = Fact.d_equal e in
    linearize (use Th.pprod s x) s

and fuse i e s =   
  let (x, _, _) = Fact.d_equal e in   
  let (ch', es', si') = Solution.fuse i (eqs_of s i) [e] in
  let s' = Fact.Equalset.fold merge es' s in
    update s' i si'

and compose i e s =
  let (a, b, prf) = Fact.d_equal e in
  let a' = find i s a 
  and b' = find i s b in
  let e' = Fact.mk_equal a' b' None in
  let sl' = Th.solve i e' in
  let s = if Th.eq i Th.la then List.fold_right slack sl' s else s in
  let (ch', es', si') = Solution.compose i (eqs_of s i) sl' in
  let s' =  Fact.Equalset.fold merge es' s in
  let s'' = update s' i si' in
  let s''' = 
    Set.fold
      (fun x acc ->
	   try
	     let e' = Solution.equality (eqs_of acc i) x in
	     let acc' =  if Th.eq i Th.la then nonlin_equal e' acc else acc in
	       deduce i e' acc'
	   with
	       Not_found -> acc)
      ch' s''
  in
    s'''

and slack e s =
  let (k, a, _) = Fact.d_equal e in
    if is_slack k then
      let sl' = Sl.update e s.sl in
	s.sl <- sl';
	try      (* Keep [cnstrnt s k] stronger than [cnstrnt s (sl s a)] *)
	  refine (Fact.mk_cnstrnt k (c s a) None) s
	with
	    Not_found -> s
    else 
      s

and refine c s =
  let (ch', p') = Partition.add c s.p in
    s.p <- p'; 
    close ch' s 





(** Deduce new constraints from an equality *)
and deduce i e s =
  if Th.eq i Th.la then
    deduce_la e s
  else if Th.eq i Th.bvarith then
    deduce_bvarith e s
  else if Th.eq i Th.pprod then
    deduce_nonlin e s
  else 
    s

and deduce_bvarith e s = 
  let (x, b, _) = Fact.d_equal e in
    match b with
      | App(Bvarith(Unsigned), [y]) ->   (* [x = unsigned(y)] *)      
	  let c = Fact.mk_cnstrnt (v s x) Sign.T None in
	    add c s
      | _ -> 
	  s

and deduce_nonlin e s =
  let (x, a, _) = Fact.d_equal e in
  let x' = v s x in
  try
    let j = c s a in
      (try
	 let i = c s x' in
	   if Sign.sub j i then s else 
	     refine (Fact.mk_cnstrnt x' j None) s
       with
	   Not_found -> 
	     refine (Fact.mk_cnstrnt x' j None) s)
  with
      Not_found -> s


(* If C(k) = 0, then we can assert that S(k) = 0.
   Inequality Propagation: If Sl(k) = R+[k'] - R-[k''],
   then if either C(k) or C[R-[k'']] goes from >= to >,
   then we can assert R+[k']>0. *) 

and deduce_la e s =
  let inconsistent (x, a) =
    try
      (dom s x = Dom.Int) &&
      (match Arith.d_num a with
	 | Some(q) -> not(Q.is_integer q)
	 | None -> false)
    with
	Not_found -> false
  in
  let (k, sk, prf) = Fact.d_equal e in
    if inconsistent (k, sk) then
      raise Exc.Inconsistent
    else if not(is_slack k) then 
      s 
    else  
      begin
	Trace.msg "rule" "Deduce" e Fact.pp_equal;
	try  
	  let c_sk = c s sk in  (* Keep invariant that [c s k] is stronger than [cnstrnt s sk]. *)
	  let s = add (Fact.mk_cnstrnt k c_sk None) s in 
	  let c_k = c s k in 
	    if c_k = Sign.Zero then
	      equality (Fact.mk_equal k Arith.mk_zero None) s
	    else 
	      let (r_plus, r_minus) = partition s (sl s k) in
		if c_k = Sign.Pos || c s r_minus = Sign.Pos then
		  if c s r_plus = Sign.Pos then s else 
		    add (Fact.mk_cnstrnt r_plus Sign.Pos None) s 
		else 
		  s
	with
	    Not_found -> s (* should not happen *)
      end 
	    
and partition s a =
    let sign_of_monomial = function
      | App(Arith(Num(q)), []) -> Sign.of_q q
      | App(Arith(Multq(q)), [k]) -> Sign.multq q (c s k)
      | k -> cnstrnt s k
    in
    let rec loop posl negl = function
      | [] -> (Arith.mk_addl posl, Arith.mk_addl negl)
      | m :: ml ->
	  (match sign_of_monomial m with
	     | (Sign.Pos | Sign.Nonneg | Sign.Zero) ->
		 loop (m :: posl) negl ml
	     | (Sign.Neg | Sign.Nonpos) -> 
		 loop posl (Arith.mk_neg m :: negl) ml
	     | Sign.F -> raise Exc.Inconsistent  (* following should not happen *)
	     | Sign.T -> raise Not_found)
    in
      loop [] [] (Arith.monomials a)
	   
	

(** Merging variable equality/disequalities/constraints *)
and diseq d s =
  Trace.msg "rule" "Diseq" d Fact.pp_diseq;
  let (x, y, _) = Fact.d_diseq d in
  let (ch', p') = Partition.diseq d s.p in
  let s' = arrays_diseq d s in
    close ch' s'
    
(** Propagating a disequalities.
  From the disequality [i <> j] and the facts
  [z1 = select(upd, j')], [z2 = update(a,i',x)],
  [i = i'], [j = j'], [upd = z2], it follows that
  [z1 = z3], where [z3 = select(a,j)]. *)
and arrays_diseq d s =
  let diseq (i, j, prf) s =
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
  in
  let (x, y, prf) = Fact.d_diseq d in
    diseq (x, y, prf)
      (diseq (y, x, prf) s)

and add c s =
  Trace.msg "rule" "Add" c Fact.pp_cnstrnt;
  let normalize (a, i) =
  match i with
    | Sign.Neg -> (Arith.mk_neg a, Sign.Pos)
    | Sign.Nonpos -> (Arith.mk_neg a, Sign.Nonneg)
    | _ -> (a, i)
  in
  let (a, i, prf) = Fact.d_cnstrnt c in
    match i with
      | Sign.F ->
	  raise Exc.Inconsistent
      | Sign.Zero -> 
	  equality (Fact.mk_equal a Arith.mk_zero None) s
      | Sign.T ->
	  s
      | _ ->
	  let b = Can.term s a in
	  let (b, i) = normalize (b, i) in
	    if is_slack b then
	      refine (Fact.mk_cnstrnt b i prf) s
	    else 
	      let d = if is_int s a then Some(Dom.Int) else None in
	      let alpha = if i = Sign.Pos then false else true in
	      let k = Term.mk_slack None alpha d in
		equality (Fact.mk_equal k a None)
		  (refine (Fact.mk_cnstrnt k i None) s)


(** Propagate changes in the variable partitioning. *)    
and close ch s =
  let s' = infer (ch.Partition.chv) s in
    s'

and infer chc s =
    Set.fold
      (fun x s ->
	 try
	   let e = equation Th.la s x in
	     deduce Th.la e s
	 with
	     Not_found ->
	       (Set.fold 
		  (fun y s ->
		     try deduce Th.la (equation Th.la s y) s
		     with Not_found -> s)
		  (use Th.la s x)
		  s))
      chc s


(** Garbage collection. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)


let compactify = ref true

let rec normalize s =
  if not (!compactify) then s else gc s

and gc s =
  let filter x =  
    not (mem u s x) &&      (* left-hand sides of these solution sets. *)
    not (mem pprod s x) &&  (* are not kept in canonical form. *)
    not (mem app s x) &&
    not (mem arr s x) &&
    not (mem bvarith s x)
  in
  let p' = Partition.gc filter s.p in
    s.p <- p'; s



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
		   Status.Ok(equality_atom atom e s)
	     | Atom.Diseq(a, b) ->
		 let d = Fact.mk_diseq a b Fact.mk_axiom in
		   Status.Ok(diseq_atom atom d s)
	     | Atom.In(a, d) ->
		 let c = Fact.mk_cnstrnt a d Fact.mk_axiom in
		   Status.Ok(add_atom atom c s)
	   with 
	       Exc.Inconsistent -> 
		 Status.Inconsistent)
      
  and equality_atom a e =  
    protect
      (fun s  ->
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', e') = Abstract.equal (s, e) in
	 let s'' = equality e' s' in
	   normalize s'')
      
  and add_atom a c = 
    protect
      (fun s  -> 
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', c') = Abstract.cnstrnt (s, c) in
	 let s'' = add c' s' in
	   normalize s'')

  and diseq_atom a d =
    protect
      (fun s  ->   
	 s.ctxt <- Atom.Set.add a s.ctxt;
	 let (s', d') = Abstract.diseq (s, d) in
	 let s'' = diseq d' s' in
	   normalize s'')


  and protect f s =
   let k' = !Var.k in
     try
       Var.k := s.upper;
       let s' = f (copy s) in
	 s'.upper <- !Var.k;
	 Var.k := k';
	 s'
     with
       | exc ->
	   Var.k := k';
	   raise exc
end
 
let add = Process.atom


(** List all constraints with finite extension. *)

let rec split s =
  Atom.Set.union 
    (split_cnstrnt s) 
    (split_arrays s)

and split_cnstrnt s = 
  (* C.split (c_of s) *)
  failwith "to do"

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
