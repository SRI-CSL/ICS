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
  mutable upper : int;            (* Upper bound on fresh variable index. *)
  mutable diseqs : (int * Term.Set.t) Term.Map.t  (* Index for bitvector diseqs. *)
}


let empty = {
  ctxt = Atom.Set.empty;
  p = Partition.empty;
  eqs = Array.create Solution.empty;
  upper = 0;
  diseqs = Term.Map.empty
} 


(** {6 Accessors} *)

let ctxt_of s = s.ctxt
let p_of s = s.p
let v_of s = Partition.v_of s.p
let d_of s = Partition.d_of s.p
let c_of s = Partition.c_of s.p
let eqs_of s = Array.get s.eqs
let upper_of s = s.upper


(** Equality test. Do not take upper bounds into account. *)
let eq s t =              
  Partition.eq s.p t.p &&
  (Array.for_all2 
    (fun eqs1 eqs2 -> 
       Solution.eq eqs1 eqs2) 
    s.eqs t.eqs)


(** Destructive updates. *)
let update s i eqs =
  (Array.set s.eqs i eqs; s)


(** Shallow copying. *)
let copy s = {
  ctxt = s.ctxt;
  p = Partition.copy s.p;
  eqs = Array.copy s.eqs;
  upper = s.upper;
  diseqs = s.diseqs
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

let fold s f x = 
  let y = v s x in
    V.fold (v_of s) f y


(** Parameterized operations on solution sets. *)

let mem i s = Solution.mem (eqs_of s i)
	
let use i s = Solution.use (eqs_of s i)

let apply i s = Solution.apply (eqs_of s i)

let find i s = Solution.find (eqs_of s i)

let replace i s = Solution.replace (v s) (eqs_of s i)

let inv i s = Solution.inv (eqs_of s i)

let is_empty i s = Solution.is_empty (eqs_of s i)


(** Array selection *)

let d_select s (p1, p2) v = 
  match apply Th.arr s v with
    | App(Arrays(Select), [a; i]) when p1 a && p2 i -> (a, i)
    | _ -> raise Not_found

let d_update s (p1, p2, p3) v =
  match apply Th.arr s v with
    | App(Arrays(Update), [a; i; x]) when p1 a && p2 i && p3 x -> (a, i, x)
    | _ -> raise Not_found

let tt _ = true

(** Constraint of [a] in [s]. *)

let dom s a =
  let rec of_term = function
    | App(Arith(op), xl) -> of_arith op xl
    | App(Pp(op), xl) -> of_pprod op xl
    | App(Bvarith(op), xl) -> of_bvarith op xl
    | a -> 
	if is_intvar a then Dom.Int 
	else if is_realvar a then Dom.Real 
	else of_term (apply Th.la s a)
  and of_arith op xl =
    try
      match op, xl with
	| Num(q), [] ->
	    if Q.is_integer q then Dom.Int else Dom.Real
	| Multq(q), [x] -> 
	    if Q.is_integer q && of_term x = Dom.Int 
	    then Dom.Int 
	    else Dom.Real
	| Add, xl -> 
	    if List.for_all (fun x -> Dom.eq (of_term x) Dom.Int) xl 
	    then Dom.Int 
	    else Dom.Real
	| _ -> 
	    Dom.Real
      with
	  Not_found -> Dom.Real
  and of_pprod op xl = 
    try
      match op, xl with
	| Mult, _ -> 
	    if List.for_all (fun x -> Dom.eq (of_term x) Dom.Int) xl 
	    then Dom.Int 
	    else Dom.Real
	| Expt(n), [x] ->
	    if n >= 0 && of_term x = Dom.Int 
	    then Dom.Int 
	    else Dom.Real
	| _ ->
	    Dom.Real
      with
	  Not_found -> Dom.Real
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

let extend i e s =
  update s i (Solution.extend i e (eqs_of s i))


(** Pretty-printing. *)
 
let pp fmt s =
  let pps i sl =   
    if not(Solution.is_empty sl) then
      Solution.pp i fmt sl
  in
    Partition.pp fmt s.p;
    Array.iter (fun i eqs -> pps i eqs) s.eqs

let equation i s = Solution.equality (eqs_of s i)


(** Variable partitioning. *)

let is_equal s x y =
  match Term.is_equal x y with
    | Three.X -> Partition.is_equal s.p x y
    | res -> res

let is_eq s x y = (is_equal s x y = Three.Yes)
let is_diseq s x y = (is_equal s x y = Three.No)
 

(** Constraint of a term. *)

let rec cnstrnt s a =
  try
    c s a 
  with
      Not_found ->
	c s (apply Th.la s a)

(** Sigma normal forms. *)

let sigma s op =
  match op with
    | Arrays(op) ->
	Arr.sigma (is_equal s) op
    | _ ->
	Th.sigma op


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

  let rec atom s a =
    Trace.call "rule" "Can" a Atom.pp;
    let b = match a with
      | Atom.True -> Atom.True
      | Atom.Equal(a, b) -> equal s (a, b)
      | Atom.Diseq(a, b) -> diseq s (a, b)
      | Atom.In(a, d) -> sign s (a, d)
      | Atom.False -> Atom.False
    in
      Trace.exit "rule" "Can" b Atom.pp;
      b
      
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

  let rec atom (s, a) =
    Trace.call "rule" "Abst" a Atom.pp;
    let (s', a') = match a with
      | Atom.True -> (s, Atom.True)  
      | Atom.False -> (s, Atom.False)
      | Atom.Equal(x, y) -> equal s (x, y)
      | Atom.Diseq(x, y) -> diseq s (x, y)
      | Atom.In(x, d) -> cnstrnt s (x, d)
    in
      Trace.exit "rule" "Abst" a' Atom.pp;
      (s', a')

  and equal s (a, b) =
    match a, b with
      | Var _, Var _ -> 
	  (s, Atom.mk_equal (a, b))
      | Var _, App(f, _) ->
	  let i = Th.of_sym f in
	  let (s', y') = term i (s, b) in
	    let e' = Atom.mk_equal (a, y') in
	      (s', e')
      | App(f, _), Var _ ->
	  let i = Th.of_sym f in
	  let (s', x') = term i (s, a) in
	  let e' = Atom.mk_equal (b, x') in
	    (s', e')
      | App(f, _), App(g, _) -> 
	  let i = Th.of_sym f and j = Th.of_sym g in
	  let (i', j') = 
	    if Th.eq i j then (i, i) 
	    else if Th.is_fully_interp i && not(Th.is_fully_interp j) then (i, u)
	    else if not(Th.is_fully_interp i) && Th.is_fully_interp j then (u, j)
	    else (u, u)
	  in
	  let (s', x') = term i' (s, a) in
	  let (s'', y') = term j' (s', b) in
	  let e' = Atom.mk_equal (x', y') in
	    (s'', e')

  and diseq s (a, b) =
    let (s', x') = term Th.u (s, a) in
    let (s'', y') = term Th.u (s', b) in
    let d' = Atom.mk_diseq (x', y') in
      (s'', d')

  and cnstrnt s (a, i) =
    let (s', x') = term Th.la (s, a) in
    let l' = Atom.mk_in  (x', i) in
      (s', l')

  and term i (s, a) =
    match a with
      | Var _ -> 
	  (s, a)
      | App(f, al) ->
	  let j = Th.of_sym f in
	  let (s', al') = args j (s, al) in
	  let a' = if Term.eql al al' then a else sigma s f al' in
	    if not(Th.is_fully_interp i) || i <> j then
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


(** Processing an equality over pure terms. *)
let rec equality e s =
  Trace.msg "rule" "Assert" e Fact.pp_equal;
  let (a, b, _) = Fact.d_equal e in
    if Term.eq a b then s else 
      match a, b with
	| Var _, Var _ -> 
	    merge_v e s
	| App(f, _), Var _ -> 
	    let i = Th.of_sym f in
	      merge_i i e s
	| Var _, App(f, _) -> 
	    let i = Th.of_sym f in
	      merge_i i e s
	| App(f, _), App(g, _) ->
	    let i = Th.of_sym f and j = Th.of_sym g in
	      if Th.eq i j && Th.is_fully_interp i then
		merge_i i e s
	      else 
		let (s', x') = name i (s, a) in
		let (s'', y') = name j (s', b) in
		  merge_v (Fact.mk_equal x' y' None) s''
	
(** Processing of a variable equality. *)  
and merge_v e s =
  let fuse_star e = 
    List.fold_right 
      (fun i s -> if is_empty i s then s else fuse i e s)
  in
  let (x, y, prf) = Fact.d_equal e in
    match is_equal s x y with
      | Three.Yes -> 
	  s
      | Three.No -> 
	  raise Exc.Inconsistent
      | Three.X ->
	  Trace.msg "rule" "Merge(v)" e Fact.pp_equal;
	  let (ch', p') = Partition.merge e s.p in
	    s.p <- p';
	    let s' = fuse_star e Th.all s in  (* propagate on rhs. *)  
	      close_p ch' s'


(** Processing of an interpreted equality. *)
and merge_i i e s =
  let (x, y, rho) = Fact.d_equal e in
  let a = find i s x
  and b = find i s y in
    if Term.eq a b then s else
      let e' = Fact.mk_equal a b None in
	Trace.msg "rule" "Merge(i)" e' Fact.pp_equal;
	try
	  compose i s (Th.solve i e')
	with 
	    Exc.Incomplete ->
	      let (a, b, _) = Fact.d_equal e in
	      let (s, x) = name i (s, a) in
	      let (s, y) = name i (s, b) in
	      let e' = Fact.mk_equal x y None in
		merge_v e' s

and nonlin_equal e s =
  let rec linearize occs s =
    Set.fold
      (fun x s ->
	 try 
	   let a = apply Th.pprod s x in
	   let b = Sig.map (find Th.la s) a in
	     if Term.eq a b then s else 
	       let (s', b') = Abstract.term Th.u (s, b) in
	       let e' = Fact.mk_equal (v s' x) b' None in
		 merge_v e' s'
	 with
	     Not_found -> s)
      occs s
  in
  let (x, _, _) = Fact.d_equal e in
    linearize (use Th.pprod s x) s


and fuse i e s =   
  let (ch', es', eqs') = Solution.fuse i (eqs_of s i) [e] in
    Array.set s.eqs i eqs';
    let s' = Fact.Equalset.fold merge_v es' s in
      close_i i ch' s'

and compose i s r =
  let (ch', es', eqs') = Solution.compose i (eqs_of s i) r in
    Array.set s.eqs i eqs';
    let s' =  Fact.Equalset.fold merge_v es' s in
      close_i i ch' s'

and refine c s = 
  Trace.msg "rule" "Refine" c Fact.pp_cnstrnt;
  let (k, i, _) = Fact.d_cnstrnt c in
  let (ch', p') = Partition.add c s.p in
    s.p <- p';
    close_p ch' s

(** Infer new disequalities from equalities. *)
and infer i e s =
  let (a, b, _) = Fact.d_equal e in
    if Term.eq a b then 
      s
    else if Th.eq i Th.la then
      infer_la e s
    else if Th.eq i Th.bv then
      infer_bv e s
    else if Th.eq i Th.cop then
      infer_cop e s
    else 
      s

(** If [x = q] and [y = p] with [q], [p] numerical constraints,
  then deduce [x <> y]. *)
and infer_la e s =
  let (x, a, _) = Fact.d_equal e in
    match Arith.d_num a with  
      | Some(q) ->
          Solution.fold
            (fun y (b, _) s ->
               match Arith.d_num b with
                 | Some(p) when not(Q.equal q p) ->
	             let d = Fact.mk_diseq (v s x) (v s y) None in
                        diseq d s
	         | _ -> s)               
            (eqs_of s Th.la) s
      | None ->
	 s 

and infer_bv e s =
  let (x, a, _) = Fact.d_equal e in
    match Bitvector.d_const a with  
      | Some(c) ->
          Solution.fold
            (fun y (b, _) s ->
               match Bitvector.d_const b with
                 | Some(d) when not(Bitv.equal c d) ->
	             let d = Fact.mk_diseq (v s x) (v s y) None in
                        diseq d s
	         | _ -> s)               
            (eqs_of s Th.bv) s
      | None ->
	 s 

(** If [x = a], [y = b] are in the coproduct solution set, 
  and if [a] and [b] are diseq in this theory, then deduce [x <> y]. *)
and infer_cop e s =
  let (x, a, _) = Fact.d_equal e in
    match a with
     | App(Coproduct(InL | InR), [_]) ->
         Solution.fold
           (fun y (b, _) s ->
              if Coproduct.is_diseq a b then
                let d = Fact.mk_diseq (v s x) (v s y) None in 
                  diseq d s              
              else
                 s)
           (eqs_of s Th.cop) s
     | _ -> s

(** Deduce new constraints from an equality *)
and deduce i e s =
  let (a, b, _) = Fact.d_equal e in
    if Term.eq a b then 
      s
    else if Th.eq i Th.la then
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
  Trace.msg "rule" "Deduce(nl)" e Fact.pp_equal;
  let (x, a, _) = Fact.d_equal e in
  let x' = v s x in
  try
    let j = c s a in
      (try
	 let i = c s x' in
	   if Sign.sub j i then s else 
	     add (Fact.mk_cnstrnt x' j None) s
       with
	   Not_found -> 
	     add (Fact.mk_cnstrnt x' j None) s)
  with
      Not_found -> s


(** If [k = R[k''], [k' = R'[k'']] in [A], then
    isolate [k''] in [k' = R'[k'']] to obtain [k'' = R'']
    and plug this solution into [R] to obtain [R'''] as [sigma(R[k'':= R''])].
    deduce new constraints from [k = R'''].  Notice that equality [k' = R'] has
    only to be considered once. *)

and deduce_la e s =
  let s' = deduce_la1 e s in
  let (k, r, _) = Fact.d_equal e in
  let k = v s k in
    if not(is_slack k) then s else
      let visited = ref Term.Set.empty in
	Arith.fold
	  (fun _ k'' s ->
	     Set.fold
	       (fun k' s ->
		  if Term.Set.mem k' !visited then s else 
		    try
		      let r' = apply Th.la s k' in
		      let r'' = Arith.isolate k'' (k', r') in
		      let e' = Fact.mk_equal k (Arith.replace r k'' r'') None in
			visited := Term.Set.add k' !visited;
			deduce_la1 e' s
		    with
			Not_found -> s)
	       (use Th.la s k'') 
	       s)
	  r s'
     
    
(* If C(k) = 0, then we can assert that S(k) = 0.
   Inequality Propagation: If S(k) = R+[k'] - R-[k''],
   then if either C(k) or C[R-[k'']] goes from >= to >,
   then we can assert R+[k']>0. *) 

and deduce_la1 e s =
  let inconsistent (x, a) =
    try
      (dom s x = Dom.Int) &&
      (match Arith.d_num a with
	 | Some(q) -> not(Q.is_integer q)
	 | None -> false)
    with
	Not_found -> false
  in
  let partition s a =
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
  in
  let (x, sk, prf) = Fact.d_equal e in
  let k = v s x in
    if not(is_slack k) then 
      s 
    else if inconsistent (k, sk) then
      raise Exc.Inconsistent
    else  
      begin
	Trace.msg "rule" "Deduce" e Fact.pp_equal;
	try  
	  let c_sk = c s sk in  (* Keep invariant that [c s k] is stronger than [c s sk]. *)
	  let s = add (Fact.mk_cnstrnt k c_sk None) s in 
	  let c_k = c s k in 
	    if c_k = Sign.Zero then
	      equality (Fact.mk_equal k Arith.mk_zero None) s
	    else 
	      let (r_plus, r_minus) = partition s (find Th.la s k) in
		if c_k = Sign.Pos || c s r_minus = Sign.Pos then
		  if c s r_plus = Sign.Pos then s else 
		    add (Fact.mk_cnstrnt r_plus Sign.Pos None) s 
		else 
		  s
	with
	    Not_found -> s (* should not happen *)
      end 
	    

(** Merging variable equality/disequalities/constraints *)
and diseq d s =
  Trace.msg "rule" "Diseq" d Fact.pp_diseq;
  let (x, y, _) = Fact.d_diseq d in
  let (ch', p') = Partition.diseq d s.p in
    s.p <- p';
    close_p ch' s

and add c s =
  Trace.msg "rule" "Add" c Fact.pp_cnstrnt;
  let normalize (a, i) =
    let (a', i') = match i with
      | Sign.Neg -> (Arith.mk_neg a, Sign.Pos)
      | Sign.Nonpos -> (Arith.mk_neg a, Sign.Nonneg)
      | _ -> (a, i)
    in
      (lookup s a', i')   
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
	  let (b, i) = normalize (a, i) in
	    match b with
	      | Var _ when is_slack b ->
		  refine (Fact.mk_cnstrnt b i prf) s
	      | App(Arith(Multq(q)), [x]) when is_slack x ->
		  refine (Fact.mk_cnstrnt x (Sign.multq (Q.inv q) i) None) s
	      | _ ->
		  let d = if is_int s a then Some(Dom.Int) else None in
		  let alpha = if i = Sign.Pos then false else true in
		  let k = Term.mk_slack None alpha d in
		    equality (Fact.mk_equal k b None)
		      (refine (Fact.mk_cnstrnt k i None) s)


(** Propagate changes in the variable partitioning. *)    
and close_i i =
  Set.fold
    (fun x s ->
       try
	 let e' = equation i s x in
	 let s' =  if Th.eq i Th.la then nonlin_equal e' s else s in
	 let s'' =  deduce i e' s' in
         let s''' = infer i e' s'' in
           s'''
       with
	   Not_found -> s)

(** Propagate changes in the variable partitioning. *)    
and close_p ch s =
  close_v ch.Partition.chv
    (close_c ch.Partition.chc 
       (close_d ch.Partition.chd s))

and close_v chv = 
  Set.fold
    (fun x s ->
       let y = v s x in
	 if Term.eq x y then s else 
	   let e = Fact.mk_equal x y None in
	     Trace.msg "rule" "Close(v)" e Fact.pp_equal;
	     let s' =  List.fold_right
			 (fun i s ->
			    let a = find i s x 
			    and b = find i s y in
			      if Term.eq a b || (is_var a && is_var b) then 
				s 
			      else
				merge_i i e s)
			 Th.interp s
	     in
	     let s'' = arrays_equal e s' in
	     let s''' = bvarith_equal e s'' in
	       s''')
    chv 

and close_c chc = 
  Set.fold
    (fun x s ->
       try
	 let i = c s x in
	   match i with
	     | Sign.F -> 
		 raise Exc.Inconsistent
	     | Sign.Zero ->
		 equality (Fact.mk_equal x Arith.mk_zero None) s
	     | _ ->
		 s
	   with
	       Not_found -> s)
    chc

and close_d chd =
  Set.fold
    (fun x s ->
       let yl = d s x in
	 List.fold_right
	   (fun y s ->
	      let d = Fact.mk_diseq x y None in
		arrays_diseq d 
                   (bv_diseq d s))
	   yl s)
    chd 

(** {6 Bitvector propagation} *)

and bv_diseq d' s =
  let add x c =
    try
      let (k, cs) = Term.Map.find x s.diseqs in
	failwith "to do"
    with
	Not_found -> 
	  Term.Map.add x (1, Term.Set.singleton c)
  in
  let (x, _, _) = Fact.d_diseq d' in
    try
      let a = apply Th.bv s x in
	if not(Bitvector.is_const a) then s else
	  s
    with
	Not_found -> s
  
(** {6 Array propagation} *)

(** Forward chaining on the array properties
  - [a[i:=x][i] = x]
  - [i <> j] implies [a[i:=x][j] = a[x]]
  - [i <> j] and [i <> k] implies [a[j:=x][i] = a[k:=y][i]]
  - [a[j:=x] = b[k:=y]], [i <> j], [i <> k] implies [a[i] = b[i]].
  - [a[i:=x] = b[i := y]] implies [x = y]. *)

and arrays_diseq d s =
  if is_empty Th.arr s then s else 
    arrays_diseq1 d
      (arrays_diseq2 d
	 (arrays_diseq3 d s))

(** [i <> j] implies [a[i:=x][j] = a[j]].
  Thus, look for [v = u[j]] and [u' = a[i := x]] with [u = u'] in [s]
  using the use lists. Now, when [w = a[j]], then infer [v = w]. *)
and arrays_diseq1 d s =
  let (i, j, prf) = Fact.d_diseq d in
  let diseq  (i, j) s =
    Set.fold
      (fun v s -> 
	 try
	   let (u, j) = d_select s (tt, tt) v in     (* [v = u[j]] *)
	     fold s
	       (fun u' s ->                         (* [u = u'] *)
		  try                               (* [u' = a[i:=x]] *)
		    let (a, i, x) = d_update s (tt, tt, tt) u' in
		    let (s, w) = name Th.arr (s, Arr.mk_select Term.is_equal a j) in
		    let e' = Fact.mk_equal v w None in
		      merge_v e' s
		  with
		      Not_found -> s)
	       u s
	 with
	     Not_found -> s)
      (use Th.arr s j) s
  in
    diseq (i, j)
      (diseq (j, i) s)

(** [i <> j] and [i <> k] implies [a[j:=x][i] = a[k:=y][i]].
  Thus, for [i <> j], select [v = u[i]] and [u' = a[j:=x]] with
  [u = u'] in [s]. Now, for all [u'' = a[k:=y]] with [k <> i],
  add [a[j:=x][i] = a[k:=y][i]]. *)
and arrays_diseq2 d s =
  let (i, j, _) = Fact.d_diseq d in
  let diseq (i, j) s =
    Set.fold 
      (fun v s ->
	 (try
	    let (u, _) = d_select s (tt, is_eq s i) v in
	      fold s
		(fun u' s ->
		   if not(is_eq s u u') then s else
		     try
		       let (a, _, x) = d_update s (tt, is_eq s j, tt) u' in
			 Set.fold
			   (fun u'' s ->
			      try
				let (_, k, y) = d_update s (is_eq s a, is_diseq s i, tt) u'' in
				let (s, u1) = name Th.arr (s, Arr.mk_update Term.is_equal a j x) in
				let (s, v1) = name Th.arr (s, Arr.mk_select Term.is_equal u1 i) in
				let (s, u2) = name Th.arr (s, Arr.mk_update Term.is_equal a k y) in
				let (s, v2) = name Th.arr (s, Arr.mk_select Term.is_equal u2 i) in
				let e' = Fact.mk_equal v1 v2 None in
				  merge_v e' s
			      with
				  Not_found -> s)
			   (use Th.arr s a) s
		     with
			 Not_found -> s)
		u s
	  with 
	      Not_found -> s))
      (use Th.arr s i) s
  in
    diseq (i, j)
      (diseq (i, j) s)

(** [a[j:=x] = b[k:=y]], [i <> j], [i <> k] implies [a[i] = b[i]].
  We are propagating disequalities [i <> j]. 
  If [u = a[j := x]], then look for all [k] disequal from [i] for [v = b[k:=y]].
  Now, assert [a[i] = b[i]]. *)
and arrays_diseq3 d' s = 
  let (i, j, _) = Fact.d_diseq d' in
  let diseq (i, j) s =
    Set.fold
      (fun u s ->
	 try
	   let (a, j, _) = d_update s (tt, tt, tt) u in
	     List.fold_right
	       (fun k s ->
		  Set.fold
		    (fun v s ->
		       try
			 let (b, _, _) = d_update s (tt, is_eq s k, tt) v in
			 let (s, w1) = name Th.arr (s, Arr.mk_select Term.is_equal a i) in
			 let (s, w2) = name Th.arr (s, Arr.mk_select Term.is_equal b i) in
			 let e' = Fact.mk_equal w1 w2 None in
			   merge_v e' s
		       with
			   Not_found -> s)
		    (use Th.arr s k) s)
	       (d s i) s
	 with
	     Not_found -> s)
      (use Th.arr s j) s
  in
    diseq (i, j)
      (diseq (j, i) s)


and arrays_equal e s =
  if is_empty Th.arr s then s else 
    arrays_equal1 e
      (arrays_equal2 e
	 (arrays_equal3 e s))
     
(** [i = j] implies [a[i:= x][j] = x].
  Since [i] and [j] are already merged on rhs, it suffices
  to look for [v1 = v2'[i]] and [v2 = a[i := x]] with [v2] equal [v2'] in [s].
  Now, [v1 = x].  *)
and arrays_equal1 e s =
  let (i, j, _) = Fact.d_equal e in
    Set.fold
      (fun v1 s ->
	 try
	   let (v2', _) = d_select s (tt, is_eq s i) v1 in
	     Set.fold
	       (fun v2 s -> 
		  if not(is_eq s v2 v2') then s else 
		    try
		      let (a, _, x) = d_update s (tt, is_eq s i, tt) v2 in
		      let e' = Fact.mk_equal (v s v1) x None in
			merge_v e' s
		    with
			Not_found -> s)
	       (use Th.arr s (v s j))
	       s
	  with
	      Not_found ->  s)
      (use Th.arr s (v s j))
      s

(** [a[i:=x] = b[i := y]] implies [x = y].
  Thus, if [v = u] has been merged, then look for
  [v = a[i:=x]] and [v' = b[i := y]] with [v = v'] in [s], now merge [x = y]. *)
and arrays_equal2 e s = 
  let (v, _, _) = Fact.d_equal e in   (* [find] of [v] has changed. *)
    try
      let (_, i, x) = d_update s (tt, tt, tt) v in
	fold s
	  (fun v' s ->
	     if Term.eq v v' then s else
	       try
		 let (_, _, y) = d_update s (tt, is_eq s i, tt) v' in
		 let e' = Fact.mk_equal x y None in
		   merge_v e' s
	       with
		   Not_found -> s)
	  v s
    with
	Not_found -> s

(** [a[j:=x] = b[k:=y]], [i <> j], [i <> k] implies [a[i] = b[i]].
  Thus, for a merged [v = u], look for [v = a[j:= x]] and [v' = b[k := y]] 
  with [v = v'] in [s]. For all [i] such that [i <> j] and [i <> k] add
  [w1 = w2] for [w1 = a[i]] and [w2 = b[i]], possibly extending the solution set. *)
and arrays_equal3 e s = 
  let (v, _, _) = Fact.d_equal e in   (* [find] of [v] has changed. *)
    try
      let (a, j, _) = d_update s (tt, tt, tt) v in
	fold s
	  (fun v' s ->
	     if Term.eq v v' then s else
	       try
		 let (b, k, _) = d_update s (tt, tt, tt) v' in
		   List.fold_right
		     (fun i s ->
			if not(is_diseq s i k) then s else
			  let (s, w1) = name Th.arr (s, Arr.mk_select Term.is_equal a i) in
			  let (s, w2) = name Th.arr (s, Arr.mk_select Term.is_equal b i) in
			  let e' = Fact.mk_equal w1 w2 None in
			    merge_v e' s)
		     (d s j) s
	       with
		   Not_found -> s)
	  v s
    with
	Not_found -> s
   
	  
and bvarith_equal e s =
  if is_empty Th.bvarith s then s else 
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
		      equality e' s'
		| _ ->
		    s )
	   with
	       Not_found -> s)
	(use bvarith s x)
	s


    
(** Garbage collection. Remove all variables [x] which are are scheduled
 for removal in the partitioning. Check also that this variable [x] does
 not occur in any of the solution sets. Since [x] is noncanonical, this
 check only needs to be done for the [u] part, since all other solution
 sets are kept in canonical form. *)


let compactify = ref true

let rec normalize s =
  if !compactify then gc s else s

and gc s =
  let filter x =  
    not (List.exists (fun i -> mem i s x) Th.all)
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
    Trace.func "process" "Process" Atom.pp (Status.pp pp)
      (fun a ->
	 try
	   (match Can.atom s a with
	      | Atom.True -> 
		  Status.Valid
	      | Atom.False ->
		  Status.Inconsistent
	      | a ->
		  Status.Ok(protect
			      (fun s ->
				 s.ctxt <- Atom.Set.add a s.ctxt;
				 let (s, a) = Abstract.atom (s, a) in
				   process a s)
			      s))
	 with 
	     Exc.Inconsistent -> 
	       Status.Inconsistent)

  and process a s = 
    match a with
      | Atom.Equal(x, y) ->
	  let e = Fact.mk_equal x y Fact.mk_axiom in
	    normalize (equality e s)
      | Atom.Diseq(x, y) ->
	  let d = Fact.mk_diseq x y Fact.mk_axiom in
	    normalize (diseq d s)
      | Atom.In(x, d) ->
	  let c = Fact.mk_cnstrnt x d Fact.mk_axiom in
	    normalize (add c s)
      | Atom.True -> 
	  s
      | Atom.False ->
	  raise Exc.Inconsistent
	     
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
  Atom.Set.empty

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
