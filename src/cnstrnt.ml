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

open Mpa  
open Sym

(** {6 Term comparisons} *)

module Pred = struct

  let le = Arith.le
  let lt = Arith.lt

  let ge a b = le b a
  let gt a b = lt b a

end


(** {6 Bounds} *)

type bound =
  | Posinf 
  | Neginf
  | Bound of bool * Term.t


module Bound = struct

  let eq l m =
    match l, m with
      | Neginf, Neginf -> true
      | Posinf, Posinf -> true
      | Bound(alpha, a), Bound(beta, b) -> alpha = beta && Term.eq a b
      | _ -> false

  let le l m =
    match l, m with
      | Neginf, _ -> true
      | _, Posinf -> true
      | Posinf, _ -> false
      | Bound(true, a), Bound(true, b) -> Pred.le a b
      | Bound(_, a), Bound(_, b) -> Pred.lt a b
      | Bound _, Neginf -> false

  let lt l m = 
    match l, m with
      | Neginf, Neginf -> false
      | Neginf, _ -> true
      | Posinf, Posinf -> false
      | _, Posinf -> true
      | Posinf, _ -> false
      | Bound(alpha, a), Bound(beta, b) ->
	  if alpha = beta then Pred.lt a b else Pred.le a b
      | Bound _, Neginf -> false

  let occurs x = function
    | Neginf -> false
    | Posinf -> false
    | Bound(_, a) -> Term.occurs x a

end


(** {6 Intervals} *)

module Interval = struct

  type t = bound * bound

  let mk_empty = (Posinf, Neginf)

  let is_empty = function
    | (Posinf, Neginf) -> true
    | _ -> false

  let mk_full = (Neginf, Posinf)

  let mk_equal a =
    let b = Bound(true, a) in
      (b, b)
      
  let mk_singleton q = mk_equal (Arith.mk_num q)
 
  let mk_zero = mk_singleton Q.zero

  let mk_one = mk_singleton Q.one

  let mk_nonneg = (Bound(true, Arith.mk_zero), Posinf)

  let pp fmt (l, u) =
    (match l with
       | Neginf -> 
	   Pretty.string fmt "(-inf"
       | Posinf -> 
	   Pretty.string fmt "(inf"
       | Bound(alpha, a) -> 
	   Pretty.string fmt (if alpha then "[" else "(");
	   Term.pp fmt a);
    Pretty.string fmt ", ";
    (match u with
       | Neginf -> 
	   Pretty.string fmt "-inf)"
       | Posinf -> 
	   Pretty.string fmt "inf)"
       | Bound(alpha, a) -> 
	   Pretty.string fmt (if alpha then "]" else ")");
	   Term.pp fmt a)
   
  let occurs x (l, u) =
    Bound.occurs x l || Bound.occurs x u
  
  let eq (l1, u1) (l2, u2) =
    Bound.eq l1 l2 && Bound.eq u1 u2

  let sub (l1, u1) (l2, u2) = 
    Bound.le l2 l1 && Bound.le u1 u2

  let disjoint (l1, u2) (l2, u2) =
    Bound.lt u2 l2 || Bound.lt u2 l1
 
  let rec make d (l, u) =
    let mk_lower ((alpha, a) as bnd) =    (* normalized lower bound *)
      if d = Dom.Real then bnd else 
	match a with
	  | Term.App(Arith(Num(q)), []) -> 
	      if d = Dom.Int && not alpha then
		(true, Arith.mk_num(Q.of_z(Q.floor(Q.add q Q.one))))
	      else if d = Dom.Nonint && alpha && Q.is_integer q then
		(false, Arith.mk_num q)
	      else 
		bnd
	  | _ -> 
	      bnd
    and mk_upper ((beta, b) as bnd) =     (* normalized upper bound *)
      if d = Dom.Real then bnd else 
	match b with
	  | Term.App(Arith(Num(p)), []) -> 
	      if d = Dom.Int && not beta then
		let p' = Q.of_z(Q.ceil(Q.sub p Q.one)) in
		  (true, Arith.mk_num(Q.of_z(Q.ceil(Q.sub p Q.one))))
	      else if d = Dom.Nonint && beta && Q.is_integer p then
		(false, Arith.mk_num p)
	      else 
		bnd
	  | _ ->
	      bnd
    in
      match l, u with
	| Neginf, Posinf ->
	    mk_full
	| Posinf, _ -> 
	    mk_empty
	| _, Neginf ->
	    mk_empty
	| Neginf, Bound(beta, b) ->
	    let (beta', b') = mk_upper (beta, b) in
	    (Neginf, Bound(beta', b'))
	| Bound(alpha, a), Posinf ->
	    let (alpha', a') = mk_lower (alpha, a) in
	    (Bound(alpha', a'), Posinf)
	| Bound(alpha, a), Bound(beta, b) ->
	    let (alpha', a') = mk_lower (alpha, a)
	    and (beta', b') = mk_upper (beta, b) in
	      if alpha && beta && Pred.gt a b then
		mk_empty
	      else if Pred.ge a b then
		mk_empty
	      else 
		(Bound(alpha', a'), Bound(beta', b'))

  let const q = mk_singleton q
	
  let multq d q ((l, u) as i) =
    if Q.is_zero q then mk_zero
    else if Q.is_one q then i
    else if Q.is_pos q then
      let multq = function
	| Neginf -> Neginf
	| Posinf -> Posinf
	| Bound(alpha, a) -> Bound(alpha, Arith.mk_multq q a)
      in
	make d (multq l, multq u)
    else 
      let multq = function
	| Neginf -> Posinf
	| Posinf -> Neginf
	| Bound(alpha, a) -> Bound(alpha, Arith.mk_multq q a)
      in
	make d (multq u, multq l)

  exception Empty
	
  let add d (l1, u1) (l2, u2) =
    try
      let l = match l1, l2 with
	| _, Posinf -> raise Empty
	| Posinf, _ -> raise Empty
	| Neginf, _ -> Neginf
	| _, Neginf -> Neginf
	| Bound(alpha1, a1), Bound(alpha2, a2) -> 
	    Bound(alpha1 && alpha2, Arith.mk_add a1 a2)
      and u = match u1, u2 with
	| _, Neginf -> raise Empty
	| Neginf, _ -> raise Empty
	| Posinf, _ -> Posinf
	| _, Posinf -> Posinf
	| Bound(alpha1, a1), Bound(alpha2, a2) ->
	    Bound(alpha1 && alpha2, Arith.mk_add a1 a2)
      in
	make d (l, u)
    with
	Empty -> mk_empty
	  

  let replace d x y ((l, u) as i) =
    match l, u with
      | _ , Neginf -> mk_empty
      | Posinf, _ -> mk_empty
      | Neginf, Posinf -> i
      | Neginf, Bound(beta, b) ->
	  let b' = Arith.replace x y b in
	    if b == b' then i else make d (Neginf, (Bound(beta, b')))
      | Bound(alpha, a), Posinf ->
	  let a' = Arith.replace x y a in
	    if a == a' then i else 
	      make d (Bound(alpha,a'), Posinf)
      | Bound(alpha, a), Bound(beta, b) ->
	  let a' = Arith.replace x y a 
	  and b' = Arith.replace x y b in
	    if a' == a && b' == b then i else
	      make d (Bound(alpha, a'), Bound(beta, b'))
		
end 


(** {6 Intersection of intervals} *)

module Intervals = struct

  module Set = Set.Make(
    struct
      type t = Interval.t
      let compare i j = 
	if Interval.eq i j then 0 else Pervasives.compare i j
    end)

  type t = Set.t

  let inject = Set.singleton

  let mk_empty = inject(Interval.mk_empty)

  let is_empty is = (is == mk_empty)

  let mk_full = inject(Interval.mk_full)

  let mk_singleton q = inject(Interval.mk_singleton q)

  let mk_zero = mk_singleton Q.zero
  let mk_one = mk_singleton Q.one

  let to_list = Set.elements

  let eq = Set.equal

  let subsumed i = Set.exists (Interval.sub i)

  let sub is js =
    Set.for_all (fun i -> subsumed i js) is

  exception Empty

  let inter1 d ((l1, u1) as i) js =
    if Interval.is_empty i then 
      mk_empty
    else if subsumed i js then
      js 
    else 
       let js = ref js in
       let normalized = ref false in
       let replace j k = 
	 if Interval.is_empty k then
	   raise Empty
	 else 
	   begin 
	     js := Set.add k (Set.remove j !js);
	     normalized := true
	   end 
       in
	 try
	   Set.iter
	     (fun ((l2, u2) as j) ->                     (* [[l1, u1] inter [l2, u2]] reduces to *)
		if Bound.le l1 l2 && Bound.le u1 u2 then (* [[l2, u1]] if [l1 <= l2] and [u1 <= u2]. *)
		  replace j (Interval.make d (l2, u1))
		else if Bound.le l2 l1 && Bound.le u2 u1 then
		  replace j (Interval.make d (l1, u2)))  
	     !js;
	   if !normalized then
	     !js
	   else 
	     Set.add i !js
	 with
	     Empty -> mk_empty


  let inter d = Set.fold (inter1 d) 

  let pp fmt is =
    Pretty.list Interval.pp fmt (Set.elements is)

  let fold = Set.fold

  let map d f is = 
    Set.fold 
      (fun i acc -> 
	 let j = f i in
	   if Interval.eq i j then 
	     acc
	   else
	     inter1 d j (Set.remove i acc))
      is is

  let multq d q = map d (Interval.multq d q)

  let add d is js =
    Set.fold 
      (fun i -> 
	 Set.fold 
	   (fun j -> 
	      inter1 d (Interval.add d i j)) js) 
      is mk_full

  let mult d is js = mk_full

  let disjoint is js =
    Set.exists (fun i -> Set.exists (Interval.disjoint i) js) is
   
  let replace d x a is =
    map d (Interval.replace d x a) is 


  let d_equalities is =
    Set.fold
      (fun ((l, h) as i) ((es, is) as acc) ->
	 match l, h with
	   | Bound(true, a), Bound(true, b) 
	       when Term.eq a b ->
	         (Term.Set.add a es, Set.remove i is)
	   | _ ->
	       acc)
      is
      (Term.Set.empty, is)

  let occurs x = Set.exists (Interval.occurs x)
	 
end 


(** {6 Constraints} *)

open Bound

type t = {
  dom : Dom.t;
  intervals : Intervals.t
}

let dom_of c = c.dom

let intervals_of c = Intervals.Set.elements c.intervals

let pp fmt c = 
  let il = intervals_of c in
    if c.dom <> Dom.Real || il = [] then
      Dom.pp fmt c.dom;
    Pretty.set Interval.pp fmt il

let mk_empty = {
  dom = Dom.Real;
  intervals = Intervals.mk_empty
}
let is_empty c = (c == mk_empty)

let mk_dom d = {dom = d; intervals = Intervals.mk_full}

let mk_real = mk_dom Dom.Real
let mk_int = mk_dom Dom.Int
let mk_nonint = mk_dom Dom.Nonint

let mk_nat = {
  dom = Dom.Int;
  intervals = Intervals.Set.singleton Interval.mk_nonneg
}

let make d is =
  {dom = d; intervals = is}

let mk_singleton q =
  let d = if Q.is_integer q then Dom.Int else Dom.Nonint in
    {dom = d; 
     intervals = Intervals.Set.singleton (Interval.mk_singleton q)}

let mk_equal a =
  match Arith.d_num a with
    | Some(q) -> 
	mk_singleton q
    | None ->
	{dom = Dom.Real;
	 intervals = Intervals.Set.singleton (Interval.mk_equal a)}
  

let mk_zero = mk_singleton Q.zero
let mk_one = mk_singleton Q.one

let mk_less dom (b, beta) =
  { dom = dom;
    intervals = Intervals.Set.singleton (Interval.make dom (Neginf, Bound(beta, b))) }

let mk_greater dom (alpha, a) =
  { dom = dom;
    intervals = Intervals.Set.singleton (Interval.make dom (Bound(alpha, a), Posinf)) }

let eq c d =
  Dom.eq c.dom d.dom &&
  Intervals.eq c.intervals d.intervals
  
let sub c d =
  Dom.sub c.dom d.dom &&
  Intervals.sub c.intervals d.intervals
 
let disjoint c d = 
  Intervals.disjoint c.intervals d.intervals
  
let inter c d = 
  try
    let dom = Dom.inter c.dom d.dom in
    let is = Intervals.inter dom c.intervals d.intervals in
      make dom is
  with
      Dom.Empty -> mk_empty

let d_equalities c =
  let (es, d) = Intervals.d_equalities c.intervals in
    (es, make c.dom d)
    

(** {6 Abstract interpretation} *)

let add c d =
  let dom = match c.dom, d.dom with
    | Dom.Real, _ -> Dom.Real
    | _, Dom.Real -> Dom.Real
    | Dom.Int, Dom.Int -> Dom.Int
    | Dom.Int, Dom.Nonint -> Dom.Nonint
    | Dom.Nonint, Dom.Nonint -> Dom.Real  (* overapproximation. *)
    | Dom.Nonint, Dom.Int -> Dom.Nonint
  in
    make dom (Intervals.add dom c.intervals d.intervals)
  

let rec addl = function
  | [] -> mk_zero
  | [c] -> c
  | [c; d] -> add c d
  | c :: cl -> add c (addl cl)

let multq q c =
  if Q.is_zero q then
    mk_zero
  else if Q.is_one q then
    c
  else 
    let dom = if Q.is_integer q && c.dom = Dom.Int then Dom.Int else Dom.Real in
      make dom (Intervals.multq dom q c.intervals)
	
let mult c d =
  let dom = match c.dom, d.dom with
    | Dom.Int, Dom.Int -> Dom.Int
    | _ -> Dom.Real
  in
    make dom (Intervals.mult dom c.intervals d.intervals)

let rec multl = function
  | [] -> mk_one
  | [c] -> c
  | c :: cl -> mult c (multl cl)

let expt n c =
  if n = 0 then
    mk_one
  else if n < 0 then
    mk_real
  else
    mk_real

let div c d = mk_real

(** {6 Miscellaneous} *)

let add_upper (beta, b) c =
  let i = (Neginf, Bound(beta, b)) in
    make c.dom (Intervals.inter1 c.dom i c.intervals)

let add_lower (alpha, a) c =
  let i = (Bound(alpha, a), Posinf) in
    make c.dom (Intervals.inter1 c.dom i c.intervals)

let add_dom dom c =
  try
    let dom = Dom.inter dom c.dom in
      make dom c.intervals
  with
      Dom.Empty -> mk_empty
	
let exists p c = Intervals.Set.exists p c.intervals
  
let exists_lower p c =
  Intervals.Set.exists
    (fun (l, _) ->
       match l with
	 | Neginf -> false
	 | Posinf -> false
	 | Bound(alpha, a) -> p (alpha, a))
    c.intervals

let exists_upper p c =
  Intervals.Set.exists
    (fun (_, u) ->
       match u with
	 | Neginf -> false
	 | Posinf -> false
	 | Bound(alpha, a) -> p (alpha, a))
    c.intervals

let fold f c = Intervals.Set.fold f c.intervals

let fold_lower f c =
  Intervals.Set.fold
    (fun (l, _) acc ->
       match l with
	 | Posinf -> acc
	 | Neginf -> acc
	 | Bound(alpha, a) -> f (alpha, a) acc)
    c.intervals

let fold_upper f c =
  Intervals.Set.fold
    (fun (_, u) acc ->
       match u with
	 | Posinf -> acc
	 | Neginf -> acc
	 | Bound(alpha, a) -> f (alpha, a) acc)
    c.intervals

let replace x a c =
  make c.dom (Intervals.replace c.dom x a c.intervals)

let occurs x c = Intervals.occurs x c.intervals

  
(** {6 Constraint of a term.} *)

let of_term f a =
  let rec term a =
    match a with
      | Term.App(Arith(op), xl) -> arith op xl
      | Term.App(Pp(op), xl) -> pprod op xl
      | Term.App(Bvarith(op), [x]) -> bvarith op x
      | Term.App(Fun(Apply(Some(d))), [_]) ->
	  mk_dom d
      | _ -> 
	  f a

  and arith op al = 
    match op, al with
      | Num(q), [] ->
	  mk_singleton q
      | Multq(q), [x] ->
	  multq q (term x)
      | Add, [x; y] ->
	  add (term x) (term y)
      | Add, xl ->
	  addl (List.map term xl)
      | _ ->
	  assert false

  and bvarith op a =
    match op with
      | Unsigned -> mk_nat

  and pprod op al =
    match op, al with
      | Expt(n), [x] -> 
	  expt n (term x)
      | Mult, [] -> 
	  mk_one
      | Mult, [x] ->
	  term x
      | Mult, [x; y] ->
	  mult (term x) (term y)
      | Mult, xl -> 
	  multl (List.map term xl)
      | _ ->
	  assert false

  in 
    term a

let rec of_addl f = function
  | [] -> mk_zero
  | [x] -> of_term f x
  | [x; y] -> add (of_term f x) (of_term f y)
  | x :: xl -> add (of_term f x) (of_addl f xl)




(** Integer test. *)
let rec is_int f a = 
  let is_int_var x = 
    try 
      let c = f x in
	Dom.eq c.dom Dom.Int
    with 
	Not_found -> false
  in
    match a with
      | Term.App(Arith(Num(q)), []) ->
	  Q.is_integer q
      | Term.App(Arith(Multq(q)), [x]) ->
	  Q.is_integer q &&
	  is_int f x
      | Term.App(Arith(Add), xl) ->
	  List.for_all (is_int f) xl
      | _ ->
	  false

(** Test if all variables are interpreted in the integers. *)
let rec is_diophantine f a =
  try
    let rec loop = function
      | Term.App(Arith(Num(_)), []) -> 
	  true
      | Term.App(Arith(Multq(_)), [x]) -> 
	  loop x
      | Term.App(Arith(Add), xl) -> 
	  List.for_all loop xl
      | a -> 
	  Dom.eq (f a).dom Dom.Int
    in 
      loop a
  with
      Not_found -> false


