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

(** {6 Term comparison} *)

let le a b =
  let (q, ml) = Arith.poly_of a 
  and (p, nl) = Arith.poly_of b in
    Term.eql ml nl && Q.le q p

let lt a b =
  let (q, ml) = Arith.poly_of a 
  and (p, nl) = Arith.poly_of b in
    Term.eql ml nl && Q.lt q p

let less (a, alpha, b) =
  if alpha then le a b else lt a b


(** {6 Relations between constraint sets} *)

type rel = 
  | Disjoint
  | Same
  | Sub
  | Super
  | Overlap

let relpp fmt rel =
  Pretty.string fmt
    (match rel with
       | Disjoint -> "disjoint"
       | Same -> "same"
       | Sub -> "sub"
       | Super -> "super"
       | Overlap -> "overlap")


(** {6 Lower bounds} *) 

module Low = struct

  type t = 
    | Neginf 
    | Bound of bool * Term.t

  let mk_neginf = Neginf

  let mk_strict a = Bound(false, a)

  let mk_nonstrict a = Bound(true, a)

  let d_num = function
    | Bound(alpha, Term.App(Arith(Num(q)), [])) -> 
	(alpha, q)
    | _ -> 
	raise Not_found

  let mk_low d l =   (* normalize according to domain [d] *)
    try
      let (alpha, q) = d_num l in
	if d = Dom.Int && not alpha then
	  let q' = Q.of_z(Q.floor(Q.add q Q.one)) in
	    mk_nonstrict (Arith.mk_num q')
	else if d = Dom.Nonint && alpha && Q.is_integer q then
	  mk_strict (Arith.mk_num q)
	else 
	  l
    with
	Not_found -> l

  let eq l m =
    match l, m with
      | Neginf, Neginf -> 
	  true
      | Bound(alpha, a), Bound(beta, b) -> 
	  alpha = beta && Term.eq a b
      | _ -> 
	  false

  let le l m =
    match l, m with
      | Neginf, _ -> 
	  true
      | Bound _, Neginf -> 
	  false
      | Bound(false, a), Bound(true, b) -> 
	  lt a b
      | Bound(_, a), Bound(_, b) -> 
	  le a b

  let ge l m = le m l

  let pp fmt = function
    | Neginf -> 
	Pretty.string fmt "(-inf"
    | Bound(alpha, a) -> 
	Pretty.string fmt (if alpha then "[" else "(");
	Term.pp fmt a

end

module High = struct

  type t = 
    | Posinf
    | Bound of Term.t * bool

  let mk_posinf = Posinf

  let mk_strict a = Bound(a, false)

  let mk_nonstrict a = Bound(a, true)

  let d_num = function
    | Bound(Term.App(Arith(Num(p)), []), beta) -> (p, beta)
    | _ -> raise Not_found

  let mk_high d u =
    try
      let (p, beta) = d_num u in
	if d = Dom.Int && not beta then
	  let p' = Q.of_z(Q.ceil(Q.sub p Q.one)) in
	    mk_nonstrict (Arith.mk_num p')
	else if d = Dom.Nonint && beta && Q.is_integer p then
	  mk_strict (Arith.mk_num p)
	else 
	  u
    with
	Not_found -> u

  let eq l m =
    match l, m with
      | Posinf, Posinf -> true
      | Bound(a, alpha), Bound(b, beta) -> alpha = beta && Term.eq a b
      | _ -> false

  let le u v =
    match u, v with
      | _, Posinf -> true
      | Posinf, Bound _ -> false
      | Bound(a, true), Bound (b, false) -> lt a b
      | Bound(a, _), Bound(b, _) -> le a b

  let ge u v = le v u

  let pp fmt = function
    | Posinf -> Pretty.string fmt "inf)"
    | Bound(b, beta) -> 
	Term.pp fmt b;
	Pretty.string fmt (if beta then "]" else ")")

end

(** {6 Comparisons between lower and upper bounds} *)

module Supinf = struct

  let lt u l = 
    match u, l with
      | High.Posinf, _ -> 
	  false
      | _, Low.Neginf -> 
	  false
      | High.Bound(b, true), Low.Bound(true, a) -> 
	  lt b a
      | High.Bound(b, _), Low.Bound(_, a) -> 
	  le b a
  
end 


(** {6 Intervals} *)

module Interval = struct

  type t = Low.t * High.t

  let mk_empty = 
    let zero = Arith.mk_zero in
    (Low.mk_strict zero, High.mk_strict zero)

  let is_empty i = (i == mk_empty)

  let mk_full = (Low.mk_neginf, High.mk_posinf)

  let mk_singleton q =
    let n = Arith.mk_num q in
      (Low.mk_nonstrict n, High.mk_nonstrict n)

  let mk_equal a =
    match Arith.d_num a with
      | Some(q) -> mk_singleton q 
      | None -> (Low.mk_nonstrict a, High.mk_nonstrict a)

  let mk_zero = mk_singleton Q.zero
  let mk_one = mk_singleton Q.one

  let mk_less d u = (Low.Neginf, High.mk_high d u)

  let mk_greater d l = (Low.mk_low d l, High.Posinf)

  let d_singleton (l, u) =
    match l, u with
      | Low.Bound(true, a), High.Bound(b, true)
	  when Term.eq a b ->
	  a
      | _ ->
	  raise Not_found

  let pp fmt (l, u) =
    Low.pp fmt l;
    Pretty.string fmt "..";
    High.pp fmt u

  let occurs x (l, u) =
    match l, u with
      | Low.Neginf, High.Posinf -> 
	  false
      | Low.Bound(_, a), High.Posinf -> 
	  Term.occurs x a
      | Low.Neginf, High.Bound(b, _) -> 
	  Term.occurs x b
      | Low.Bound(_, a), High.Bound(b, _) ->
	  Term.occurs x a || Term.occurs x b

  let notin a (l, u) =
    match l, u with
      | Low.Neginf, High.Posinf -> 
	  false
      | Low.Bound(alpha, b), High.Posinf -> 
	  if alpha then lt a b else le a b
      | Low.Neginf, High.Bound(c, beta) ->
	  if beta then lt c a else le c a
      | Low.Bound(alpha, b), High.Bound(c, beta) -> 
	  (if alpha then lt a b else le a b) ||
	  (if beta then lt c a else le c a)
	  
  let eq (l1, u1) (l2, u2) =
    Low.eq l1 l2 && High.eq u1 u2

  let sub (l1, u1) (l2, u2) = 
    Low.ge l1 l2 && High.le u1 u2

  let disjoint (l1, u2) (l2, u2) =
    Supinf.lt u2 l2 || Supinf.lt u2 l1
    
  let cmp i j =
    if disjoint i j then Disjoint
    else if eq i j then Same
    else if sub i j then Sub
    else if sub j i then Super
    else Overlap

  let cmp i j =
    Trace.call "foo" "Cmp" (i, j) (Pretty.pair pp pp);
    let k = cmp i j in
      Trace.exit "foo" "Cmp" k relpp;
      k

  let varfold f (l, u) e =
    match l, u with
      | Low.Neginf, High.Posinf -> 
	  e
      | Low.Neginf, High.Bound(b, _) -> 
	  Term.fold f b e
      | Low.Bound(_, a), High.Posinf -> 
	  Term.fold f a e
      | Low.Bound(_, a), High.Bound(b, _) -> 
	  Term.fold f a (Term.fold f b e)
	
  let inconsistent l u =
    match l, u with
      | Low.Neginf, _ -> false
      | _, High.Posinf -> false
      | Low.Bound(alpha, a), High.Bound(b, beta) ->
	  if alpha && beta then lt b a else le b a
    
  let rec make d l u =
    if inconsistent l u then 
      mk_empty 
    else if d = Dom.Real then
      (l, u)
    else 
      let l' = Low.mk_low d l 
      and u' = High.mk_high d u in
	if inconsistent l' u' then 
	  mk_empty 
	else 
	  (l', u')

  let addq d q ((l, u) as i) = 
    if Mpa.Q.is_zero q then
      i
    else
      match l, u with
	| Low.Bound(alpha, a), High.Bound(b, beta) ->
	    let l' = Low.Bound(alpha, Arith.mk_addq q a)
	    and u' = High.Bound(Arith.mk_addq q b, beta) in
	    make d l' u'
	| Low.Bound(alpha, a), High.Posinf ->
	    make d (Low.Bound(alpha, Arith.mk_addq q a)) High.Posinf
	| Low.Neginf, High.Bound(b, beta) ->
	    make d Low.Neginf (High.Bound(Arith.mk_addq q b, beta))
	| _ ->
	    i

  let multq d q ((l, u) as i) =
    if Q.is_zero q then mk_zero
    else if Q.is_one q then i 
    else if Q.is_pos q then
      let l' = match l with
	| Low.Neginf -> Low.Neginf
	| Low.Bound(alpha, a) -> Low.mk_low d (Low.Bound(alpha, Arith.mk_multq q a))
      and u' = match u with
	| High.Posinf -> High.Posinf
	| High.Bound(b, beta) -> High.mk_high d (High.Bound(Arith.mk_multq q b, beta))
      in
	(l', u')
    else (* [Q.is_neg q] *)
      let u' = match l with
	| Low.Neginf -> High.Posinf
	| Low.Bound(alpha, a) -> High.mk_high d (High.Bound(Arith.mk_multq q a, alpha))
      and l' = match u with
	| High.Posinf -> Low.Neginf
	| High.Bound(b, beta) -> Low.mk_low d (Low.Bound(beta, Arith.mk_multq q b))
      in
	(l', u')
	
 let add d (l1, u1) (l2, u2) =
   let l = match l1, l2 with
     | Low.Neginf, _ -> Low.Neginf
     | _, Low.Neginf -> Low.Neginf
     | Low.Bound(alpha1, a1), Low.Bound(alpha2, a2) ->
	 Low.Bound(alpha1 && alpha2, Arith.mk_add a1 a2)
   and u = match u1, u2 with
     | High.Posinf, _ -> High.Posinf
     | _, High.Posinf -> High.Posinf
     | High.Bound(b1, beta1), High.Bound(b2, beta2) ->
	 High.Bound(Arith.mk_add b1 b2, beta1 && beta2)
   in
     make d l u
   

 let mult d c1 c2 = mk_full
		      
 let expt d n c =
   if n = 0 then
     mk_one
   else if n < 0 then
     mk_full
   else
     mk_full

 let div c1 c2 = mk_full

 let replace d x y ((l, u) as i) =
   match l, u with
     | Low.Neginf, High.Posinf -> i
     | Low.Neginf, High.Bound(b, beta) ->
	 let b' = Arith.replace x y b in
	   if b == b' then i else 
	     make d Low.Neginf (High.Bound(b', beta))
     | Low.Bound(alpha, a), High.Posinf ->
	 let a' = Arith.replace x y a in
	   if a == a' then i else 
	     make d (Low.Bound(alpha,a')) High.Posinf
     | Low.Bound(alpha, a), High.Bound(b, beta) ->
	 let a' = Arith.replace x y a 
	 and b' = Arith.replace x y b in
	   if a' == a && b' == b then i else
	     let l' = Low.Bound(alpha, a') 
	     and u' = High.Bound(b', beta) in
	       make d l' u'

 let is_numeric (l, h) =
   match l, h with
      | Low.Neginf, High.Posinf -> 
	  true
      | Low.Neginf, High.Bound(b, _) ->
	  Arith.is_num b
      | Low.Bound(_, a), High.Posinf ->
	  Arith.is_num a
      | Low.Bound(_, a), High.Bound(b, _) ->
	  Arith.is_num a && Arith.is_num b

end 

(** {6 Intersection of intervals} *)

module Intervals = struct

  module Inter = Set.Make(
    struct
      type t = Interval.t
      let compare i j = 
	if Interval.eq i j then 0 else Pervasives.compare i j
    end)

  type t = Inter.t

  let mk_empty = Inter.singleton (Interval.mk_empty)

  let is_empty is = (is == mk_empty)

  let inject = Inter.singleton

  let mk_full = Inter.empty

  let mk_singleton q = inject (Interval.mk_singleton q)

  let mk_zero = mk_singleton Q.zero
  let mk_one = mk_singleton Q.one

  let to_list = Inter.elements

  let eq = Inter.equal

  let sub is js = (* empty intervals interpreted as unconstrained. *)
    match Inter.is_empty is, Inter.is_empty js with
      | true, true -> true
      | true, false -> false
      | false, true -> true
      | false, false ->
	  (Inter.for_all
	     (fun i -> Inter.exists (Interval.sub i) js) is)


  exception Found

  let choose p is =
    let result = ref (Obj.magic 1) in
      try
	Inter.iter
	  (fun i ->
	     if p i then 
	       begin
		 result := i;
		 raise Found
	       end)
	  is;
	raise Not_found
      with
	  Found -> !result
 
  (** [[l1, u1] inter [l2, u2]] reduces to [[l2, u1]] if [l1 <= l2] and [u1 <= u2]. *)
  let inter1 dom ((l1, u1) as i) js = 
    if Interval.is_empty i then 
      mk_empty
    else if Inter.exists (fun j -> Interval.sub j i) js then
      js
    else 
      try
	let ((l2, u2) as j) = 
	  choose 
	    (fun (l2, u2) -> 
	       Low.le l1 l2 && High.le u1 u2)
	    js
	in
	let js' = Inter.remove j js in
	  Inter.add (Interval.make dom l2 u1) js'
      with
	  Not_found ->
	    (try
	       let ((l2, u2) as j) = 
		 choose 
		   (fun (l2, u2) -> 
		      Low.le l2 l1 && High.le u2 u1)
		   js
	       in
	       let js' = Inter.remove j js in
		 Inter.add (Interval.make dom l1 u2) js'  
	     with
		 Not_found ->
		   Inter.add i js)

  let inter dom = 
    Inter.fold (inter1 dom)

  let pp fmt is =
    Pretty.list Interval.pp fmt (Inter.elements is)

  let map d f is = 
    Inter.fold 
      (fun i acc -> 
	 let j = f i in
	   if Interval.eq i j then 
	     acc
	   else
	     inter1 d j (Inter.remove i acc))
      is is

  let fold = Inter.fold
  let exists = Inter.exists
  let for_all = Inter.for_all

  let notin a = Inter.exists (Interval.notin a) 

  let addq d q = map d (Interval.addq d q)

  let multq d q = map d (Interval.multq d q)

  let add d is js =
    fold 
      (fun i ->
	 fold 
	 (fun j -> inter1 d (Interval.add d i j))
	 js)
      is
      mk_full

  let disjoint is js =
    exists (fun i -> exists (Interval.disjoint i) js) is
   
  let mult d c1 c2 = mk_full
		      
  let expt d n c =
    if n = 0 then
      mk_one
    else if n < 0 then
      mk_full
    else
      mk_full
	
  let div c1 c2 = mk_full

  let replace d x a is =
    map d (Interval.replace d x a) is 

  exception High of Mpa.Q.t * bool

  let high_of is =
    try
      Inter.iter
	(fun (_, u) ->
	   try 
	     let (p, beta) = High.d_num u in
	       raise (High(p, beta))
	   with
	       Not_found -> ())
	is;
      raise Not_found
    with
	High(p, beta) -> (p, beta)


  exception Low of bool * Mpa.Q.t 

  let low_of is =
    try
      Inter.iter
	(fun (l, _) ->
	   try 
	     let (alpha, q) = Low.d_num l in
	     raise (Low(alpha, q))
	   with
	       Not_found -> ())
	is;
      raise Not_found
    with
	Low(alpha, q) -> (alpha, q)

  let d_equalities is =
    fold
      (fun ((l, h) as i) ((es, is) as acc) ->
	 match l, h with
	   | Low.Bound(true, a), High.Bound(b, true)
	       when Term.eq a b ->
	       (Term.Set.add a es, Inter.remove i is)
	   | _ ->
	       acc)
      is
      (Term.Set.empty, is)

  let occurs x =
    Inter.exists (Interval.occurs x)

  let numeric_of is =
    Inter.fold
      (fun i is ->
	 if Interval.is_numeric i then is else Inter.remove i is)
      is is
	 
end 


type t = {
  dom : Dom.t;
  intervals : Intervals.t
}

let dom_of c = c.dom

let high_of c =
  Intervals.high_of c.intervals

let low_of c =
  Intervals.low_of c.intervals

let is_unbounded c = 
  Intervals.is_empty (c.intervals)

let pp fmt c = 
  let il = Intervals.to_list c.intervals in
    if c.dom <> Dom.Real || il = [] then
      Dom.pp fmt c.dom;
    Pretty.infixl Interval.pp " inter" fmt il


let mk_empty = {
  dom = Dom.Real;
  intervals = Intervals.inject (Interval.mk_empty)
}
let is_empty c = (c == mk_empty)

let mk_dom d = {dom = d; intervals = Intervals.mk_full}

let mk_real = mk_dom Dom.Real
let mk_int = mk_dom Dom.Int
let mk_nonint = mk_dom Dom.Nonint

let mk_nat = {
  dom = Dom.Int;
  intervals = Intervals.inject (Interval.make Dom.Int (Low.Bound(true, Arith.mk_zero)) High.Posinf)
}

let make d is =
  {dom = d; intervals = is}

let mk_singleton q =
  let d = if Q.is_integer q then Dom.Int else Dom.Nonint in
    {dom = d; 
     intervals = Intervals.inject (Interval.mk_singleton q)}

let mk_equal a =
  match Arith.d_num a with
    | Some(q) -> 
	mk_singleton q
    | None ->
	{dom = Dom.Real;
	 intervals = Intervals.inject (Interval.mk_equal a)}
  

let mk_zero = mk_singleton Q.zero
let mk_one = mk_singleton Q.one

let mk_less dom (b, beta) =
  { dom = dom;
    intervals = Intervals.inject (Interval.mk_less dom (High.Bound(b, beta))) }

let mk_greater dom (alpha, a) =
  { dom = dom;
    intervals = Intervals.inject (Interval.mk_greater dom (Low.Bound(alpha, a))) }

let numeric_of c =
  let is' = Intervals.numeric_of c.intervals in
    if is' == c.intervals then c else
      {dom = c.dom; intervals = is'}

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

let cmp c d =
  if disjoint c d then Disjoint
  else if eq c d then Same
  else if sub c d then Sub
  else if sub d c then Super
  else Overlap

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

let addq q c =
  let dom = if Dom.eq c.dom Dom.Int && Q.is_integer q then Dom.Int else Dom.Real in
    make dom (Intervals.addq dom q c.intervals)
  
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

let subtract c d =
  add c (multq Q.negone d)
	
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

let is_finite c = 
  Dom.eq c.dom Dom.Int &&
  Intervals.exists
    (function
       | (Low.Bound(_,Term.App(Arith(Num _), [])), 
	  High.Bound(Term.App(Arith(Num _), []), _)) -> true
       | _ -> false)
    c.intervals

let is_full c =
  Dom.eq c.dom Dom.Real &&
  Intervals.is_empty c.intervals

let notin a c = 
  (Intervals.notin a c.intervals) ||
  (match Arith.d_num a with
     | Some(q) -> not(Dom.mem q c.dom)
     | None -> false)

let fold f c =
  Intervals.fold f c.intervals

(** Fold over all variables. *)
let varfold f c =
  Intervals.fold (Interval.varfold f) c.intervals
  
  
let replace x a c =
  make c.dom (Intervals.replace c.dom x a c.intervals)
      
	 
  
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


(** Integer test. Incomplete. *)
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


(** Subsumption *)

let lower_is_subsumed (alpha, a) c =
  false

let upper_is_subsumed (b, beta) c =
  false

(** Derived constraints *)

let implied c =
  Intervals.fold
    (fun (l, u) acc ->
       match l, u with
	 | Low.Neginf, _ -> 
	     acc
	 | _, High.Posinf -> 
	     acc
	 | Low.Bound(true, a), High.Bound(b, true) ->
	     if le a b then acc else Arith.mk_le a b :: acc
	 | Low.Bound(_, a), High.Bound(b, _) ->
	     if lt a b then acc else Arith.mk_lt a b :: acc)
    c.intervals []

let equal x c =
  Intervals.fold
    (fun (l, u) acc ->
       match l, u with
	 | Low.Neginf, High.Posinf -> 
	     acc
	 | Low.Bound(alpha, a), High.Posinf -> 
	     Arith.mk_greater (x, alpha, a) :: acc
	 | Low.Neginf, High.Bound(b, beta) -> 
	     Arith.mk_less (x, beta, b) :: acc
	 |  Low.Bound(alpha, a), High.Bound(b, beta) ->
	       Arith.mk_greater (x, alpha, a) ::  Arith.mk_less (x, beta, b) :: acc)
    c.intervals []


let occurs x c =
  Intervals.occurs x c.intervals


