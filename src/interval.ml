(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*i*)
open Mpa
open Binrel
open Tools
(*i*)

(*s Interpretation domains. *)

type domain = Int | Real | NonintReal

let domain_union dom1 dom2 =
  if dom1 = dom2 then dom1
  else match dom1, dom2 with
    | Real, _ -> Real
    | _, Real -> Real
    | (NonintReal | Int), (NonintReal | Int) -> Real

exception Empty

let domain_inter dom1 dom2 =
  if dom1 = dom2 then dom1
  else match dom1, dom2 with
    | (Int | Real), (Int | Real) -> Int
    | (Int | NonintReal), (Int | NonintReal) -> raise Empty
    | (Real | NonintReal), (Real | NonintReal) -> NonintReal    

(*s Lower and upper bounds of possibly infinite intervals. *)

type kind = Strict | Nonstrict

let toggle = function
  | Strict -> Nonstrict
  | Nonstrict -> Strict
	
type low =
  | Neginf
  | Low of kind * Q.t

type high =
  | Posinf
  | High of kind * Q.t

let low_cmp i1 i2 =
  match i1, i2 with
    | Neginf, Neginf -> Equal
    | Neginf, _ -> Less
    | _, Neginf -> Greater
    | Low(k1,q1), Low(k2,q2) ->
	(match Q.cmp q1 q2 with                               (* e.g. [Nonstrict(q)] is less then [Strict(q)] *)
	   | Q.Equal -> if k1 = k2 then Equal else if k1 = Strict then Greater else Less
	   | Q.Greater -> Greater
	   | Q.Less -> Less)

let high_cmp j1 j2 =
  match j1,j2 with
    | Posinf, Posinf -> Equal
    | Posinf, _ -> Greater
    | _, Posinf -> Less
    | High(k1,q1), High(k2,q2) ->              (* e.g. [Strict(q)] is less then [Nonstrict(q)] *)
	(match Q.cmp q1 q2 with
	   | Q.Equal -> if k1 = k2 then Equal else if k1 = Strict then Less else Greater
	   | Q.Less -> Less
	   | Q.Greater -> Greater)
	  
let low_high_cmp i j =
  match i, j with
    | Neginf, _ -> Less
    | _, Posinf -> Greater
    | Low(k1,q1), High(k2,q2) ->
	(match Q.cmp q1 q2 with
	   | Q.Equal ->
	       (match k1,k2 with
		  | Strict,Strict -> Less
		  | Nonstrict,Nonstrict -> Greater
		  | _ -> Equal)                         (* [Equal] actually means adjacent here *)
	   | Q.Less -> Less
	   | Q.Greater -> Greater)

(*s An interval consists of a lower bound, an upper bound, and the
  domain over which it is interpreted. *)

type interval = domain * low * high

let is_bot_interval (dom,l,h) =
  match l,h with
    | Neginf, _ -> false
    | _, Posinf -> false
    | Low(Nonstrict,q1), High(Nonstrict,q2) ->
	  Q.gt q1 q2 
    | Low(_,q1), High(_,q2) -> Q.ge q1 q2

		  
let rec interval dom l h =
  match dom with
    | Real -> real_interval l h
    | Int -> int_interval l h
    | NonintReal -> nonintreal_interval l h

and real_interval l h =
  (Real,l,h)

and int_interval l h =
  let l' = match l with
    | Low(Nonstrict, q) when Q.is_integer q -> l
    | Low(_,q) -> Low(Nonstrict, Q.of_z(Q.floor(Q.add q Q.one)))
    | _ -> l
  and h' = match h with
    | High(Nonstrict,q) when Q.is_integer q -> h
    | High(_,q) -> High(Nonstrict,Q.of_z(Q.ceil(Q.sub q Q.one)))
    | _ -> h
  in
  (Int,l',h')

and nonintreal_interval l h =
  let l' = match l with
    | Low(Nonstrict,q) when Q.is_integer q ->
	Low(Strict,q)
    | _ -> l
  and h' = match h with
    | High(Nonstrict,q) when Q.is_integer q ->
	High(Strict,q)
    | _ -> h
  in
  (NonintReal,l',h')

let interval_mem q (dom,l,h) =
  let memq q (i,j) =
    let upfrom q = function
      | Neginf -> true
      | Low(k,p) -> Q.gt q p || (Q.equal q p && k = Nonstrict)
    in
    let downfrom q = function
      | Posinf -> true
      | High(k,p) -> Q.lt q p || (Q.equal q p && k = Nonstrict)
    in
    upfrom q i && downfrom q j
  in
  match dom with
    | Int -> Q.is_integer q && memq q (l,h)
    | NonintReal -> not(Q.is_integer q) && memq q (l,h)
    | Real -> memq q (l,h)
  
let interval_eq (dom1,l1,h1) (dom2,l2,h2) =
  dom1 = dom2 && low_cmp l1 l2 = Equal && high_cmp h1 h2 = Equal

let rec interval_compl (dom,l,h) =
  match dom with
    | Real -> real_interval_compl l h
    | Int -> int_interval_compl l h
    | NonintReal -> nonintreal_interval_compl l h

and real_interval_compl l h =
  match l, h with
    | Neginf, Posinf ->
	[]
    | Neginf, High(k,q) ->
	[interval Real (Low(toggle k,q)) Posinf]
    | Low(k,q),Posinf ->
	[interval Real Neginf (High(toggle k,q))]
    | Low(k1,q1),High(k2,q2) ->
	[interval Real Neginf (High(toggle k1,q1)); interval Real (Low(toggle k2,q2)) Posinf]

and int_interval_compl l h =
  match l, h with
    | Neginf, Posinf ->
	[interval NonintReal Neginf Posinf]
    | Neginf, High(k,q) ->
	[interval NonintReal Neginf h; interval Real (Low(toggle k,q)) Posinf]
    | Low(k,q),Posinf ->
	[interval Real Neginf (High(toggle k,q)); interval NonintReal l Posinf]
    | Low(k1,q1),High(k2,q2) ->
	[interval Real Neginf (High(toggle k1,q1));
	 interval NonintReal l h;
	 interval Real (Low(toggle k2,q2)) Posinf]

and nonintreal_interval_compl l h =
  failwith "to do"
	 

(*s Disjunction of Intervals. Invariant: intervals are disjoint, o
  rdered from left-to-right, and no interval is empty
  (when interpreted over the reals).
*)

type t = interval list

	   (*s Constructing Intervals. *)
			    
let bot = []
let top  = [Real,Neginf,Posinf]
let int   = [Int,Neginf,Posinf]
let real  = top
let nonint =  ([NonintReal,Neginf,Posinf])

let inj i = [i]
	
			      
let oo dom p q =
  let i = interval dom (Low(Strict,p)) (High(Strict,q)) in
  if is_bot_interval i then bot else [i]
    
let oc dom p q =
  let i = interval dom (Low(Strict,p)) (High(Nonstrict,q)) in
  if is_bot_interval i then bot else [i]
  
let co dom p q =
  let i = interval dom (Low(Nonstrict,p)) (High(Strict,q)) in
  if is_bot_interval i then bot else [i]
  
let cc dom p q =
  let i = interval dom (Low(Nonstrict,p)) (High(Nonstrict,q)) in
  if is_bot_interval i then bot else [i]
        
let lt dom p = [interval dom Neginf (High(Strict,p))]
let le dom p = [interval dom Neginf (High(Nonstrict,p))]
let gt dom p = [interval dom (Low(Strict,p)) Posinf]
let ge dom p = [interval dom (Low(Nonstrict,p)) Posinf]
 
let singleton q =
  let dom = if Q.is_integer q then Int else NonintReal in
  [interval dom (Low(Nonstrict,q)) (High(Nonstrict,q))]

let diseq q =
  let dom = Real in
  let i1 = interval dom Neginf (High(Strict,q)) in
  let i2 = interval dom (Low(Strict,q)) Posinf in
  [i1;i2]

	     (* Membership, equality, and containment *)

let mem q l =
  List.exists (interval_mem q) l

let eq l1 l2 =
  try List.for_all2 interval_eq l1 l2 with Invalid_argument _ -> false

    
      (*s Some useful recognizers. *)

let is_bot l = (l = [])
		   
let is_top = function
  | [Real,Neginf,Posinf] -> true
  | _ -> false

let is_singleton = function
  | [_,Low(Nonstrict,q1), High(Nonstrict,q2)] -> Q.equal q1 q2
  | _ -> false

let is_real l =
  true
    
let is_int l =
  List.for_all (fun (dom,_,_) -> dom = Int) l
		 
let is_nonintreal l =
  List.for_all (fun (dom,_,_) -> dom = NonintReal) l
		       
	
      (*s Get the value of a singleton set. *)

let value_of = function
  | [_,Low(Nonstrict,q1), High(Nonstrict,q2)] when Q.equal q1 q2 -> q1
  | _ -> raise (Invalid_argument "Not a singleton")

	(*s Fold function *)

let fold flt fle fgt fge foo fco foc fcc ftop fbot l  =
  List.fold_right (fun (d,l,h) acc ->
		     match l, h with
		       | Neginf, High(Strict,q) -> flt d q acc
		       | Neginf, High(Nonstrict,q) -> fle d q acc
		       | Neginf, Posinf -> ftop d acc
		       | Low(Strict,q), Posinf -> fgt d q acc
		       | Low(Nonstrict,q), Posinf -> fge d q acc
		       | Low(Strict,q1), High(Strict,q2) -> foo d (q1,q2) acc
		       | Low(Strict,q1), High(Nonstrict,q2) -> foc d (q1,q2) acc
		       | Low(Nonstrict,q1), High(Strict,q2) -> fco d (q1,q2) acc     
		       | Low(Nonstrict,q1), High(Nonstrict,q2) -> fcc d (q1,q2) acc)
    l
    fbot		     

	
	(*s Pretty printing *)

let low_pp fmt = function
  | Neginf -> Format.fprintf fmt "( "
  | Low(k,q) -> Format.fprintf fmt "%s" (if k = Strict then "(" else "["); Q.pp fmt q

let high_pp fmt = function
  | Posinf -> Format.fprintf fmt " )"
  | High(k,q) -> Q.pp fmt q; Format.fprintf fmt "%s" (if k = Strict then ")" else "]")

let interval_pp fmt (dom,i,j) =
  let domstr = match dom with Real -> "" | Int -> "int" | NonintReal -> "nonintreal" in
  Format.fprintf fmt "@[%s" domstr; 
  low_pp fmt i;
  Format.fprintf fmt "..";
  high_pp fmt j;
  Format.fprintf fmt "@]"

let rec pp_intervals fmt = function
  | [] -> ()
  | [i] -> interval_pp fmt i
  | i :: l -> interval_pp fmt i; Format.fprintf fmt "@ union@ "; pp_intervals fmt l

let pp fmt l =
  match l with
    | [Real,Neginf, Posinf] ->
	Format.fprintf fmt "real"
    | [Int,Neginf,Posinf] ->
	Format.fprintf fmt "int"
    | [NonintReal,Neginf,Posinf] ->
	Format.fprintf fmt "nonintreal"
    | _ ->
        Format.fprintf fmt "@[";
        pp_intervals fmt l;
	Format.fprintf fmt "@]"


    (*s Greatest lower bound of two constraints. Traverse two
      constraints from left-to-right. If the lower bounds of
      the currently processes intervals are equal, then add
      the maximally shared interval and continue with the remaining
      intervals (including the parts of the current intervals not in
      the intersection). If the lower bounds are not equal, then
      keep traversing. *)

let upper dom j1 j2 =
  match j1 with
    | High(k,q) ->
	interval dom (Low(toggle k, q)) j2
    | _ -> assert false

let add (dom,i,j) l =
  let rho = interval dom i j in
  if is_bot_interval rho then l else rho :: l

let single dom i j =
  let rho = interval dom i j in
  if is_bot_interval rho then bot else [rho]

let rec inter l1 l2 =
  match l1,l2 with
    | [Real,Neginf,Posinf], _ -> l2
    | _, [Real,Neginf,Posinf] -> l1
    | [], _ -> []
    | _, [] -> []
    | (dom1,i1,j1) :: l1', (dom2,i2,j2) :: l2' ->
	(match low_cmp i1 i2, high_cmp j1 j2 with
	   | Equal, Equal ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add (interval dom i1 j1) (inter l1' l2')
		with
		    Empty -> inter l1' l2')
	   | Equal, Greater ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add (interval dom i2 j2) (inter (add (upper dom1 j2 j1) l1') l2')
		with
		    Empty -> inter (add (upper dom1 j2 j1) l1') l2')
	   | Equal, Less ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add (interval dom i1 j1) (inter l1' (add (upper dom2 j1 j2) l2'))
		with
		    Empty -> inter l1' (add (upper dom2 j1 j2) l2'))
	   | Less, _ ->
	       inter (add (interval dom1 i2 j1) l1') l2
	   | Greater, Greater ->
	       inter l1 (add (interval dom2 i1 j2) l2')
	   | Greater, Less ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add (interval dom i1 j1) (inter l1' (add (upper dom2 j1 j2) l2'))
		with
		    Empty -> inter l1' (add (upper dom2 j1 j2) l2'))
	   | Greater, Equal ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add (interval dom i1 j1) (inter l1' l2')
		with
		    Empty -> inter l1' l2))
	
let low_to_high = function
  | Low(k,q) -> High(toggle k,q)
  | _ -> assert false

let high_to_low = function
  | High(k,q) -> Low(toggle k,q)
  | _ -> assert false
	
let rec union l1 l2 =
  match l1, l2 with
    | [Real,Neginf,Posinf], _ -> top
    | _, [Real,Neginf,Posinf] -> top
    | [], _ -> l2
    | _, [] -> l1
    | rho1 :: l1', rho2 :: l2' ->
	interval_union rho1 rho2 @ union l1' l2'
	
and interval_union ((dom1,i1,j1) as rho1) ((dom2,i2,j2) as rho2) =
  match low_cmp i1 i2, high_cmp j1 j2 with
    | (Equal| Less), (Equal | Greater) ->        (* [rho2] sub [rho1] *)      
       single (domain_union dom1 dom2) i1 j1
    | Equal, Less
    | Greater, (Equal | Less) ->                  (* [rho1] sub [rho2] *)
	single (domain_union dom1 dom2) i2 j2     
    | Less, Less ->                               (* bounds of [rho1] are less than the ones of [rho2] *)
	(match low_high_cmp i2 j1 with
	   | Greater ->                           (* [rho1] strictly before [rho2] *)
	       [rho1; rho2]
	   | Equal ->                             (* [rho1] followed immediately by [rho2] *)
	       if dom1 = dom2 then
		 single dom1 i1 j2
	       else
		 [rho1; rho2]
	   | Less ->                              (* [rho1] starts before and overlaps [rho2] *)
	       if dom1 = dom2 then
		 single dom1 i1 j2
	       else
		 single dom1 i1 (low_to_high i2) @
		 single (domain_union dom1 dom2) i2 j1 @
		 single dom2 (high_to_low j1) j2)
    | Greater, Greater ->                         (* [rho1] bounds are greater than the ones of [rho2] *)
	(match low_high_cmp i1 j2 with
	   | Greater ->                         
	       [rho2; rho1]
	   | Equal ->                         
	       if dom1 = dom2 then
		 single dom1 i2 j1
	       else
		 [rho2; rho1]
	   | Less ->
	       if dom1 = dom2 then
		 single dom1 i2 j1
	       else
		 single dom2 i2 (low_to_high i1) @
		 single (domain_union dom1 dom2) i1 j2 @
		 single dom1 (high_to_low j2) j1)
  
let rec compl l =
  match l with
    | [] -> top
    | [rho] -> interval_compl rho
    | rho :: l ->
	inter (interval_compl rho) (compl l)

let ite l1 l2 l3 =
  union (inter l1 l2)
    (inter (compl l1) l3)

	  
     (*s Comparison between two constraints. *)
	  
type rel = Binrel.t

let cmp l1 l2 =
  match inter l1 l2 with
    | [] ->
	Disjoint
    | l ->
	(match eq l1 l, eq l2 l with
	   | true, true -> Same
	   | true, _ -> Sub
	   | _, true -> Super
	   | _ -> Overlap)

let is_disjoint l1 l2 =
  inter l1 l2 = []

    (*s Abstract interpretation of addition and multiplication. *)
  
let mult1 (dom1,i1,j1) (dom2,i2,j2) =
  let kind k1 k2 =
    match k1, k2 with
      | Nonstrict, Nonstrict -> Nonstrict
      | _ -> Strict
  in
  let mult_bound (k1,q1) (k2,q2) =
    (kind k1 k2, Q.mult q1 q2)
  in
  let gt ((k1,q1) as v1) ((k2,q2) as v2) =
    Q.gt q1 q2 || (Q.equal q1 q2 && k1 = Strict && k2 = Nonstrict)
  in
  let min v1 v2 =
    if gt v1 v2 then v2 else v1
  in
  let max v1 v2 =
    if gt v1 v2 then v1 else v2
  in
  let dom = domain_union dom1 dom2 in
  match i1,j1,i2,j2 with
    | Neginf, Posinf, _, _
    | _, _, Neginf, Posinf ->
	interval dom Neginf Posinf
    | Low(k1,i1), Posinf, Low(k2,i2), Posinf ->
	if Q.ge i1 Q.zero && Q.ge i2 Q.zero then
	  interval dom (Low(kind k1 k2, Q.mult i1 i2)) Posinf
	else
	  interval dom Neginf Posinf
    | Low(k1,i1), Posinf, Neginf, High(k2,j2) ->
	interval dom Neginf Posinf (*i to do i*)
    | Low(k1,i1), Posinf, Low(k3,i2), High(k4,j2) ->
	interval dom Neginf Posinf (*i to do*)
    | Low(k1,i1), High(k2,j1), Neginf, High(k4,j2) ->
	interval dom Neginf Posinf (*i to do i*)
    | Low(k1,i1), High(k2,j1), Low(k3,i2), Posinf ->
	interval dom Neginf Posinf (*i to do i*)
    | Neginf, High(k2,j1), Low(k3,i2), Posinf ->
	interval dom Neginf Posinf (*i to do i*)
    | Neginf, High(k2,j1), Low(k3,i2), High(k4,j2) ->
	interval dom Neginf Posinf (*i to do i*)
    | Neginf, High(k2,j1), Neginf, High(k4,j2) ->
	if Q.le j1 Q.zero && Q.le j2 Q.zero then
	  interval dom (Low(kind k2 k4, Q.mult j1 j2)) Posinf
	else
	  interval dom Neginf Posinf
    | Low(k1l,i1), High(k1h,j1), Low(k2l,i2), High(k2h,j2) ->
	let i1i2 = mult_bound (k1l,i1) (k2l,i2) in
	let i1j2 = mult_bound (k1l,i1) (k2h,j2) in
	let i2j1 = mult_bound (k2l,i2) (k1h,j1) in
	let j1j2 = mult_bound (k1h,j1) (k2h,j2) in
	let (k,i) = min i1i2 (min i1j2 (min i2j1 j1j2)) in
	let (l,j) = max i1i2 (max i1j2 (max i2j1 j1j2)) in
	interval dom (Low(k,i)) (High(l,j))
		 
let mult l1 l2 =
  List.fold_right
    (fun rho1 acc1 ->
       (List.fold_right
	  (fun rho2 acc2 ->
	     let rho = mult1 rho1 rho2 in
	     interval_pp Format.std_formatter rho;
	     if is_bot_interval rho then acc2 else union [rho] acc2)
	  l2 acc1))
    l1 []


let multq q l =
  let rec revmap acc = function
    | [] -> acc
    | (dom,i,j) :: l ->
	let j' = match i with
	  | Neginf -> Posinf
	  | Low(k1,q1) -> High(k1,Q.mult q q1)
	in
	let i' = match j with
	  | Posinf -> Neginf
	  | High(k2,q2) -> Low(k2,Q.mult q q2)
	in
	revmap (interval dom i' j' :: acc) l
  in
  match l with
    | [_,Neginf,Posinf] ->
	l  
    | _ ->
	(match Q.cmp q Q.zero with
	   | Q.Equal ->
	       singleton Q.zero
	   | Q.Greater ->
	       List.map
		 (fun (dom,i,j) ->
		    let i' = match i with
		      | Neginf -> Neginf
		      | Low(k1,q1) -> Low(k1,Q.mult q q1)
		    in
		    let j' = match j with
		      | Posinf -> Posinf
		      | High(k2,q2) -> High(k2,Q.mult q q2)
		    in
		    interval dom i' j')
		 l
	   | Q.Less ->
	       revmap [] l)

    
let add1 (dom1,i1,j1) (dom2,i2,j2) =
  let dom = domain_union dom1 dom2 in
  let i = match i1,i2 with
    | Neginf, _ ->
	Neginf
    | _, Neginf ->
	Neginf
    | Low(Nonstrict,q1),Low(Nonstrict,q2) ->
	Low(Nonstrict, Q.add q1 q2)
    | Low(_,q1),Low(_,q2) ->
	Low(Strict, Q.add q1 q2)
  in
  let j = match j1,j2 with
    | Posinf, _ ->
	Posinf
    | _, Posinf ->
	Posinf
    | High(Nonstrict,q1),High(Nonstrict,q2) ->
	High(Nonstrict, Q.add q1 q2)
    | High(_,q1),High(_,q2) ->
	High(Strict, Q.add q1 q2)
  in
  interval dom i j
 
let add l1 l2 =
  List.fold_right
    (fun rho1 acc1 ->
       (List.fold_right
	  (fun rho2 acc2 ->
	     let rho = add1 rho1 rho2 in
	     if is_bot_interval rho then acc2 else union [rho] acc2)
	  l2 acc1))
    l1 []


let to_list l = l

let rec of_list = function
  | [] -> bot
  | i :: l -> union [i] (of_list l)
