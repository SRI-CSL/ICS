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

(*s Support modules for interval.ml*)

open Mpa         
open Binrel
open Tools


(*s The three interpretation domains.NonintReal represents the domain of all Reals except Integers. *)

type domain = Int | Real | NonintReal


(*s Union of two domains*)

let domain_union dom1 dom2 =
  if dom1 = dom2 then dom1 else Real(*s e.g domain_union Int Int = Int*)
  
exception Empty


(*s Intersection of two domains*)

let domain_inter dom1 dom2 =
  if dom1 = dom2 then dom1
  else match dom1, dom2 with
    | (Int | NonintReal), (Int | NonintReal) -> raise Empty(*s Int, NonintReal are disjoint sets*)
    | (Int | Real), (Int | Real) -> Int
    | (Real | NonintReal), (Real | NonintReal) -> NonintReal    



(*s Lower and Upper bounds of possibly infinite intervals.Strict represents\
 inclusive bound and Nonstrict represents exclusive bound*)

type kind = Strict | Nonstrict (*s Strict bounds-> '[',']';Nonstrict bounds-> '(',')' *)


(*s Toggle between Strict and Nonstrict bounds of an interval*)

let toggle = function
  | Strict -> Nonstrict
  | Nonstrict -> Strict
	

(*s Defines the types of lower limits.Neginf represents negative infinity*)

type low =
  | Neginf(*s represented by "(-inf" *)
  | Low of kind * Q.t(*s represented by "(Q.t" or "[Q.t" *)



(*s Defines the types of higher limits.Posinf represents positive infinity*)

type high =
  | Posinf(*s represented by "inf)" *)
  | High of kind * Q.t(*s represented by "Q.t)" or "Q.t]" *)



(*s Compares the lower limit of two intervals*)

let low_cmp i1 i2 =
  match i1, i2 with
    | Neginf, Neginf -> Equal
    | Neginf, _ -> Less
    | _, Neginf -> Greater
    | Low(k1,q1), Low(k2,q2) ->
	(match Q.cmp q1 q2 with 
	   | Q.Equal -> if k1 = k2 then Equal else if k1 = Strict then Less else Greater(*s e.g. [Nonstrict(i1)] is greater than [Strict(i2)] when i1=i2 *) 
	   | Q.Greater -> Greater
	   | Q.Less -> Less)



(*s Compares the higher limit of two intervals*)

let high_cmp j1 j2 =
  match j1,j2 with
    | Posinf, Posinf -> Equal
    | Posinf, _ -> Greater
    | _, Posinf -> Less
    | High(k1,q1), High(k2,q2) ->              
	(match Q.cmp q1 q2 with
	   | Q.Equal -> if k1 = k2 then Equal else if k1 = Strict then Greater else Less(*s e.g. [Nonstrict(j1) is less than [Strict(j2)] when j1=j2 *)
	   | Q.Less -> Less
	   | Q.Greater -> Greater)



(*s Compares the lower limit of one interval with the higher limit of another interval*)	  

let low_high_cmp i j =
  match i, j with
    | Neginf, _ -> Less
    | _, Posinf -> Less
    | Low(k1,q1), High(k2,q2) ->
	(match Q.cmp q1 q2 with
	   | Q.Equal ->
	       (match k1,k2 with
		  | Strict,Strict -> Equal
		  | _,_ -> Greater)(*s e.g. [Strict(i)] is greater than [Nonstrict(j)] when i=j *)
	   | Q.Less -> Less
	   | Q.Greater -> Greater)



(*s An interval consists of a lower bound, an upper bound, and the
  domain over which it is interpreted. *)

type interval = domain * low * high



(*s Determines wether a given interval is empty*)

let is_bot_interval (dom,l,h) =
  match l,h with
    | Neginf, _ -> false(*s an interval with a lower limit of negative infinity cannot be empty*)
    | _, Posinf -> false
    | Low(Strict,q1),High(Strict,q2)->Q.gt q1 q2(*s Q.gt checks wether q1 is strictly greater than q2 or not*)
    | Low(_,q1),High(_,q2) -> Q.ge q1 q2(*s Q.ge checks wether q1 is equal or greater than q2*)
	  

(*Places given interval in correct domain*)		  
let rec interval dom l h =
  match dom with
    | Real -> real_interval l h
    | Int -> int_interval l h
    | NonintReal -> nonintreal_interval l h

and real_interval l h =
  (Real,l,h)

and int_interval l h =
  let l' = match l with
    | Low(Strict, q) when Q.is_integer q -> l
    | Low(_,q) -> Low(Strict, Q.of_z(Q.floor(Q.add q Q.one)))(*s e.g. it interprets "(3" as"[4"*)
    | _ -> l
  and h' = match h with
    | High(Strict,q) when Q.is_integer q -> h
    | High(_,q) -> High(Strict,Q.of_z(Q.ceil(Q.sub q Q.one)))
    | _ -> h
  in
  (Int,l',h')

and nonintreal_interval l h =
  let l' = match l with
    | Low(Strict,q) when Q.is_integer q ->(*s Cannot have an inclusive lower or upper bound for an integer in the domain of nonintreals *)
	Low(Nonstrict,q)
    | _ -> l
  and h' = match h with
    | High(Strict,q) when Q.is_integer q ->
	High(Nonstrict,q)
    | _ -> h
  in
  (NonintReal,l',h')


(*Tells wether an element q is a member of a given interval*)
let interval_mem q (dom,l,h) =
  let memq q (i,j) =(*s Checks wether q lies between i and j*)
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
  

(*s Equality of two intervals*)
let interval_eq (dom1,l1,h1) (dom2,l2,h2) =
  dom1 = dom2 && low_cmp l1 l2 = Equal && high_cmp h1 h2 = Equal


(*s Finds complement of an interval*)
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
	[interval Real (Low(toggle k,q)) Posinf](*s The 'kind' of lower,upper limits is changed*)
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
  match l, h with
    | Neginf,Posinf ->
       [interval Int Neginf Posinf]
    | Neginf,High(k,q) ->
       [interval Int Neginf h; interval Real (Low(toggle k,q)) Posinf]
    | Low(k,q),Posinf ->
       [interval Real Neginf (High(toggle k,q)); interval Int l Posinf]
    | Low(k1,q1),High(k2,q2) ->
       [interval Real Neginf (High(toggle k1,q1));
       interval Int l h;
       interval Real (Low(toggle k2,q2)) Posinf]

(*s Disjunction of Intervals with these invariant properties: intervals are disjoint, ordered from left-to-right, and no interval is empty when interpreted over the domain of Reals.
*)

type t = interval list


(*s Constructing Intervals. *)
			    
let bot = [](*s An empty interval*)
let top  = [Real,Neginf,Posinf]
let int   = [Int,Neginf,Posinf]
let real  = top
let nonint =  [NonintReal,Neginf,Posinf]

let inj i = [i](*s Inject an element into the list*)
	

(*s Constructing intervals with bounds. o represents open i.e. Nonstrict and c represents closed i.e Strict*)	
		      
let oo dom p q =
  let i = interval dom (Low(Nonstrict,p)) (High(Nonstrict,q)) in
  if is_bot_interval i then bot else [i]
    
let oc dom p q =
  let i = interval dom (Low(Nonstrict,p)) (High(Strict,q)) in
  if is_bot_interval i then bot else [i]
  
let co dom p q =
  let i = interval dom (Low(Strict,p)) (High(Nonstrict,q)) in
  if is_bot_interval i then bot else [i]
  
let cc dom p q =
  let i = interval dom (Low(Strict,p)) (High(Strict,q)) in
  if is_bot_interval i then bot else [i]
    
(*s Construct intervals with infinity as one of the limits*)    

let lt dom p = [interval dom Neginf (High(Strict,p))]
let le dom p = [interval dom Neginf (High(Nonstrict,p))]
let gt dom p = [interval dom (Low(Strict,p)) Posinf]
let ge dom p = [interval dom (Low(Nonstrict,p)) Posinf]
 
(*s Construct a singleton set [q]*)

let singleton q =
  let dom = if Q.is_integer q then Int else NonintReal in
  [interval dom (Low(Strict,q)) (High(Strict,q))]


(*s Construct the set of Reals without [q]*)

let diseq q =
  let dom = Real in
  let i1 = interval dom Neginf (High(Nonstrict,q)) in
  let i2 = interval dom (Low(Nonstrict,q)) Posinf in
  [i1;i2]

	     
(*s Membership, equality, and containment of lists of intervals*)
(*s Checks wether [q] is a member of list [l]*)                                                          

let mem q l =
  List.exists (interval_mem q) l

(*s Checks wether two lists are equal *)

let eq l1 l2 =
  try List.for_all2 interval_eq l1 l2 with Invalid_argument _ -> false

    
(*s Useful recognizers. *)

(*s Checks wether [l] is an empty list*)

let is_bot l = (l = [])

(*s Checks if given interval denotation is the domain of all Reals*)
		   
let is_top = function
  | [Real,Neginf,Posinf] -> true
  | _ -> false


(*s Checks for singleton set [q]*)

let is_singleton = function
  | [_,Low(Strict,q1), High(Strict,q2)] -> Q.equal q1 q2
  | _ -> false


let is_real l =
  true
    

(*s Checks wether all elements in the list are integers*)

let is_int l =
  List.for_all (fun (dom,_,_) -> dom = Int) l
		 

(*s Checks wether all elemants in list are NonintReals*)

let is_nonintreal l =
  List.for_all (fun (dom,_,_) -> dom = NonintReal) l
		       
	
(*s Get the value of a singleton set [q]. *)

let value_of = function
  | [_,Low(Strict,q1), High(Strict,q2)] when Q.equal q1 q2 -> q1
  | _ -> raise (Invalid_argument "Not a singleton")


	
(*s Pretty printing functions*)

let low_pp fmt = function
  | Neginf -> Format.fprintf fmt "(-inf "
  | Low(k,q) -> Format.fprintf fmt "%s" (if k = Strict then "[" else "("); Q.pp fmt q

let high_pp fmt = function
  | Posinf -> Format.fprintf fmt " inf)"
  | High(k,q) -> Q.pp fmt q; Format.fprintf fmt "%s" (if k = Strict then "]" else ")")

let interval_pp fmt (dom,i,j) =
  let domstr = match dom with Real -> "real" | Int -> "int" | NonintReal -> "nonintreal" in
  Format.fprintf fmt "@[%s" domstr; 
  low_pp fmt i;
  Format.fprintf fmt "..";
  high_pp fmt j;
  Format.fprintf fmt "@]\n"

let rec pp_intervals fmt = function
  | [] -> ()
  | [i] -> interval_pp fmt i
  | i :: l -> interval_pp fmt i; Format.fprintf fmt "@ union@ "; pp_intervals fmt l

let pp fmt l =
  match l with
    | [Real,Neginf, Posinf] ->
	Format.fprintf fmt "real\n"
    | [Int,Neginf,Posinf] ->
	Format.fprintf fmt "int\n"
    | [NonintReal,Neginf,Posinf] ->
	Format.fprintf fmt "nonintreal\n"
    | _ ->
        Format.fprintf fmt "@[";
        pp_intervals fmt l;
	Format.fprintf fmt "@]\n"


(*s Converts the first bound i.e. from an upper to a lower limit*)

let upper dom j1 j2 =
  match j1 with
    | High(k,q) ->
	interval dom (Low(toggle k, q)) j2
    | _ -> assert false



(*s Adding an interval to a list*)

let add2 (dom,i,j) l=
  let rho = interval dom i j in
  if is_bot_interval rho then l else rho :: l(*s appends a non empty interval to a given list*)



(*s Creating an interval list.It differs from normal interval only in its type conventions*)

let single dom i j =
  let rho = interval dom i j in
  if is_bot_interval rho then bot else [rho]


(*s Converts a lower bound to a higher bound of different kind*)

let low_to_high = function
  | Low(k,q) -> High(toggle k,q)
  | _ -> assert false



(*s Converts a higher bound to a lower bound of a different kind*)

let high_to_low = function
  | High(k,q) -> Low(toggle k,q)
  | _ -> assert false
	

(*s Finds the interval of two lists*)
    
let rec inter l1 l2 =
  match l1,l2 with
    | [Real,Neginf,Posinf], _ -> l2
    | _, [Real,Neginf,Posinf] -> l1
    | [], _ -> []
    | _, [] -> []
    | (dom1,i1,j1) :: l1', (dom2,i2,j2) :: l2' ->
	(match low_cmp i1 i2, high_cmp j1 j2 with
	   | Equal, Equal ->(*s Cases are constructed by comparing the lower and upper limits of the two intervals*)
	       (try
		  let dom = domain_inter dom1 dom2 in 
		  add2 (interval dom i1 j1) (inter l1' l2')
		with
		    Empty -> inter l1' l2')(*s Empty denotes the case when the two intervals are disjoint*)
	   | Equal, Greater ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add2 (interval dom i2 j2) (inter (add2 (upper dom1 j2 j1) l1') l2')
		with
		    Empty -> inter (add2 (upper dom1 j2 j1) l1')l2')
	   | Equal, Less ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add2 (interval dom i1 j1) (inter l1' (add2 (upper dom2 j1 j2) l2'))
		with
		    Empty -> inter l1' (add2 (upper dom2 j1 j2) l2'))
	   | Less, _ ->
	       inter (add2 (interval dom1 i2 j1) l1') l2
	   | Greater, Greater ->
	       inter l1 (add2 (interval dom2 i1 j2) l2')
	   | Greater, Less ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add2 (interval dom i1 j1) (inter l1' (add2 (upper dom2 j1 j2) l2'))
		with
		    Empty -> inter l1' (add2 (upper dom2 j1 j2) l2'))
	   | Greater, Equal ->
	       (try
		  let dom = domain_inter dom1 dom2 in
		  add2 (interval dom i1 j1) (inter l1' l2')
		with
		    Empty -> inter l1' l2))





(*s Compare two given lists*)

let cmp l1 l2 =
  match inter l1 l2 with
    | [] ->
	Disjoint
    | l ->
	(match eq l1 l, eq l2 l with(*s Both lists are compared to the same list [l]*)
	   | true, true -> Same
	   | true, _ -> Sub
	   | _, true -> Super
	   | _ -> Overlap)



(*s Determine the complement of a given list*)

let rec compl l=
if l = [] then top else(*s l is compared with the domain of Reals*)
  match cmp l top with
  | Same ->[]
  | Sub ->
        (match l with
             | [] -> top
	     | [rho] -> interval_compl rho
             | rho :: l ->inter (interval_compl rho) (compl l))
  | _ -> assert false(*s [l] cannot be the superset of all Reals*)

(*s Determine union of two lists*)

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
    | Equal,Equal ->         
       single (domain_union dom1 dom2) i1 j1
    | Equal, Less ->
       (match dom1,dom2 with
	  | Real,Real ->single (domain_union dom1 dom2) i2 j2
	  | Real,_ ->single dom1 i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | Int,NonintReal -> single (domain_union dom1 dom2) i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | Int,_ ->single (domain_union dom1 dom2) i2 j2
	  | NonintReal,Int ->single (domain_union dom1 dom2) i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | NonintReal,_ ->single (domain_union dom1 dom2) i2 j2)
    | Equal,Greater ->
       (match dom1,dom2 with
	  | Real,_ ->single (domain_union dom1 dom2) i1 j1
	  | Int,(NonintReal|Real) -> single (domain_union dom1 dom2) i2 j2 @ single dom1 (high_to_low j2) (j1)
	  | Int,Int ->single (domain_union dom1 dom2) i1 j1
	  | NonintReal,(Int|Real) ->single (domain_union dom1 dom2) i2 j2 @ single dom1 (high_to_low j2) (j1)
	  | NonintReal,NonintReal ->single (domain_union dom1 dom2) i1 j1)
    | Greater,Equal ->
        (match dom1,dom2 with
	  | Real,Real ->single (domain_union dom1 dom2) i2 j2
	  | Real,_ ->single dom2 (i2) (low_to_high i1) @ single dom1 i1 j1
	  | Int,(NonintReal|Real) -> single dom2 (i2) (low_to_high i1) @ single (domain_union dom1 dom2) i1 j1
	  | Int,Int ->single (domain_union dom1 dom2) i2 j2
	  | NonintReal,(Int|Real) ->single dom2 (i2) (low_to_high i1) @ single (domain_union dom1 dom2) i1 j1
	  | NonintReal,NonintReal ->single (domain_union dom1 dom2) i2 j2)                 
    | Greater,Less ->
        (match dom1,dom2 with
	  | Real,Real ->single (domain_union dom1 dom2) i2 j2
	  | Real,_ ->single dom2 (i2) (low_to_high i1) @ single (domain_union dom1 dom2) i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | Int,(NonintReal|Real) -> single dom2 (i2) (low_to_high i1) @ single (domain_union dom1 dom2) i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | Int,Int ->single (domain_union dom1 dom2) i2 j2
	  | NonintReal,(Int|Real) ->single dom2 (i2) (low_to_high i1) @ single (domain_union dom1 dom2) i1 j1 @ single dom2 (high_to_low j1) (j2)
	  | NonintReal,NonintReal ->single (domain_union dom1 dom2) i2 j2)
    | Less, Less ->                              
	(match low_high_cmp i2 j1 with
	   | Greater ->  
              (match i2,j1 with
		   | Low(k3,i2),High(k2,j1) ->if k3 = Strict && k2 = Strict && Q.sub i2 j1 = Q.one && dom1=Int && dom2=Int then single(domain_union dom1 dom2)i1 j2 else [rho1;rho2]      
	           | _ ->[rho1; rho2])
	   | Equal ->                            
	       if dom1 = dom2 then
		 single dom1 i1 j2
	       else
		 [rho1; rho2]
	   | Less ->                              
	       if dom1 = dom2 then
		 single dom1 i1 j2
	       else if dom2 = Real then single dom1 i1 (low_to_high i2) @ single dom2 i2 j2
               else if dom1 = Real then single dom1 i1 j1 @ single dom2 (high_to_low j1) (j2)
               else
		 single dom1 i1 (low_to_high i2) @
		 single (domain_union dom1 dom2) i2 j1 @
		 single dom2 (high_to_low j1) j2)
    | Less,Equal ->
        (match dom1,dom2 with
	  | Real,_ ->single (domain_union dom1 dom2) i1 j1 
	  | Int,(NonintReal|Real) -> single dom1 (i1) (low_to_high i2) @ single (domain_union dom1 dom2) i2 j2
	  | Int,Int ->single (domain_union dom1 dom2) i1 j1
	  | NonintReal,(Int|Real) ->single dom1 (i1) (low_to_high i2) @ single (domain_union dom1 dom2) i2 j2
	  | NonintReal,NonintReal ->single (domain_union dom1 dom2) i1 j1)
    | Less,Greater ->
        (match dom1,dom2 with
	  | Real,_ ->single (domain_union dom1 dom2) i1 j1
	  | Int,(NonintReal|Real) -> single dom1 (i1) (low_to_high i2) @ single (domain_union dom1 dom2) i2 j2 @ single dom1 (high_to_low j2) (j1)
	  | Int,Int ->single (domain_union dom1 dom2) i1 j1
	  | NonintReal,(Int|Real) ->single dom1 (i1) (low_to_high i2) @ single (domain_union dom1 dom2) i2 j2 @ single dom1 (high_to_low j2) (j1)
	  | NonintReal,NonintReal ->single (domain_union dom1 dom2) i1 j1)  
    | Greater, Greater ->                        
	(match low_high_cmp i1 j2 with
	   | Greater ->                         
	       (match i1,j2 with
		   | Low(k1,i1),High(k4,j2) ->if k1 = Strict && k4 = Strict && Q.sub i1 j2 = Q.one && dom1=Int && dom2=Int then single(domain_union dom1 dom2)i2 j1 else [rho2;rho1]      
	           | _ ->[rho2; rho1])
	   | Equal ->                         
	       if dom1 = dom2 then
		 single dom1 i2 j1
	       else
		 [rho2; rho1]
	   | Less ->
	       if dom1 = dom2 then
		 single dom1 i2 j1
	       else if dom2 = Real then single dom2 i2 j2 @ single dom1 (high_to_low j2) (j1)
	       else if dom1 = Real then single dom2 i2 (low_to_high i1) @ single dom1 i1 j1
               else 
                 single dom2 i2 (low_to_high i1) @
		 single (domain_union dom1 dom2) i1 j2 @
		 single dom1 (high_to_low j2) j1)




let ite l1 l2 l3 =
   union (inter (compl l1) l2) (inter l1 l3)

	  
type rel = Binrel.t


(*s Checks wether two given lists are disjoint*)

let is_disjoint l1 l2 =
  inter l1 l2 = []

let kind1 i1 i2 = 
 match i1, i2 with
  | Low(Strict,z1),Low(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Strict,z1),Low(Nonstrict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Nonstrict,z1),Low(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Strict,_),Low(Strict,_) -> Strict
  | Low(_,_),Low(_,_) -> Nonstrict
  | _ ->assert false

let kind2 i1 j2 =
 match i1,j2 with
  | Low(Strict,z1),High(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Strict,z1),High(Nonstrict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Nonstrict,z1),High(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | Low(Strict,_),High(Strict,_) -> Strict
  | Low(_,_),High(_,_) -> Nonstrict
  | _ ->assert false

let kind3 j1 i2 =
 match j1,i2 with
  | High(Strict,z1),Low(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Strict,z1),Low(Nonstrict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Nonstrict,z1),Low(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Strict,_),Low(Strict,_) -> Strict
  | High(_,_),Low(_,_) -> Nonstrict
  | _ ->assert false

let kind4 j1 j2 =
 match j1,j2 with
  | High(Strict,z1),High(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Strict,z1),High(Nonstrict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Nonstrict,z1),High(Strict,z2)
     when Q.is_zero(z1) && Q.is_zero(z2) -> Strict
  | High(Strict,_),High(Strict,_) -> Strict
  | High(_,_),High(_,_) -> Nonstrict
  | _ -> assert false


let mult_bound (k1,q1) (k2,q2) =
match k1,k2,q1,q2 with
  | Strict,Strict,z1,z2
     when Q.is_zero(z1) && Q.is_zero(z2) -> (Strict,Q.zero)
  | Strict,Nonstrict,z1,z2
     when Q.is_zero(z1) && Q.is_zero(z2) -> (Strict,Q.zero)
  | Nonstrict,Strict,z1,z2
     when Q.is_zero(z1) && Q.is_zero(z2) -> (Strict,Q.zero)
  | Nonstrict,Nonstrict,z1,z2
     when Q.is_zero(z1) && Q.is_zero(z2) -> (Nonstrict,Q.zero)
  | Strict,Strict,_,_ -> (Strict,Q.mult q1 q2)
  | _,_,_,_ -> (Nonstrict,Q.mult q1 q2)


let gtv ((k1,q1) as v1) ((k2,q2) as v2)=
Q.gt q1 q2 || (Q.equal q1 q2 && k1 = Strict && k2 = Nonstrict)

let min v1 v2=
if gtv v1 v2 then v2 else v1

let max v1 v2=
if gtv v1 v2 then v1 else v2

(*s Abstract interpretation of addition and multiplication*)
let mult1 (dom1,ii1,jj1) (dom2,ii2,jj2)=
let dom = domain_union dom1 dom2 in
match ii1,jj1,ii2,jj2 with
      | Neginf,Posinf,_,_ -> interval dom (Neginf) (Posinf)
      | _,_,Neginf,Posinf -> interval dom (Neginf) (Posinf)
      | Low(k1,i1),Posinf,Low(k3,i2),Posinf -> 
        if Q.ge i1 Q.zero && Q.ge i2 Q.zero 
        then interval dom (Low(kind1 ii1 ii2,Q.mult i1 i2)) Posinf 
        else interval dom (Neginf) (Posinf)
      | Low(k1,i1),Posinf,Neginf,High(k4,j2) -> 
       if Q.ge i1 Q.zero && Q.le j2 Q.zero 
       then interval dom (Neginf) (High(kind2 ii1 jj2,Q.mult i1 j2)) 
       else interval dom (Neginf) (Posinf)
      | Low(k1,i1),Posinf,Low(k3,i2),High(k4,j2) ->
       if Q.ge i2 Q.zero && Q.le i1 Q.zero 
       then interval dom (Low(kind2 ii1 jj2,Q.mult i1 j2)) (Posinf) 
         else if Q.ge i1 Q.zero && Q.ge i2 Q.zero 
         then interval dom (Low(kind1 ii1 ii2,Q.mult i1 i2)) (Posinf) 
           else if Q.le i1 Q.zero && Q.le j2 Q.zero 
           then interval dom (Neginf) (High(kind1 ii1 ii2,Q.mult i1 i2)) 
             else if Q.ge i1 Q.zero && Q.le j2 Q.zero 
             then interval dom (Neginf) (High(kind2 ii1 jj2,Q.mult i1 j2)) 
             else interval dom Neginf Posinf
      | Low(k1,i1),High(k2,j1),Neginf,High(k4,j2) ->
        if Q.le j1 Q.zero && Q.ge j2 Q.zero 
        then interval dom (Low(kind2 ii1 jj2,Q.mult i1 j2)) (Posinf) 
          else if Q.le j1 Q.zero && Q.le j2 Q.zero 
          then interval dom (Low(kind4 jj1 jj2,Q.mult j1 j2)) (Posinf) 
            else if Q.ge i1 Q.zero && Q.ge j2 Q.zero 
            then interval dom (Neginf) (High(kind4 jj1 jj2,Q.mult j1 j2)) 
            else if Q.ge i1 Q.zero && Q.le j2 Q.zero
            then interval dom (Neginf) (High(kind2 ii1 jj2,Q.mult i1 j2))
            else interval dom (Neginf) (Posinf)
      | Low(k1,i1),High(k2,j1),Low(k3,i2),Posinf->
        if Q.le j1 Q.zero && Q.le i2 Q.zero 
        then interval dom (Neginf) (High(kind1 ii1 ii2,Q.mult i1 i2)) 
          else if Q.le j1 Q.zero && Q.ge i2 Q.zero 
          then interval dom (Neginf) (High(kind3 jj1 ii2,Q.mult j1 i2)) 
            else if Q.ge i1 Q.zero && Q.ge i2 Q.zero 
            then interval dom (Low(kind1 ii1 ii2,Q.mult i1 i2)) (Posinf) 
              else if Q.ge i1 Q.zero && Q.le i2 Q.zero 
              then interval dom (Low(kind1 ii1 ii2,Q.mult i1 i2)) (Posinf) 
              else interval dom (Neginf) (Posinf)
      | Neginf,High(k2,j1),Low(k3,i2),Posinf-> 
        if Q.le j1 Q.zero && Q.ge i2 Q.zero 
        then interval dom (Neginf) (High(kind3 jj1 ii2,Q.mult j1 i2)) 
        else interval dom (Neginf) (Posinf)
      | Neginf,High(k2,j1),Low(k3,i2),High(k4,j2)->
        if Q.ge j1 Q.zero && Q.le j2 Q.zero 
        then interval dom (Low(kind3 jj1 ii2,Q.mult j1 i2)) (Posinf) 
          else if Q.le j1 Q.zero && Q.le j2 Q.zero 
          then interval dom (Low(kind4 jj1 jj2,Q.mult j1 j2))(Posinf) 
            else if Q.ge j1 Q.zero && Q.ge i2 Q.zero 
            then interval dom (Neginf) (High(kind4 jj1 jj2,Q.mult j1 j2)) 
              else if Q.le j1 Q.zero && Q.ge i2 Q.zero 
              then interval dom (Neginf) (High(kind3 jj1 ii2,Q.mult j1 i2)) 
              else interval dom (Neginf) (Posinf)
      | Neginf,High(k2,j1),Neginf,High(k4,j2)-> 
        if Q.le j1 Q.zero && Q.le j2 Q.zero 
        then interval dom (Low(kind4 jj1 jj2,Q.mult j1 j2)) (Posinf) 
        else interval dom (Neginf) (Posinf) 
   
      | Low(k1l,i1),High(k1h,j1),Low(k2l,i2),High(k2h,j2) ->
          let i1i2 = mult_bound (k1l,i1) (k2l,i2) in
          let i1j2 = mult_bound (k1l,i1) (k2h,j2) in
          let i2j1 = mult_bound (k2l,i2) (k1h,j1) in
          let j1j2 = mult_bound (k1h,j1) (k2h,j2) in
	  let (k,i)=min i1i2 (min i1j2 (min i2j1 j1j2)) in
	  let (l,j)=max i1i2 (max i1j2 (max i2j1 j1j2)) in
	  interval dom (Low(k,i)) (High(l,j))




(*s Multiplies two lists together*)
		 
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




(*s Multiplies a list with an integer*)

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

    


(*s Adds two domains together*)

let add1 (dom1,i1,j1) (dom2,i2,j2) =
  let dom = domain_union dom1 dom2 in
  let i = match i1,i2 with
    | Neginf, _ ->
	Neginf
    | _, Neginf ->
	Neginf
    | Low(Nonstrict,q1),Low(Nonstrict,q2) ->
	Low(Nonstrict, Q.add q1 q2)
    | Low(Nonstrict,q1),Low(Strict,q2) ->
	Low(Nonstrict, Q.add q1 q2)
    | Low(Strict,q1),Low(Nonstrict,q2) ->
	Low(Nonstrict,Q.add q1 q2)
    | Low(Strict,q1),Low(Strict,q2) ->
	Low(Strict,Q.add q1 q2)
 in
  let j = match j1,j2 with
    | Posinf, _ ->
	Posinf
    | _, Posinf ->
	Posinf
    | High(Nonstrict,q1),High(Nonstrict,q2) ->
	High(Nonstrict, Q.add q1 q2)
    | High(Nonstrict,q1),High(Strict,q2) ->
	High(Nonstrict, Q.add q1 q2)
    | High(Strict,q1),High(Nonstrict,q2) ->
	High(Nonstrict,Q.add q1 q2)
    | High(Strict,q1),High(Strict,q2) ->
	High(Strict,Q.add q1 q2)

  in
  interval dom i j
 


(*s Adds two lists together*)

let rec add l1 l2 =
  List.fold_right
    (fun rho1 acc1 ->
       (List.fold_right
	  (fun rho2 acc2 ->
	     let rho = add1 rho1 rho2 in
	     if is_bot_interval rho then acc2 else union [rho] acc2)
	  l2 acc1))
    l1 []



(*s Interpretations of a list*)

let to_list l = l


let rec of_list = function
  | [] -> bot
  | i :: l -> union [i] (of_list l)
