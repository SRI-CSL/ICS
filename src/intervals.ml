
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
 * Author: Harald Ruess
 i*)

(*i*)
open Mpa         
open Binrel
open Tools
open Allen
(*i*)


(*s Disjunction of Intervals with these invariant properties:
   intervals are disjoint, ordered from left-to-right, and no
   interval is empty when interpreted over the domain of Reals. *)

type t = Interval.t list


(*s Constructing Intervals. *)
			   
let empty = []               (*s The empty interval. *)
let full  = [Interval.full]  (*s The real number line. *)

let inj dom i =
  if Interval.is_empty i then 
    empty 
  else 
    let x,y = Interval.destructure i in
    let j = Interval.make dom x y in
    if Interval.is_empty j then
      empty
    else 
      [j]


(*s Construct a singleton set [q]*)

let singleton u =
  [Interval.singleton u]
 
(*s Construct the set of Reals without [q]*)

let diseq dom u =
  let a = Extq.of_q u in
  [Interval.make dom (Extq.neginf,false) (a,false); 
   Interval.make dom (a,false) (Extq.posinf,false)]
	     

(*s Membership, equality, and containment of lists of intervals*)
(*s Checks wether [q] is a member of list [l]*)                                                          

let mem q l = 
  List.exists (Interval.mem q) l


(*s Checks wether two lists are equal *)

let eq l m =
  try 
    List.for_all2 Interval.eq l m
  with 
      Invalid_argument _ -> false


(*s [sub l m] iff for all intervals [i] in [m] there exists
    a [j] such that [i] is contained in [j]. *)

let rec sub l m =
  List.for_all (fun i -> sub1 i m) l

and sub1 i m =
  assert(not(Interval.eq i Interval.empty));
  match m with
    | [] -> 
	false
    | j :: m' ->
	(match Interval.cmp i j with
	  | Equals | Sub | Starts | Finishes ->
	      true
	  | Before | Meets -> 
	      sub1 i m'
	  | After | MetBy | Overlaps | OverlappedBy 
	  | Super | StartedBy | FinishedBy -> 
	      false)

(*s Checks wether [l] is an empty list*)

let is_bot l = (l = [])

(*s Checks if given interval denotation is the domain of all Reals*)
		   
let is_top l =
  match l with
    | [i] -> Interval.eq Interval.full i
    | _ -> false

(*s Checks for singleton set [q] *)

let is_singleton l = 
  match l with
    | [i] -> Interval.is_singleton i <> None
    | _ -> false
       
	
(*s Get the value of a singleton set [q]. *)

let d_singleton = function
  | [i] -> (match Interval.is_singleton i with Some(u) -> u | None -> assert false)
  | _ -> raise (Invalid_argument "Not a singleton")


(*s Pretty printing functions*)

let pp fmt l =
  let rec print = function
    | [] -> ()
    | [i] -> Interval.pp fmt i
    | i :: l -> Interval.pp fmt i; Format.fprintf fmt ", "; print l
  in
  Format.fprintf fmt "@[";
  print l;
  Format.fprintf fmt "@]"


(*s Finds the interval of two lists*)

   
let inter dom l m =
  let cons i l = 
    if Interval.eq Interval.empty i then l else i:: l 
  in
  let toggle (a,alpha) = (a, not alpha) in
  let make = Interval.make dom in
  let remake i = make (Interval.lo i) (Interval.hi i) in  (* [i] may be interpreted over new domain. *)
  let rec loop l m =
    match l, m with
      | [], _ 
      | _, [] ->
	  []
      | [i], [j] -> 
	  cons (Interval.inter dom i j) []
      | i :: l', j :: m' ->
	  (match Interval.cmp i j with
	     | Before | Meets -> 
		 loop l' m
	     | After | MetBy ->  
		 loop l m'
	     | Overlaps ->
		 let j1 = make (Interval.lo j) (Interval.hi i)  (* intersection of [i] and [j] *)
		 and j2 = make (toggle (Interval.hi i)) (Interval.hi j) in (* upper section of [j]. *)
		 cons j1 (loop l' (j2 :: m'))
	     | OverlappedBy ->
		 let i1 = make (Interval.lo i) (Interval.hi j) in    (* intersection of [i] and [j] *)
		 let i2 = make  (toggle (Interval.hi j)) (Interval.lo i) in
		 cons i1 (loop (i2 :: l') m')
	     | Sub ->  
		 let k = make (toggle (Interval.hi i)) (Interval.hi j) in (* upper section of [j]. *)
		 cons i (loop l' (k :: m'))
	     | Super ->  
		 let k = make (toggle (Interval.hi j)) (Interval.hi i) in (* upper section of [i]. *)
	       cons (remake j) (loop (k :: l') m')
	     | Starts -> 
		 let k = make (toggle (Interval.hi i)) (Interval.hi j) in (* [j] without [i] *)
		 cons (remake i) (loop l' (k :: m'))
	     | StartedBy ->
		 let k = make (toggle (Interval.hi j)) (Interval.hi i) in (* [i] without [j] *)
		 cons (remake j) (loop (k :: l') m')
	     | FinishedBy ->
		 cons (remake j) (loop l' m')
	     | Equals | Finishes ->
		 cons (remake i) (loop l' m'))
  in
  loop l m

(*s Compare two given lists*)

type rel = Binrel.t

let cmp l m =
  let rec loop l m =
    (match l, m with
      | [], [] -> 
	  Binrel.Same
      | [], _ ->
	  Binrel.Sub
      | _, [] ->
	  Binrel.Super
      | i :: l', j :: m' ->
	  let s1 = match Interval.cmp i j with
	    | Equals -> Same
	    | Sub | Starts | Finishes -> Binrel.Sub
	    | Before | Meets | After | MetBy -> Binrel.Disjoint
	    | Overlaps | OverlappedBy -> Binrel.Overlap
	    | Super | StartedBy | FinishedBy -> Binrel.Super
	  and s2 = loop l' m' 
	  in
	  Binrel.union s1 s2)
  in
  loop l m

(*s Analyze a constraint. *)

let analyze l =
  match l with 
    | [] -> 
	Status.Empty
    | [i] -> 
	if Interval.eq i Interval.full then Status.Full
	else (match Interval.is_singleton i with
	  | None -> Status.Other
	  | Some u -> Status.Singleton u)
    | _ ->
	Status.Other

(*s Determine union of two lists*)

let union dom l m = 
  let make = Interval.make dom in
  let remake i = make (Interval.lo i) (Interval.hi i) in  (* [i] may be interpreted over new domain. *)
  let cons i l =  (* [i] is nonempty and to-the-left of [l] *)
    match l with
      | [] -> 
	  [i]
      | j :: l' when Interval.cmp i j = Meets ->   (* merge adjacent intervals. *)
	  Interval.make dom (Interval.lo i) (Interval.hi j) :: l'
      | _ ->
	  i :: l
  in
  let rec loop l m =
    match l, m with
      | [], _ -> 
	  m
      | _, [] -> 
	  l
      | [i], [j] ->
	  inj dom (Interval.union dom i j)
      | i :: l', j :: m' ->
	  (match Interval.cmp i j with
	     | Before -> 
		 cons (remake i) (loop l' m)
	     | After ->  
		 cons (remake j) (loop l m')
	     | Meets | Overlaps ->
		 cons (make (Interval.lo i) (Interval.hi j)) (loop l' m')
	     | MetBy | OverlappedBy ->
		 cons (make (Interval.lo j) (Interval.hi i)) (loop l' m')
	     | Sub | Starts | Finishes ->  
		 cons j (loop l' m')
	     | Super | StartedBy | FinishedBy | Equals ->  
		 cons (remake i) (loop l' m'))
  in
  loop l m

let insert dom i =
  union dom (inj dom i)            (* to be changed to general domain. *)

(*s Complement. Should be done more efficiently... *)

let rec compl dom l =
  match l with
    | [] -> full
    | [i] -> compl1 dom i
    | i :: l -> inter dom (compl1 dom i) (compl dom l)

and compl1 dom i = 
  match Interval.compl dom i with
    | Interval.Empty -> empty
    | Interval.One(j) -> inj dom j
    | Interval.Two(j1,j2) -> [j1; j2]     (* [j1] and [j2] are ordered. *)
  
	
(*s Checks wether two given lists are disjoint*)

let rec is_disjoint l m =
  match l, m with
    | [], _ -> true
    | _, [] -> true
    | i :: l', j :: m' ->
	(match Interval.cmp i j with
	   | Before | Meets -> 
	       is_disjoint l' m
	   | After | MetBy ->  
	       is_disjoint l m'
	   | _  ->
	       false)

(*s Abstract interpretation of addition and multiplication*)

let of_binary dom f l m =
  List.fold_right
    (fun i -> 
       List.fold_right 
	 (fun j -> insert dom (f i j)) 
	 m)
    l []
 
let add  dom = of_binary dom (Interval.add dom)
let mult dom = of_binary dom (Interval.mult dom)
let div dom = of_binary dom (Interval.div dom)

let expt dom n l =
  let rec loop = function
    | 0 -> singleton Q.one
    | n -> mult dom l (loop (n - 1))
  in
  loop n

(*s Multiplies an interval list with an integer *)

let multq dom  q l =
  List.fold_right (fun i -> insert dom (Interval.multq dom q i)) l []


(*s Interpretations of a list*)

let to_list l = l

let rec of_list dom = function
  | [] -> empty
  | i :: l -> insert dom i (of_list dom l)


(*s Additional constructors. *)

let oo dom u v = inj dom (Interval.make dom (Interval.strict u) (Interval.strict v))
let oc dom u v = inj dom (Interval.make dom (Interval.strict u) (Interval.nonstrict v))
let co dom u v = inj dom (Interval.make dom (Interval.nonstrict u) (Interval.strict v))
let cc dom u v = inj dom (Interval.make dom (Interval.nonstrict u) (Interval.nonstrict v))

let lt dom u = inj dom (Interval.make dom Interval.neginf (Interval.strict u))
let le dom u = inj dom (Interval.make dom Interval.neginf (Interval.nonstrict u))
let gt dom u = inj dom (Interval.make dom (Interval.strict u) Interval.posinf)
let ge dom u = inj dom (Interval.make dom (Interval.nonstrict u) Interval.posinf)

let neg dom = lt dom Q.zero
let pos dom = gt dom Q.zero
let nonneg dom = ge dom Q.zero
let nonpos dom = le dom Q.zero
