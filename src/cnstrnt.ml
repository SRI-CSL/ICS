
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
(*i*)

module type Sort = sig
  type t
  val eq: t -> t -> bool
  val enum: t list
end

(*i*)
module type C = sig

  type sort
  type t
  val eq : t -> t -> bool
  val bot : t
  val top : t  
  type cnstrnt =
    | Nonarith of sort
    | Arith of Interval.domain * Interval.low * Interval.high
  val to_list : t -> cnstrnt list
  val of_list : cnstrnt list -> t
  val singleton: Mpa.Q.t -> t
  val zero : t
  val one : t
  val is_singleton : t -> bool
  val value_of : t -> Mpa.Q.t  
  val oo : Interval.domain -> Mpa.Q.t ->  Mpa.Q.t -> t
  val oc : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
  val cc : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
  val co : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
  val nonreal : t
  val sort : sort -> t   
  val real : t
  val int : t                          
  val nonintreal : t
  val diseq : Mpa.Q.t -> t       
  val lt : Interval.domain -> Mpa.Q.t -> t
  val le : Interval.domain -> Mpa.Q.t -> t
  val gt : Interval.domain -> Mpa.Q.t -> t
  val ge : Interval.domain -> Mpa.Q.t -> t  
  val is_bot : t -> bool
  val is_top : t -> bool
  val is_sub : t -> bool
  val inter : t -> t -> t
  val union : t -> t -> t
  val compl : t -> t
  val ite : t -> t -> t -> t
  val is_real : t -> bool
  val is_int : t -> bool
  val is_nonintreal : t -> bool
  val is_nonreal : t -> bool
  val is_sort : sort -> t -> bool  
  val cmp: t -> t -> Binrel.t
  val is_disjoint : t -> t -> bool
  val mult : t -> t -> t
  val add : t -> t -> t
  val mem : ('a -> sort option) -> ('a -> Mpa.Q.t option) -> 'a -> t -> bool
end
(*i*)

  
module Make(S: Sort) = struct

  type sort = S.t

  module Sorts =
    Set.Make(
      struct
	type t = sort
	let compare = compare
      end)

  let nonreals =
    ref (List.fold_right Sorts.add S.enum Sorts.empty)

  let nonreals_is_full nr =
    Sorts.equal nr !nonreals
 
		     
  (*s Constraints. Entities are partitioned into nonreals and reals. A nonreal term
    is either a boolean, a set of predicates, a tuple term (cartesian), a bitvector,
    or another nonreal term (other). Now, a constraint is either the unconstraining
    [Top], the inconsistent constraint [Bot], or it is of the form [Sub(s,i)] where
    [s] is the set of possible nonreal constraints, and [i] is a specification of
    a subset of reals. *)

  
  type t =
    | Top
    | Sub of Sorts.t * Interval.t
    | Bot

  let eq c1 c2 =
    match c1, c2 with
      | Sub(s1,i1), Sub(s2,i2) ->
	  Sorts.equal s1 s2 && Interval.eq i1 i2
      | Top, Top -> true
      | Bot, Bot -> true
      | _ -> false
	
(*s Constructors. *)

  let bot = Bot
  let top  = Top

		
(*s Recognizers. *)

  let is_bot = function
    | Bot -> true
    | _ -> false
	
  let is_top = function
    | Top -> true
    | _ -> false

  let is_sub = function
    | Sub _ -> true
    | _ -> false  

	  
(*s Converting between list of single constraints and internal
  representation of constraints. *)

  type cnstrnt =
    | Nonarith of sort
    | Arith of Interval.domain * Interval.low * Interval.high

  let rec to_list = function
    | Bot -> []
    | Sub(s,i) -> nonreals_to_list s @ reals_to_list i
    | Top -> assert false

  and nonreals_to_list s =
    Sorts.fold (fun x acc -> Nonarith x :: acc) s []

  and reals_to_list i =
    let l = Interval.to_list i in
    List.fold_right (fun (d,l,h) acc -> Arith(d,l,h) :: acc) l []

  let of_list l =
    let add1 c1 (nonreals, reals) =
      match c1 with
	| Nonarith(x) ->
	    (Sorts.add x nonreals, reals)
	| Arith(d,l,h) ->
	    (nonreals, Interval.union (Interval.inj (d,l,h)) reals)
    in
    let (nonreals,reals) = 
      List.fold_right add1 l (Sorts.empty, Interval.empty)
    in
    if Sorts.is_empty nonreals && Interval.is_empty reals then
      Bot
    else if nonreals_is_full nonreals && Interval.is_full reals then
      Top
    else
      Sub(nonreals,reals)

	
(*s Derived constructors. *)
	   
  let zero = Sub(Sorts.empty, Interval.singleton Q.zero)
  let one = Sub(Sorts.empty, Interval.singleton Q.one)
  let singleton q = Sub(Sorts.empty,Interval.singleton q)
		    
  let oo dom p q =
    let i = Interval.oo dom p q in
    if Interval.is_empty i then bot else Sub(Sorts.empty,i)
    
  let oc dom p q =
    let i = Interval.oc dom p q in
    if Interval.is_empty i then bot else Sub(Sorts.empty,i)

  let co dom p q =
    let i = Interval.co dom p q in
    if Interval.is_empty i then bot else Sub(Sorts.empty,i)
    
  let cc dom p q =
    let i = Interval.cc dom p q in
    if Interval.is_empty i then bot else Sub(Sorts.empty,i)
		   
  let int = Sub(Sorts.empty ,Interval.int)
  let real = Sub(Sorts.empty, Interval.real)
  let nonintreal = Sub(Sorts.empty, Interval.nonint)
		     
  let diseq q = Sub(Sorts.empty,Interval.diseq q)    

  let lt dom q = Sub(Sorts.empty,Interval.lt dom q)
  let le dom q = Sub(Sorts.empty,Interval.le dom q)
  let gt dom q = Sub(Sorts.empty,Interval.gt dom q)
  let ge dom q = Sub(Sorts.empty,Interval.ge dom q)

  let nonreal = Sub(!nonreals, Interval.empty)
  let sort srt = Sub(Sorts.add srt Sorts.empty, Interval.empty)
		       
    
(*s More Recognizers. *)
	
  let is_real = function
    | Sub(s,_) when Sorts.is_empty s -> true
    | _ -> false
	
  let is_int = function
    | Sub(s,i) when Sorts.is_empty s && Interval.is_int i -> true
    | _ -> false
	
  let is_nonintreal = function
    | Sub(s,i) when Sorts.is_empty s && Interval.is_nonintreal i -> true
    | _ -> false
    
  let is_nonreal = function
    | Sub(s,_) -> not(Sorts.is_empty s)
    | _ -> false

  let is_sort srt = function
    | Sub(s,_) -> Sorts.mem srt s
    | _ -> false


  (*s Value of a singleton. *)
	
  let is_singleton = function
    | Sub(s,i) when Sorts.is_empty s && Interval.is_singleton i -> true
    | _ -> false
	
  let value_of c =
    assert(is_singleton c);
    match c with
      | Sub(_,i) ->Interval.value_of i
      | _ -> assert false

	  
  (*s Intersection, union, and complement of constraints. *)

  let inter c1 c2 =
    match c1,c2 with
      | Top, _ -> c2
      | _, Top -> c1
      | Bot, _ -> Bot
      | _, Bot -> Bot
      | Sub(s1,i1), Sub(s2,i2) ->
	  let s = Sorts.inter s1 s2 in
	  let i = Interval.inter i1 i2 in
	  if Sorts.is_empty s && Interval.is_empty i then
	    Bot
	  else
	    Sub(s,i)
	
  let union c1 c2 =
    match c1,c2 with
      | Top, _ -> Top
      | _, Top -> Top
      | Bot, _ -> c2
      | _, Bot -> c1
      | Sub(s1,i1), Sub(s2,i2) ->
	  let s = Sorts.union s1 s2 in
	  let i = Interval.union i1 i2 in
	  if Interval.is_full i && (nonreals_is_full s) then
	    Top
	  else
	    Sub(s,i)

  let compl = function
    | Bot -> Top
    | Top -> Bot
    | Sub(s,i) ->
	let s' = s (* Sorts.diff !nonreals s *) in
	let i' = Interval.compl i in
	Sub(s',i')

  let ite c1 c2 c3 =
    union (inter c1 c2) (inter (compl c1) c3)

    
    (*s Comparison of constraints. *)

  let rec cmp c1 c2 =
    match c1,c2 with
      | Top, Top ->
	  Binrel.Same
      | Bot, Bot ->
	  Binrel.Same
      | Bot, _ ->
	  Binrel.Disjoint
      | _, Bot ->
	  Binrel.Disjoint
      | Sub(s1,i1), Sub(s2,i2) when Sorts.equal s1 s2 ->
	  Interval.cmp i1 i2
      | Sub(s1,i1), Sub(s2,i2) ->
	  Binrel.union (nonreals_cmp s1 s2) (Interval.cmp i1 i2)         (* ??? *)
      | Sub _, Top ->
	  Binrel.Sub
      | Top, Sub _ ->
	  Binrel.Super

  and nonreals_cmp s1 s2 =
    if Sorts.equal s1 s2 then
      Binrel.Same
    else 
      let s = Sorts.inter s1 s2 in
      if Sorts.is_empty s then
	Binrel.Disjoint
      else if Sorts.equal s s1 then
	Binrel.Sub
      else if Sorts.equal s s2 then
	Binrel.Super
      else
	Binrel.Overlap

  let is_disjoint c1 c2 =
    cmp c1 c2 = Binrel.Disjoint

	  (*s Membership. *)

  let mem sort_of num_of x c =
    match c with
      | Top -> true
      | Bot -> false
      | Sub(s,i) ->
	  (match num_of x with
	     | Some(q) ->Interval.mem q i
	     | None ->
		 (match sort_of x with
		    | Some(srt) -> Sorts.mem srt s
		    | None -> false))

	
	  (*s Abstract interpretation of addition and multiplication. *)

  let mult c1 c2 =
    match c1, c2 with
      | Top, _ -> Top
      | _, Top -> Top
      | Sub(s1,i1), Sub(s2,i2) ->
	  let i = Interval.mult i1 i2 in
	  if Interval.is_empty i then Bot
	  else Sub(Sorts.empty, i)
      | _ -> Bot
    
  let add c1 c2 =
    match c1, c2 with
      | Top, _ -> Top
      | _, Top -> Top
      | Sub(s1,i1), Sub(s2,i2) ->
	  let i = Interval.add i1 i2 in
	  if Interval.is_empty i then Bot
	  else Sub(Sorts.empty, i)
      | _ -> Bot

end 
  





