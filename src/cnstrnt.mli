
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

(*s Module [Cnstrnt] includes constructors for building and manipulating
  constraint expressions of type [cnstrnt] as defined in module [Term]. *)

(*s The input signature of the functor [Cnstrnt.Make]. [t] is
  considered to represent a finite set of sorts with disjoint
  extensions. [eq] tests for equality of sorts, and [ext] is the
  finite enumeration of all sorts. *)

module type Sort = sig
  type t
  val eq: t -> t -> bool
  val enum: t list
end


module type C = sig

  type sort

  (*s A constraint is either a real or a nonreal constraint. A nonreal term
    is either boolean, a set of predicate, a tuple term (cartesian), a bitvector,
    or another nonreal term (other). Now, a constraint is either the unconstraining
    [Top], the inconsistent constraint [Bot], or it is of the form [Sub(s,i)] where
    [s] is the set of possible nonreal constraints, and [i] is a specification of
    a subset of reals. *)

  type t

  val eq : t -> t -> bool

    (*s The empty constraint and the fully unconstrained constraint. *)
    
  val bot : t
  val top : t

    
    (*s Listify a constraint which satisfies the [is_sub] recognizers.
      The result is a list of possible [nonreal] constraints together
      with a [real] constraint. If the real constraint is [None], then
      no real number is in the extension of this constraint. Otherwise,
      the extension is described in terms of a list of disjoint intervals. *)

  type cnstrnt =
    | Nonarith of sort
    | Arith of Interval.domain * Interval.low * Interval.high

  val to_list : t -> cnstrnt list
  val of_list : cnstrnt list -> t

      
      (*s The only member of the [singleton q] constraint is [q] itself.
	[zero] and [one] are just abbreviations for [singleton 0] and [singleton 1],
	respectively. *)
  
  val singleton: Mpa.Q.t -> t
  val zero : t
  val one : t

    
    (*s [is_singleton c] tests whether or not the denotation of the constraint [c] is
      a singleton set with a rational as its sole member. In these cases, the accessor
      [value_of c] yields this rational constant. *)
  
  val is_singleton : t -> bool
  val value_of : t -> Mpa.Q.t

      
      (*s [oo d q1 q2] constructs an open interval constraint with all the reals
	in between [q1] and [q2] which satisfy the domain condition. Likewise,
	[oc], [co], and [cc] are used to respectively construct left-open, right-open,
	and closed intervals. *)
    
  val oo : Interval.domain -> Mpa.Q.t ->  Mpa.Q.t -> t
  val oc : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
  val cc : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
  val co : Interval.domain ->  Mpa.Q.t ->  Mpa.Q.t -> t

      (*s The extension of the [int], [rea], [nonintreal constraints are
	the integers, the reals, and all non-integer reals, respectively. *)

 
  val nonreal : t
  val sort : sort -> t
    
  val real : t
  val int : t                          
  val nonintreal : t


    (*s The extension of [diseq q] are all reals without [q]. *)
  
  val diseq : Mpa.Q.t -> t       

      
    (*s The extension of [lt d q] is the set of all reals in the domain [d]
      which are less than [q]. Likewise, [le], [gt], and [ge] construct
      'less or equal', 'greater than', and 'greater or equal' constraints. *)
    
  val lt : Interval.domain -> Mpa.Q.t -> t
  val le : Interval.domain -> Mpa.Q.t -> t
  val gt : Interval.domain -> Mpa.Q.t -> t
  val ge : Interval.domain -> Mpa.Q.t -> t

      
    (*s [is_bot c] holds iff no term is in the extension of [c], [is_top c]
      holds iff every term is in the extension of [c], and [is_sub c] holds in
      all other cases. *)
    
  val is_bot : t -> bool
  val is_top : t -> bool
  val is_sub : t -> bool

      
    (* Intersection, union, and complement of constraints. [ite c1 c2 c3] is
       just shorthand for [union (inter c1 c2) (inter (compl c1) c3)]. *)
    
  val inter : t -> t -> t
  val union : t -> t -> t
  val compl : t -> t
  val ite : t -> t -> t -> t

      
    (*s [is_real c]  holds iff the kind part of [c] is [Int] or Real] or if
      the sign part of is a genuine constraint (i.e. other than [Top] and [Bot]).
      The other recognizers are defined similarly. *)
    
  val is_real : t -> bool
  val is_int : t -> bool
  val is_nonintreal : t -> bool

  val is_nonreal : t -> bool
  val is_sort : sort -> t -> bool

  
   (*s [cmp c1 c2] returns [Same] if the extension of [c1] and c2] are
     identical, [Sub] if the extension of [c1] is strictly included in the
     extension of [c2], [Super] if the extension of [c2] is a strict superset
     of the the extension of [c1], [Disjoint] if the extensions of [c1] and [c2]
     are disjoint, and [Overlap] otherwise. *)
    
  val cmp: t -> t -> Binrel.t

  val is_disjoint : t -> t -> bool

      (*s Abstract interpretation of addition and multiplication. *)

  val mult : t -> t -> t
  val add : t -> t -> t

      (*s [mem sort_of num_of x c] tests if [x] is in the extension of
	the constraint [c]. The function [sort_of] determines a non-arithmetic
	sort for [x] and, if [x] represents a rational number,  [num_of x] returns
	the value of [x]. *)

  val mem : ('a -> sort option) -> ('a -> Mpa.Q.t option) -> 'a -> t -> bool

end

			  
module Make(S: Sort): (C with type sort = S.t)
 
	      






