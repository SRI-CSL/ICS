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

(*s An Interval can be described by specifying the 
    lower and upper bounds along with the domain it
    occupies.The module interval.ml describes ways of
    constructing an interval or a list of intervals
    and then performing set operations namely union,
    intersection,complement,abstact addition and 
    multiplication on the interval(list). 
    It is important to differentiate between an interval 
    and a list of intervals. The latter is a union or 
    intersection or some other combination of several of
    the former.While constructing a list of intervals
    two things have to be considered :-
    1. The individual intervals must be mutually disjoint.
    2. No single interval must be empty. 
    3. They must be ordered left to right. *)

(*i*)
open Mpa
(*i*)

(*s An Interval is specified (although not completely)
    by the domain its members constitute.To construct an
    interval, we have a choice of three domains, namely
    Integers, Reals, NonintReals; the last domain meaning 
    those Reals which are not integers e.g pi. *)

type domain = Int | Real | NonintReal


(*s The type [t] specifies the 'type' of intervals which
    can be constructed using Interval.ml module. In this
    file it is used to denote the arguments and return types
    of the various functions implemented in Interval.ml
    i.e it provides an abstraction for the various 
    real data types used in ICS. *)

type t

(*s An Empty interval is denoted by bot.It is equivalent to 
    the representation []. *)

val bot : t

(*s The set of all Reals is denoted by top. It is equivalent
    to the representation [Real,Neginf,Posinf], where  Neginf
    and Posinf represent negative and positive infinity. *)

val top : t

(*s The following four functions provide a way to construct
    an interval with the boundary limits that we want. i.e
    wether to have open(exclusive) or closed(inclusive) 
    boundary limits depends on which of these four functions 
    is chosen to construct the interval.
    [x dom q1 q2] constructs a single interval where 'x' can
    be oo,oc,co,cc. 
    oo ->constructs a left and right open interval
    oc ->constructs a left open and right closed interval
    co ->constructs a left closed and right open interval
    cc ->constructs a left and right closed interval.
    
   if [q1>=q2] then an empty set '[]' is created, else an
   interval ranging from 'q1' to 'q2' is created which is
   interpreted over the domain 'dom'.*)

val oo : domain -> Q.t -> Q.t -> t
val oc : domain -> Q.t -> Q.t -> t
val co : domain -> Q.t -> Q.t -> t
val cc : domain -> Q.t -> Q.t -> t



(*s Incase we want an interval with either one or both the 
    boundaries as infinty, we cannot use the above denotation
    to construct the interval.Instead the following functions
    can be used.
    [x dom q] constructs a single interval where 'x' can be
    lt,le,gt,ge.
    lt ->constructs an interval from Neginf to a value lesser than 'q'
    le ->constructs an interval from Neginf to 'q'
    gt ->constructs an interval from a value greater than 'q' to Posinf
    ge ->constructs an interval from 'q' to Posinf.
    The interval created is interpreted over the domain 'dom'.
    Neginf and Posinf always have exclusive limits i.e intervals
    having either one or both limits of the form '[-inf' or
    'inf]' cannot be constructed. *)
    
val lt : domain -> Q.t -> t
val le : domain -> Q.t -> t
val gt : domain -> Q.t -> t
val ge : domain -> Q.t -> t


(*s The set of Integers is denoted by int. *)
val int : t

(*s The set of Reals is denoted by real(synonymous with [full]). *)
val real : t

(*s The set of Reals excluding the Integers is denoted by nonintreal. *) 
val nonint : t

(*s An interval of the form domain [q..q] constitutes a singleton
    set with the singleton element [q].It is interpreted over the
    given domain.It is obvious that such a set will always have 
    inclusive upper and lower limits. i.e Real[3..3] is correct
    but Real(3..3) is not.*)

val singleton : Q.t -> t

(*s The set of Reals without [q] is denoted by [diseq q].
    It is in a sense the exact reverse of the concpt of
    singleton set, since what it leaves out constitutes
    a singleton set. *)

val diseq : Q.t -> t

(*s 'set recognizers' imply the functions used to identify a 
     particular property in a given interval or set.The following
     set recognizers have been detailed :-
     
     [is_bot s] holds iff the denotation of [s] is the empty set 
     [is_top s] holds iff the denotation of [s] is the set of real numbers
     [is_int s] holds iff [s] denotes the integers 
     [is_real s] holds iff [s] denotes the reals
     [is_nonintreal s] holds iff [s] denotes the nonintreals
     [is_singleton s] holds iff the denotation of [s] is singleton
     Each of the set recognizers returns a boolean value which is
     either true or false depending on the comparison. *)
    
val is_bot : t -> bool
val is_top : t -> bool
val is_int : t -> bool
val is_real : t -> bool
val is_nonintreal : t -> bool
val is_singleton : t -> bool

(*s Checks wether given set is singleton or not, if not it throws
    the [Invalid_argument] exception. *)
val value_of : t -> Q.t

(*s [mem q s] tests if [q] is a member of the set denoted by [s].
    Returns a boolean value depending upon the result.  *)
    
val mem : Q.t -> t -> bool

(*s [eq s1 s2] tests if [s1] and [s2] have the same denotations.
    i.e their upper and lower limits along with the domains
    over which they are interpreted should be equal. *)
    
val eq : t -> t -> bool

(*s [cmp s1 s2] matches (inter s1 s2) with s1 and s2.
    It yields 'Same' iff (s1==s2)
    'Disjoint' if (s1 != s2) 
    'Sub' if (s1 <= s2)
    'Super' if (s1 >= s2) and 'Overlap' otherwise. 
     Here the mathematical comparisons between the two
     sets imply wether or not the elements in one
     set are contained in the other and even if
     same elements are present wether they are equal
     or not. *)
    
val cmp : t -> t -> Binrel.t
  
(*s Checks wether [s1] [s2] are disjoint or not*)
val is_disjoint : t -> t -> bool

(*s Pretty printing functions. *)
val pp : Format.formatter -> t -> unit

(*s The following functions operate on the given sets to
    yield their unions,intersections etc. *)

(*s [inter s1 s2] yields the intersection of the 
    interval(list). It evaluates the intersection of the
    domains of the intervals as well as the intersection
    of the individual elements of the intervals. *)

val inter : t -> t -> t
  
(*s [union s1 s2] yields the union of the interval(list).
    It evaluates the union of the domains of the intervals
    as well as the union of the individual elements of the
    intervals. *)

val union : t -> t -> t
    
(*s [compl l] yields the complement of the interval(list)
    e.g the complement of top is bot and vice versa. *)

val compl : t -> t

val ite : t -> t -> t -> t
    
(*s Abstract interpretation of arithmetic addition and multiplication in terms
    of intervals is performed by the given functions.
    OPEN ISSUES :-
    1.) Consider the following operation ->
    (mult Int[5..inf) Int[5..inf))
    result :- Int[25..inf)
    Prime numbers like 29,31 etc. are included in the 
    result. However, a little thought shows that such
    a case is impossible. Since we cannot enumerate all
    primes upto infinty, our result cannot be termed as exact. 
    2.) Multiplying the singleton set zero i.e [0..0] with an interval
    having one of the limits as infinity results in an undefined result
    and a wrong output. *)

val add : t -> t -> t
val mult : t -> t -> t
val multq : Q.t -> t -> t

(*s Lower and Upper bounds of possibly infinite intervals.Strict represents\
    inclusive bound and Nonstrict represents exclusive bound. 
    Strict bounds-> '[',']';Nonstrict bounds-> '(',')' *)

type kind = Strict | Nonstrict

(*s Defines the types of lower limits.Neginf represents negative infinity*)
type low =
  | Neginf(*s represented by "(-inf" *)
  | Low of kind * Q.t(*s represented by "(Q.t" or "[Q.t" *)

(*s Defines the types of higher limits.Posinf represents positive infinity*)

type high =
  | Posinf(*s represented by "inf)" *)
  | High of kind * Q.t(*s represented by "Q.t)" or "Q.t]" *)


(*s An interval consists of a lower bound, an upper bound, and the
  domain over which it is interpreted. *)

type interval = domain * low * high

(*s [to_list l] returns a list of intervals. Hereby, an interval is
     represented by a triple [(d,l,h)] where [d] is the domain over which the interval
     is interpreted, [l] is the lower bound, and [h] is the upper bound. A lower bound
     is either negative infinity [Neginf], or [Low(k,q)], where the rational [q] is the
     lower bound, and the kind [k] determines if [q] belongs to the interval or not.
     Similarly, an upper bound is either infinity [Posinf] or [High(k,q)].

     The intervals [of_list l] are disjoint and ordered from left-to-right. Moreover,
     every interval is nonempty. The union of the denotations of these intervals is
     the subset of the reals denoted by top.
     If a list [l] is created in which the constituent intervals are overlapping or
     are empty then erronous results will accrue.i.e a list [l] of the following type 
     is not allowed :- (real [1..10] union real [3..15] union Int [3..2]). *)

  
val to_list : t -> interval list


val of_list : interval list -> t


(* Injects an element into a list*)
val inj : interval -> t

