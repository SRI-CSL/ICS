
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

(*s The type [t] specifies the 'type' of intervals which
    can be constructed using Interval.ml module. In this
    file it is used to denote the arguments and return types
    of the various functions implemented in Interval.ml
    i.e it provides an abstraction for the various 
    real data types used in ICS. *)

type t

(*s An Empty interval is denoted by bot.It is equivalent to 
    the representation []. *)

val empty : t

(*s The set of all Reals is denoted by top. It is equivalent
    to the representation [Real,Neginf,Posinf], where  Neginf
    and Posinf represent negative and positive infinity. *)

val full : t

(* Injects an element into a list*)

val inj : Dom.t -> Interval.t -> t



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

val diseq : Dom.t -> Q.t -> t


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
val is_singleton : t -> bool

(*s Checks wether given set is singleton or not, if not it throws
    the [Invalid_argument] exception. *)

val d_singleton : t -> Q.t

(*s [mem q s] tests if [q] is a member of the set denoted by [s].
    Returns a boolean value depending upon the result.  *)
    
val mem : Q.t -> t -> bool

(*s [eq s1 s2] tests if [s1] and [s2] have the same denotations.
    i.e their upper and lower limits along with the domains
    over which they are interpreted should be equal. *)
    
val eq : t -> t -> bool

val sub : t -> t -> bool

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

(*s Analyze a constraint. *)

val analyze : t ->  Mpa.Q.t Status.t
  
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

val inter : Dom.t -> t -> t -> t
  
(*s [union s1 s2] yields the union of the interval(list).
    It evaluates the union of the domains of the intervals
    as well as the union of the individual elements of the
    intervals. *)

val union : Dom.t -> t -> t -> t

(*s [compl s] computes the complement with respect to the reals. *)

val compl : Dom.t -> t -> t

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

val add : Dom.t -> t -> t -> t
val mult : Dom.t -> t -> t -> t
val expt : Dom.t -> int -> t -> t
val multq : Dom.t -> Q.t -> t -> t
val div : Dom.t -> t -> t -> t

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

  
val to_list : t -> Interval.t list


val of_list : Dom.t -> Interval.t list -> t

(*s Additional constructors. *)

val oo : Dom.t -> Q.t -> Q.t -> t
val oo : Dom.t -> Q.t -> Q.t -> t
val oo : Dom.t -> Q.t -> Q.t -> t
val oo : Dom.t -> Q.t -> Q.t -> t

val lt : Dom.t -> Q.t -> t
val le : Dom.t -> Q.t -> t
val gt : Dom.t -> Q.t -> t
val ge : Dom.t -> Q.t -> t

val pos : Dom.t -> t
val neg : Dom.t -> t
val nonneg : Dom.t -> t
val nonpos : Dom.t -> t
