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
 *)

(*s The module [Interval] realizes a lattice of certain subsets
  of the real numbers described by unions of intervals. *)

(*i*)
open Mpa
(*i*)

   (*s Intervals are interpreted over the domains of the reals,
     the integers, or the reals minus the integers. *)

type domain = Int | Real | NonintReal


    (* The type [t] of the disjoint union of intervals.  Each
       intervals does not only have a lower and an upper bound, but
       is also interpreted over one of the domains above. *)

type t

     (*s The empty set. *)
val empty : t

     (*s The set of all real numbers. *)
val full : t

    (*s [oo d q1 q2] constructs a single open interval [(q1..q2)] interpreted over the domain [d].
        If [q1 >= q2], then [empty] is returned. Similarly, [oc] constructs a left-open
        interval, [co] a right-open interval, and [cc] a closed interval. *)
val oo : domain -> Q.t -> Q.t -> t
val oc : domain -> Q.t -> Q.t -> t
val co : domain -> Q.t -> Q.t -> t
val cc : domain -> Q.t -> Q.t -> t

    (*s [lt d q] constructs the interval [(-inf,q)] interpreted over the domain [d],
        where [-inf] is the symbol for 'minus infinity'. Similarly, for the constructors
        less or equal' [le], 'greater than' [gt], and 'greater or equal' [ge]. *)
val lt : domain -> Q.t -> t
val le : domain -> Q.t -> t
val gt : domain -> Q.t -> t
val ge : domain -> Q.t -> t

    (*s The set of integers. *)
val int : t

    (*s The set of reals (synonymous with [full]). *)
val real : t

    (*s The set of reals without the integers. *) 
val nonint : t

    (*s [singleton q] denotes the set with the singleton element [q]. *)
val singleton : Q.t -> t

    (*s [diseq q] denotes the set of reals without [q]. *)
val diseq : Q.t -> t

    (*s Various set recognizers. [is_empty s] holds iff the denotation of
        [s] is the empty set, [is_full s] holds iff the denotation of [s]
        is the set of real numbers, [is_int s] holds iff [s] denotes the
        integers, and [is_singleton s] holds iff the denotation of [s] is
        singleton. *)
val is_empty : t -> bool
val is_full : t -> bool
val is_int : t -> bool
val is_real : t -> bool
val is_singleton : t -> bool

    (*s Get the value of a singleton constraint. Throws [Invalid_argument] if
        argument set is not a singleton. *)
val value_of : t -> Q.t

    (* [mem q s] tests if [q] is a member of the set denoted by [s]. *)
val mem : Q.t -> t -> bool

    (*s [eq s1 s2] tests if [s1] and [s2] have the same denotations. *)
val eq : t -> t -> bool

  (*s [cmp s1 s2] yields [Same] iff [s1] and [s2] have the same denotations,
      [Disjoint] if their respective denotations are disjoint sets, [Sub] if
      the denotation of [s1] is a strict subset of the denotation of [s2],
      [Super] if the denotation of [s2] is a strict subset of [s1], and [Overlap]
      otherwise. *)
    
val cmp : t -> t -> Binrel.t   

    (*s Pretty printing set constraints. *)
val pp : Format.formatter -> t -> unit

    (*s Set operations on constraints. *)
val inter : t -> t -> t
val union : t -> t -> t
val compl : t -> t
    
    (*s Abstract interpretation of arithmetic addition and multiplication in terms
      of intervals. *)
val add : t -> t -> t
val mult : t -> t -> t

    (*s [to_list l] returns a list of intervals. Hereby, an interval is
      represented by a triple [(d,l,h)] where [d] is the domain over which the interval
      is interpreted, [l] is the lower bound, and [h] is the upper bound. A lower bound
      is either negative infinity [Neginf], or [Low(k,q)], where the rational [q] is the
      lower bound, and the kind [k] determines if [q] belongs to the interval or not.
      Similarly, an upper bound is either infinity [Posinf] or [High(k,q)].

      The intervals [of_list l] are disjoint and ordered from left-to-right. Moreover,
      every interval is nonempty. The union of the denotations of these intervals is
      the subset of the reals denoted by [l]. *)

type kind = Strict | Nonstrict

type low =
  | Neginf
  | Low of kind * Q.t

type high =
  | Posinf
  | High of kind * Q.t

type interval = domain * low * high
  

val to_list: t -> interval list









