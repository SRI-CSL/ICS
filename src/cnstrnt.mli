
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

(*s Module [Cnstrnt]: extends the [Interval] module with functions
  specific to terms. *)

(*i*)
open Term
open Mpa
(*i*)

type t = cnstrnt

(*s Constructors. *)

val top : cnstrnt
val bot : cnstrnt
val boolean : cnstrnt
val tuple : cnstrnt
val arith : Interval.t -> cnstrnt
val oo : Interval.domain -> Q.t -> Q.t -> cnstrnt
val oc : Interval.domain -> Q.t -> Q.t -> cnstrnt
val co : Interval.domain -> Q.t -> Q.t -> cnstrnt
val cc : Interval.domain -> Q.t -> Q.t -> cnstrnt
val lt : Interval.domain -> Q.t -> cnstrnt
val le : Interval.domain -> Q.t -> cnstrnt
val gt : Interval.domain -> Q.t -> cnstrnt
val ge : Interval.domain -> Q.t -> cnstrnt
val real : cnstrnt
val int : cnstrnt
val nonint : cnstrnt
val neg : cnstrnt
val nonpos : cnstrnt
val singleton : Q.t -> cnstrnt
val diseq : Q.t -> cnstrnt


(*s Recognizers and Accessors. *)

val is_bot : cnstrnt -> bool
val is_top : cnstrnt -> bool
val is_tuple : cnstrnt -> bool
val is_boolean : cnstrnt -> bool

val is_arith : cnstrnt -> bool
val d_arith : cnstrnt -> Interval.t

val is_singleton : cnstrnt -> bool
val d_singleton : cnstrnt -> Q.t

val is_nonzero: cnstrnt -> bool   (*s true only if [0] is not known to be in argument constraint. *)

(*s Comparison of constraints. *)

val cmp : cnstrnt -> cnstrnt -> Binrel.t

val sub : cnstrnt -> cnstrnt -> bool

(*s Get a constraints corresponding to the domain over
  which term is to be interpreted. *)

val of_interp: Term.t -> cnstrnt

(*s Membership test. *)

type status = Yes | No | X
   
val mem : Term.t -> cnstrnt -> status

(*s Analyze a constraint. *)

type analyze =
  | Empty
  | Full
  | Singleton of Q.t
  | Other

val analyze : cnstrnt -> analyze

(*s Union and intersection of constraints. *)

val union : cnstrnt -> cnstrnt -> cnstrnt
val inter : cnstrnt -> cnstrnt -> cnstrnt
val compl : cnstrnt -> cnstrnt

(*s Abstract interpretation of an arithmetic term with [Cnstrnts.t] as
    abstract domain. [cnstrnt f a] first checks if the context [f]
    contains a declaration or not not. In the first case, this
    constraint is returned. Otherwise, when [f] throws the exception
    [Not_found] then it computes a most refined constrained by traversing
    arithmetic terms *)

val of_term : (Term.t -> cnstrnt) -> Term.t -> cnstrnt
