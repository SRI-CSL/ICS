
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

(*s Module [Interval]: Real intervals with rational
 endpoints (including infinity). *)

type t

type endpoint = Extq.t * bool

val posinf : endpoint
val neginf : endpoint
val strict : Mpa.Q.t -> endpoint
val nonstrict : Mpa.Q.t -> endpoint

(* [make d (a,alpha) (b,beta)] constructs a general, interval 
   with rational endpoints [a] and [b] (including negative and positive
   infinity) and two bits of information [alpha] and [beta], 
   with [alpha] (resp. [beta]) specifying whether [a] (resp. [b]) belongs to the interval. 
   Each interval denotates a set of reals (or integers), denoted by [D(i)].
   [D(make d (a,true) (b,true))] equals the closed interval [{x in d | a <= x <= b}],
   [D(make d (a,true) (b,false))] is the right-open interval [{x in d | a <= x < b}],
   [D(make d (a,false) (b, true))] denotes the left-open interval [{x in d | a < x <= b}],
   and [D(make d (a,false) (b,false))] denotes the open interval [{x in d | a < x < b}]. 
*)

val make : Dom.t -> endpoint -> endpoint -> t

(*s The accessor [destructure i] returns the endpoint information [(d,lo,hi)]
  for an interval [i] with denotation [D(make d lo hi)]. [lo i] returns
  the lower bound in the form [(a,alpha)] and [hi i] the upper bound in the form
  [(b, beta)]. *)

val destructure : t -> endpoint * endpoint

val lo : t -> endpoint
val hi : t -> endpoint

(*s Derived constructors. *)


val zero : t
val empty : t 
val full : t 
val singleton : Mpa.Q.t -> t

(*s [is_singleton i] returns the single value for an interval whose 
    denotation is a singleton set over the reals. *)

val is_singleton : t -> Mpa.Q.t option
val is_empty : t -> bool
val is_full : t -> bool

(*s [mem q i] tests if the rational [q] is a member of the interval [i]. *)

val mem: Mpa.Q.t -> t -> bool

(*s Equality [eq i j] on intervals [i], [j] holds if and only if they denote
    the same set of numbers. In particular, two empty intervals are always 
    identified to be equal. *)

val eq: t -> t -> bool

(*s Union and intersection of two intervals. It is the case that
  the denotation of [union i j] (resp. [inter i j]) is the union 
  (resp. intersection) of the denotations of [i] and [j]. *)

val union : Dom.t -> t -> t -> t

val inter : Dom.t -> t -> t -> t

type one_or_two =
  | Empty
  | One of t
  | Two of t * t

val compl : Dom.t -> t -> one_or_two

(*s Interval arithmetic. Let [i], [j] be two intervals
    with denotations [D(i)] and [D(j)].  Then, 
    [D(add i j)] is [D(i) + D(j)],
    [D(sub i j)] is [D(i) - D(j)],
    [D(mult i j)] is [D(i) * D(j)],
    and [D(div i j)] consists of the set of rationals 
    $\{ z | \exists x \in D(i), y \in D(j) \mbox{~such that~} y /= 0,\,z = x / y\}$\@. *)
  
val add : Dom.t -> t -> t -> t
val multq :  Dom.t -> Mpa.Q.t -> t -> t
val mult :  Dom.t -> t -> t -> t
val div :   Dom.t -> t -> t -> t

val cmp : t -> t -> Allen.t

(*s Printing an interval. *)

val pp : Format.formatter -> t -> unit
