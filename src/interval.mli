
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

(*s Module [Interval]: Real intervals with rational endpoints (including infinity). *)

type t

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

val make : Dom.t * Endpoint.t * Endpoint.t -> t

(*s The accessor [destructure i] returns the endpoint information [(d,lo,hi)]
  for an interval [i] with denotation [D(make d lo hi)]. [lo i] returns
  the lower bound in the form [(a,alpha)] and [hi i] the upper bound in the form
  [(b, beta)]. *)

val destructure : t -> Dom.t * Endpoint.t * Endpoint.t

val dom : t -> Dom.t
val lo : t -> Endpoint.t
val hi : t -> Endpoint.t

(*s Derived constructors. *)

val mk_zero : t
val mk_empty : t 
val mk_real : t 
val mk_int : t
val mk_singleton : Mpa.Q.t -> t

(*s [is_singleton i] returns the single value for an interval whose 
    denotation is a singleton set over the reals. *)

val d_singleton : t -> Mpa.Q.t option
val is_empty : t -> bool
val is_full : t -> bool

(*s [rational_endpoints i] returns the pair of rational endpoints. *)

val rational_endpoints : t -> (Mpa.Q.t * Mpa.Q.t) option

(*s [mem q i] tests if the rational [q] is a member of the interval [i]. *)

val mem: Mpa.Q.t -> t -> bool

(*s Equality [eq i j] on intervals [i], [j] holds if and only if they denote
    the same set of numbers. In particular, two empty intervals are always 
    identified to be equal. *)

val eq: t -> t -> bool

(*s Union and intersection of two intervals. It is the case that
  the denotation of [union i j] (resp. [inter i j]) is the union 
  (resp. intersection) of the denotations of [i] and [j]. *)

val union : t -> t -> t

val inter : t -> t -> t


(*s Interval arithmetic. Let [i], [j] be two intervals
    with denotations [D(i)] and [D(j)].  Then, 
    [D(add i j)] is [D(i) + D(j)],
    [D(sub i j)] is [D(i) - D(j)],
    [D(mult i j)] is [D(i) * D(j)],
    and [D(div i j)] consists of the set of rationals 
    $\{ z | \exists x \in D(i), y \in D(j) \mbox{~such that~} y /= 0,\,z = x / y\}$\@. *)
  
val add : t -> t -> t
val subtract : t -> t -> t
val multq :  Mpa.Q.t -> t -> t
val mult :  t -> t -> t
val expt : int -> t -> t
val div :   t -> t -> t

(*s Comparisons. *)

val cmp : t -> t -> t Binrel.t

val sub : t -> t -> bool

val disjoint : t -> t -> bool

(*s Printing an interval. *)

val pp : Format.formatter -> t -> unit
