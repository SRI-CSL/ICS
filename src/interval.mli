(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
*)

(** {i Rational intervals} consist of a 
  - lower endpoint [lo] which is either a rational or a representation
    of negative infinity,
  - an upper endpoint [hi] which is either a rational or a representation
    of positive infinity, and
  - an interpretation domain [dom] in {!Dom.t}.

  The denotation of an interval [i], denoted by {v D(i) v},  is a subset of
  the set of real numbers defined as follows:
  - {v D(i) v} is the closed interval [{x in i.dom | i.lo <= x <= i.hi}]

  @author Harald Ruess
*)


type t

(** {6 Constructors} *)

val make : Dom.t * (bool * Mpa.Q.t) option * (Mpa.Q.t * bool) option -> t
  (** [make (d, l, h)] constraints a rational interval with interpretation
   domain [d], lower endpoint [l], and upper endpoint [h]. *)

val mk_zero : t
  (** [mk_zero] is the singleton interval containing only [0].  *)

val mk_empty : t
  (** [D(mk_empty) is the empty set *)

val mk_dom : Dom.t -> t
  (** [D(mk_dom d)] is the real number line if [d] is {!Dom.Real} and 
   the set of integers if [d] is {!Dom.Int} *)
 
val mk_real : t 
  (** The denotation of the interval [mk_real] contains the whole real number line. *)

val mk_int : t
  (** The denotation of [mk_int] contains all integers (positive and negative). *)

val mk_nat : t

val mk_pos : t
val mk_neg : t
val mk_nonneg : t
val mk_nonpos : t

val mk_singleton : Mpa.Q.t -> t
  (** The denotation of [mk_singleton q] contains only the rational [q]. *)

val mk_zero : t
val mk_one : t


(** {6 Destructors.} *)

val destructure : t -> Dom.t * (bool * Mpa.Q.t) option * (Mpa.Q.t * bool) option
  (** The accessor [destructure i] returns the endpoint information [(d, lo, hi)]
    for an interval [i] with denotation [D(make d lo hi)]. *)

val dom : t -> Dom.t
  (** [dom i] returns the interpretation domain of an interval. *)

val lo : t -> (bool * Mpa.Q.t) option
  (** [lo i] returns the lower bound in the form of an endpoint [(a, alpha)] *)

val hi : t -> (Mpa.Q.t * bool) option
  (** [hi i] the upper bound in the form [(b, beta)]. *)


(** {6 Recognizers} *)

val d_singleton : t -> Mpa.Q.t option
  (** [is_singleton i] returns the single value [Some(q)] for an interval whose 
    denotation is a singleton set over the reals. Otherwise it returns [None]. *)

val is_empty : t -> bool
  (** [is_empty i] holds iff if [D(i)] is empty. *)

val is_full : t -> bool
  (** [is_full i] holds iff if [D(i)] is the full real number line *)

type status =
  | Empty
  | Full
  | Singleton of Mpa.Q.t
  | Other

val status : t -> status


(** {6 Predicates} *)

val mem: Mpa.Q.t -> t -> bool
  (** [mem q i] holds iff the rational [q] is in [D(i)]. *)

val is_sub : t -> t -> bool
  (** [is_sub i j] iff [D(i)] is a subset of [D(j)]. *)

val is_disjoint : t -> t -> bool
 (** [is_disjoint i j] iff the intersection of [D(i)] and  [D(j)] is empty. *)
  

(** {6 Intersection} *)

val inter : t -> t -> t
  (** [D(inter i j)] is [D(i)] inter [D(j)]. *)

val complement : t -> t
  (** [D(complement i)] is [R \ D(i)] with [R] the real number line if
      [dom i] is {!Dom.Real} and [R] is {!Dom.Int} if [R] is {!Dom.Int},
      if [complement i] can be represented as an interval. Otherwise,
      [Invalid_argument] is raised. *)

val is_complementable : t -> bool
  (** [is_complementable i] holds iff at most one interval bound of [i] is 
   rational. If [is_complementable i] holds, then [complement i] does not
   raise [Invalid_argument]. *)
  
(** {6 Interval arithmetic.} *)
  
val add : t -> t -> t
  (**  [D(add i j)] is [D(i) + D(j) := { x + y | x in D(i), y in D(j) }] *)

val addl : t list -> t

val sub : t -> t -> t
  (** [D(sub i j)] is [D(i) - D(j)] *)

val multq :  Mpa.Q.t -> t -> t
  (** [D(multq q i)] is [q * D(j)] *)

val addq :  Mpa.Q.t -> t -> t
  (** [D(addq q i)] is [q + D(j)] *)

val mult :  t -> t -> t
  (** [D(mult i j)] is a superset of [D(i) * D(j)] *)

val multl : t list -> t

val expt : int -> t -> t
  (** [D(expt n i)] is a superset of [D(i)^n]. *)

val div : t -> t -> t
  (** [D(div i j)] consists of the set of rationals [z] such that
    there exists [x in D(i)], [y in D(j)] with [y /= 0], [z = x / y]. *)


(** {6 Comparisons} *)

val eq: t -> t -> bool
  (** [eq i j] holds iff [D(i)] equals [D(j)]. In particular, two different
    intervals with empty denotations are always identified to be equal. *)


(** {6 Printing} *)

val pp : t Pretty.printer
  (** Printing an interval. *)


