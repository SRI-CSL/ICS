
(*
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
*)

(** Module [Interval]: Real intervals with lower and upper 
  endpoints of type {!Endpoints.t} and an interpretation domain in {!Dom.t}.

  Let [i] be a interval with lower bound [lo], higher bound [hi], and
  interpretation domain [d]. The denotation [D(i)] of [i] is a subset of the 
  set of real numbers defined as follows.
  [D(make d '(a,true)' '(b,true)')] equals the closed interval [{x in d | a <= x <= b}],
  [D(make d '(a,true)' '(b,false)')] is the right-open interval [{x in d | a <= x < b}],
  [D(make d '(a,false)' '(b, true)')] denotes the left-open interval [{x in d | a<x <= b}],
  and [D(make d '(a,false)' '(b,false)')] denotes the open interval [{x in d | a < x < b}]. 
*)

type t

(** {Constructor}. *)

val make : Dom.t * Endpoint.t * Endpoint.t -> t
(** [make d (a,alpha) (b,beta)] constructs a general, interval 
   with rational endpoints [a] and [b] (including negative and positive
   infinity) and two bits of information [alpha] and [beta], 
   with [alpha] (resp. [beta]) specifying whether [a] (resp. [b]) 
   belongs to the interval. *)

val mk_zero : t
(** [mk_zero] is the singleton interval containing only [0].  *)

val mk_empty : t
(** [mk_empty] is an interval with empty denotation. *)
 
val mk_real : t 
(** The denotation of the interval [mk_real] contains the whole real number line. *)

val mk_int : t
(** The denotation of [mk_int] contains all integers (positive and negative). *)

val mk_nonint : t
(** The denotation of [mk_nonint] is comprised of all reals which are not integer. *)

val mk_singleton : Mpa.Q.t -> t
(** The denotation of [mk_singleton q] contains only the rational [q]. *)


(** {Destructors.} *)

val destructure : t -> Dom.t * Endpoint.t * Endpoint.t
(** The accessor [destructure i] returns the endpoint information [(d, lo, hi)]
  for an interval [i] with denotation [D(make d lo hi)]. *)

val dom : t -> Dom.t
(** [dom i] returns the interpretation domain of an interval. *)

val lo : t -> Endpoint.t
(** [lo i] returns the lower bound in the form of an endpoint [(a, alpha)] *)

val hi : t -> Endpoint.t
(** [hi i] the upper bound in the form [(b, beta)]. *)


(** {Recognizers.} *)

val d_singleton : t -> Mpa.Q.t option
(** [is_singleton i] returns the single value [Some(q)] for an interval whose 
    denotation is a singleton set over the reals. Otherwise it returns [None]. *)

val is_empty : t -> bool
(** [is_empty i] holds iff if [D(i)] is empty. *)

val is_full : t -> bool
(** [is_full i] holds iff if [D(i)] is the full real number line *)

val rational_endpoints : t -> (Mpa.Q.t * Mpa.Q.t) option
(** [rational_endpoints i] returns the pair of rational endpoints. *)


(** {Predicates.} *)


val mem: Mpa.Q.t -> t -> bool
(** [mem q i] tests if the rational [q] is in [D(i)]. *)


val eq: t -> t -> bool
(** [eq i j] holds iff [D(i)] equals [D(j)]. In particular, two different
  intervals with empty denotations are always identified to be equal. *)


(** {Connectives.} *)

val union : t -> t -> t
(** [D(union i j)] contains the union of [D(i)] and [D(j)]. *)


val inter : t -> t -> t
(** [D(inter i j)] is [D(i)] inter [D(j)]. *)


(** {Interval arithmetic.} *)
  
val add : t -> t -> t
(**  [D(add i j)] is [D(i) + D(j)]. *)

val subtract : t -> t -> t
(** [D(sub i j)] is [D(i) - D(j)] *)

val multq :  Mpa.Q.t -> t -> t
(** [D(multq q i)] is [q * D(j)] *)

val mult :  t -> t -> t
(** [D(mult i j)] is a superset of [D(i) * D(j)] *)

val expt : int -> t -> t
(** [D(expt n i)] is [D(i)^n]. *)

val div :   t -> t -> t
(** [D(div i j)] consists of the set of rationals [z] such that
 there exists [x in D(i)], [y in D(j)] with [y /= 0], [z = x / y]. *)


(** {Comparisons.} *)

val cmp : t -> t -> t Binrel.t

val sub : t -> t -> bool
(** [sub i j] holds iff [D(i)] is a subset of [D(j)]. *)

val disjoint : t -> t -> bool
(** [disjoint i j] holds iff [D(i)] and [D(j)] are disjoint. *)


(** {Printing} *)

val pp : t Pretty.printer
(** Printing an interval. *)
