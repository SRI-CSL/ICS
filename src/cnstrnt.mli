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
 *)

(*s A {b real number constraint} consists of an {i interval} [i] with an 
  interpretation domain in {!Dom.t}, a lower and an upper endpoint
  of type {!Endpoint.t}, and a set of rationals, the so-called 
  {i exception set} [qs].  Each such constraint [c] {b denotes} a subset
  of the real numbers, denoted by [C(c)], which is obtained by removing 
  all rationals of the exception set [qs] from the denotation
  [D(i)] of the interval [i] (see module [Interval]). 
  
 @author Harald Ruess
*)


type t

module Diseqs: (Set.S with type elt = Mpa.Q.t)
  (** Exception sets are sets of rationals. *)

(** {6 Constructors} *)

val make : Interval.t * Diseqs.t -> t
  (** [make (i, qs)] constructs a constraint with interval [i]
    and exception set [qs]. *)

val mk_real : t
  (** Constraint with the real number line as its denotation. *)

val mk_int : t
  (** Constraint whose denotation are exactly the integers. *)

val mk_nonint : t
  (** Constraint whose denotation are all non-integer reals. *)

val mk_nat : t
  (** Constraint whose denotation are all nonnegative integers
    (that is, including [0]). *)

val mk_singleton : Mpa.Q.t -> t
  (** [mk_singleton q] constructs a constraint with a singleton
    denotation containing [q]. *)

val mk_zero : t
  (** [C(mk_zero)] contains only [0]. *)

val mk_one : t
  (** [C(mk_zero)] contains only [1]. *)

val mk_diseq : Mpa.Q.t -> t
  (** [C(mk_diseq q)] contains all reals except for [q]. *)

val mk_oo : Dom.t -> Mpa.Q.t -> Mpa.Q.t -> t
 (** [C(mk_oo d q p)] is [{x in d | q < x < p}]. *)
  
val mk_oc : Dom.t -> Mpa.Q.t -> Mpa.Q.t -> t
  (** [C(mk_oc d q p)] is [{x in d | q < x <= p}]. *)

val mk_co : Dom.t -> Mpa.Q.t -> Mpa.Q.t -> t
 (** [C(mk_co d q p)] is [{x in d | q <= x < p}]. *)
  
val mk_cc : Dom.t -> Mpa.Q.t -> Mpa.Q.t -> t
  (** [C(mk_cc d q p)] is [{x in d | q <= x <= p}]. *)

val mk_lower : Dom.t -> Mpa.Q.t * bool -> t
  (** - [C(mk_lower d (q, false))] is [{x in d | x < q}] and
      - [C(mk_lower d (q, true))] is [{x in d | x <= q}] *)

val mk_upper : Dom.t -> bool * Mpa.Q.t -> t
 (** - [C(mk_upper d (false, q))] is [{x in d | x > q}] and
     - [C(mk_upper d (true, q))] is [{x in d | x >= q}] *)

val mk_lt : Dom.t -> Mpa.Q.t -> t
  (** [C(mk_lt d q)] is [{x in d | x < q}] *)

val mk_le : Dom.t -> Mpa.Q.t -> t
  (** [C(mk_le d q)] is [{x in d | x <= q}] *)

val mk_gt : Dom.t -> Mpa.Q.t -> t
 (** [C(mk_gt d q)] is [{x in d | x > q}] *)

val mk_ge : Dom.t -> Mpa.Q.t -> t
  (** [C(mk_ge d q)] is [{x in d | x >= q}] *)

val mk_pos : Dom.t -> t
  (** [C(mk_pos d)] is [{x in d | x > 0}] *)

val mk_neg : Dom.t -> t
  (** [C(mk_pos d)] is [{x in d | x < 0}] *)

val mk_nonneg : Dom.t -> t
  (** [C(mk_nonneg d)] is [{x in d | x >= 0}] *)
  
val mk_nonpos : Dom.t -> t
  (** [C(mk_nonpos d)] is [{x in d | x <= 0}] *)

val of_interval : Interval.t -> t
  (** Construct a constraint [c] from an interval [i]
    with [C(c) = D(i)] *)


(** {6 Accessors} *)

val destruct : t -> Interval.t * Diseqs.t
  (** Destructure constraint into [(i, qs)]. *)

val dom_of : t -> Dom.t
  (** Get interpretation domain of a constraint. *)

val endpoints_of : t -> Endpoint.t * Endpoint.t
  (** [endpoints_of c] yields the pair [(lo, hi)]
    of the lower endpoint [lo] and the upper endpoint [hi]
    of the interval part of [c]. *)

val d_singleton : t -> Mpa.Q.t option
  (** If [C(c)] is the singleton set with element [q],
    [d_singleton c] yields [Some(q)], and [None] otherwise. *)

type bounds = 
  | Lower of Dom.t * bool * Mpa.Q.t
  | Upper of Dom.t * Mpa.Q.t * bool
  | LowerUpper of Dom.t * bool * Mpa.Q.t * Mpa.Q.t * bool
  | Unbounded of Dom.t

val bounds : t -> bounds
  (** Let [c] be a constraint with lower bound [lo]
     and upper bound [hi]; then [bounds c] yields:
    - [Lower(d, alpha, q)] if ... *)


val d_lower : t -> (Dom.t * bool * Mpa.Q.t) option

val d_upper : t -> (Dom.t * Mpa.Q.t * bool) option



(** {6 Recognizers} *)

val is_empty : t -> bool
  (** [is_empty c] holds iff [C(c)] is the empty set. *)

val is_full : t -> bool
  (** [is_full c] holds iff [C(c)] is the real number line *)

val is_unbounded : t -> bool
  (** [is_unbounded c] holds if both the lower and the upper
    endpoint are nonrational. *)

val is_finite : t -> bool
  (** [is_finite c] holds if [C(c)] is finite. *)

val is_pos : t -> bool
  (** [is_pos c] holds iff all elements in [C(c)] are positive. *) 

val is_neg : t -> bool
  (** [is_neg c] holds iff all elements in [C(c)] are negative. *) 

val mem : Mpa.Q.t -> t -> bool
  (** [mem q c] holds iff [q] is in [C(c)]. *)


(** {6 Relations} *)

val eq : t -> t -> bool
  (** [eq c d] holds iff [C(c)] is equal to [C(d)]. *)

val sub : t -> t -> bool
  (** [sub c d] holds iff [C(c)] is a subset of [C(d)]. *)

val is_disjoint : t -> t -> bool
  (** [is_disjoint c d] holds iff [C(c)] and [C(d)] are disjoint. *)

val cmp : t -> t -> t Binrel.t
  (** [cmp c d] returns 
    - [Sub] if [sub c d] holds and [eq c d] does not hold
    - [Equal] if [eq c d] holds
    - [Super] if [sub d c] holds and [eq d c] does not hold
    - [Disjoint] if [is_disjoint c d] holds
    - [Singleton(q)] if the intersection [inter c d] denotes a singleton
                     set with element [q].
    - [Overlap(e)] if none of the above holds, and [e] is the intersection
                     of [c] and [d]. *)

val status : t -> Mpa.Q.t Status.t
  (** [status c] returns
    - [Empty] if [C(c)] is empty,
    - [Full] if [C(c)] is the real number line,
    - [Singleton(q)] if [q] is the only member in [C(c)], and
    - [Other] if none of the above holds. *)


(** {6 Connectives} *)

val inter : t -> t -> t
  (** [C(inter c d)] equals [C(c)] intersected with [C(d)]. *)


(** {6 Abstract interpretation} *)

val addq : Mpa.Q.t -> t -> t
  (** [C(addq q c)] equals [q + C(c)]. *)

val add : t -> t -> t
  (** [C(add c d)] equals [C(c) + C(d)]. *)

val addl : t list -> t
  (** - [C(addl [])] equals the singleton set with element [0],
    - [C(addl [c])] equals [C(c)], and
    - [C(addl [c0;...;cn])] equals [C(c0) + ... + C(cn)]. *)

val subtract : t -> t -> t
  (** [C(subtract c d)] equals [C(c) - C(d)]. *)

val multq : Mpa.Q.t -> t -> t
  (** [C(addq q c)] equals [q * C(c)]. *)

val mult : t -> t -> t
  (** [C(mult c d)] is a superset of [C(c) * C(d)]. *)

val multl : t list -> t
  (** - [C(multl [])] equals the singleton set with element [1],
    - [C(multl [c])] equals [C(c)], and
    - [C(multl [c0;...;cn])] equals [C(c0) * ... * C(cn)]. *)

val expt : int -> t -> t
   (** [C(expt n c)] is a superset of [C(c)^n]. *)

val div : t -> t -> t
 (** [C(div c d)] is a superset of the set of rationals [z] such that
    there exists [x in C(c)], [y in C(d)] with [y /= 0], [z = x / y]. *)


(** {6 Pretty-printing constraints} *)

val pp : t Pretty.printer
