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

(** Real number constraints.

  A {b real number constraint} consists of an {i interval} [i] with an 
  interpretation domain in {!Dom.t}, a lower and an upper endpoint
  of type {!Endpoint.t}, and a set of rationals, the so-called 
  {i exception set} [qs].  Each such constraint [c] {b denotes} a subset
  of the real numbers, denoted by [C(c)], which is obtained by removing 
  all rationals of the exception set [qs] from the denotation
  [D(i)] of the interval [i] (see module [Interval]). 
  
 @author Harald Ruess
*)


type t

module Low : sig 
  
  type t = 
    | Neginf 
    | Bound of bool * Term.t

end

module High : sig

  type t =
    | Posinf
    | Bound of Term.t * bool

end

(** {6 Constructors} *)

val mk_empty : t
  (** Constraint with empty denotation. *)

val mk_real : t
  (** Constraint with the real number line as its denotation. *)

val mk_int : t
  (** Constraint whose denotation are exactly the integers. *)

val mk_nonint : t
  (** Constraint whose denotation are all non-integer reals. *)

val mk_dom : Dom.t -> t
  (** constraint with domain denotation. *)

val mk_nat : t
  (** Constraint whose denotation are all nonnegative integers
    (that is, including [0]). *)

val mk_singleton : Mpa.Q.t -> t
  (** [mk_singleton q] constructs a constraint with a singleton
    denotation containing [q]. *)

val mk_equal : Term.t -> t
  (** [mk_equal x] constructs a constraint with a singleton interpretation. *)

val mk_zero : t
  (** [C(mk_zero)] contains only [0]. *)

val mk_one : t
  (** [C(mk_zero)] contains only [1]. *)

val mk_less : Dom.t -> (Term.t * bool) -> t

val mk_greater : Dom.t -> (bool * Term.t) -> t

(** {6 Accessors} *)

val dom_of : t -> Dom.t
  (** Get interpretation domain of a constraint. *)

val high_of : t -> Mpa.Q.t * bool

val low_of : t -> bool * Mpa.Q.t

val numeric_of : t -> t

val d_equalities : t -> Term.Set.t * t
  (** [d_equalities c] destructs [c] into a pair [(es, d)]
    consisting of the implied equalities in [es] and the 
    remaining nonequality constraints [d]. *)

val implied : t -> Arith.ineq list
  (** [implied c] returns a list of all nontrivially implied inequalities of [c]. *)

val equal : Term.t -> t -> Arith.ineq list


(** {6 Recognizers} *)

val is_empty : t -> bool
  (** [is_empty c] holds iff [C(c)] is the empty set. *)

val is_full : t -> bool
  (** [is_real c] holds iff [C(c)] is the real number line *)

val is_unbounded : t -> bool
  (** [is_unbounded c] holds if both the lower and the upper
    endpoint are nonrational. *)

val is_finite : t -> bool
  (** [is_finite c] holds if [C(c)] is finite. *)


(** {6 Relations} *)

val eq : t -> t -> bool
  (** [eq c d] holds iff [C(c)] is equal to [C(d)]. *)

val sub : t -> t -> bool
  (** [sub c d] holds iff [C(c)] is a subset of [C(d)]. *)

val disjoint : t -> t -> bool
  (** [disjoint c d] holds iff [C(c)] and [C(d)] are disjoint. *)


type rel = 
  | Disjoint
  | Same
  | Sub
  | Super
  | Overlap

val cmp : t -> t -> rel
  (** [cmp c d] returns 
    - [Sub] if [sub c d] holds and [eq c d] does not hold
    - [Equal] if [eq c d] holds
    - [Super] if [sub d c] holds and [eq d c] does not hold
    - [Disjoint] if [disjoint c d] holds
    - [Singleton(q)] if the intersection [inter c d] denotes a singleton
                     set with element [q].
    - [Overlap(e)] if none of the above holds, and [e] is the intersection
                     of [c] and [d]. *)


(** {6 Connectives} *)

val inter : t -> t -> t
  (** [C(inter c d)] equals [C(c)] intersected with [C(d)]. *)


(** {6 Pretty-printing constraints} *)

val pp : t Pretty.printer


(** {6 Iterators} *)

val fold : (Low.t * High.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over all intervals *)

val varfold : (Term.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Folding over all variables. *)

val replace : Term.t -> Term.t -> t -> t
  (** [replace x a c] replaces all occurrences of [x] in [c] by [a]. *)

(** {6 Constraint abstraction} *)

val add : t -> t -> t
val addq : Mpa.Q.t -> t -> t
val multq : Mpa.Q.t -> t -> t
val subtract : t -> t -> t
val mult : t -> t -> t
val multl : t list -> t
val expt : int -> t -> t
val div : t -> t -> t

val of_term : (Term.t -> t) -> Term.t -> t

val of_addl : (Term.t -> t) -> Term.t list -> t


(** {6 Tests} *)

val occurs : Term.t -> t -> bool
  (** [occurs x c] holds iff variable [x] is a subterm of some bound in [c]. *)
  
val is_int : (Term.t -> t) -> Term.t -> bool
  (** Test if arithmetic term is integer. *)

val notin : Term.t -> t -> bool
  (** [notin a c] returns [false] when [a] is known to be not in [c]. *)
  
val is_diophantine : (Term.t -> t) -> Term.t -> bool
  (** [is_diophantine c a] holds if all variables in the linear 
    arithmetic term [a] are interpreted over the integers, that is,
    if [c(x)] yields a subconstraint of {!Cnstrnt.int}. *)
  
val lower_is_subsumed : bool * Term.t -> t -> bool

val upper_is_subsumed : Term.t * bool -> t -> bool
