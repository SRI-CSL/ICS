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

type bound = 
  | Posinf
  | Neginf
  | Bound of bool * Term.t


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

val d_equalities : t -> Term.Set.t * t
  (** [d_equalities c] destructs [c] into a pair [(es, d)]
    consisting of the implied equalities in [es] and the 
    remaining nonequality constraints [d]. *)

(** {6 Recognizers} *)

val is_empty : t -> bool
  (** [is_empty c] holds iff [C(c)] is the empty set. *)

(** {6 Relations} *)

val eq : t -> t -> bool
  (** [eq c d] holds iff [C(c)] is equal to [C(d)]. *)


(** {6 Connectives} *)

val inter : t -> t -> t
  (** [C(inter c d)] equals [C(c)] intersected with [C(d)]. *)


(** {6 Pretty-printing constraints} *)

val pp : t Pretty.printer

(** {6 Adding new constraints} *)

val add_lower : bool * Term.t -> t -> t
val add_upper : bool * Term.t -> t -> t
val add_dom : Dom.t -> t -> t


(** {6 Iterators} *)

val exists : (bound * bound -> bool) -> t -> bool

val exists_lower : (bool * Term.t -> bool) -> t -> bool
val exists_upper : (bool * Term.t -> bool) -> t -> bool

val fold : (bound * bound -> 'a -> 'a) -> t -> 'a -> 'a

val fold_lower : (bool * Term.t -> 'a -> 'a) -> t -> 'a -> 'a

val fold_upper : (bool * Term.t -> 'a -> 'a) -> t -> 'a -> 'a

val replace : Term.t -> Term.t -> t -> t
  (** [replace x a c] replaces all occurrences of [x] in [c] by [a]. *)


(** {6 Constraint abstraction} *)

val of_term : (Term.t -> t) -> Term.t -> t

val of_addl : (Term.t -> t) -> Term.t list -> t

val is_int : (Term.t -> t) -> Term.t -> bool

  
val is_diophantine : (Term.t -> t) -> Term.t -> bool
  (** [is_diophantine c a] holds if all variables in the linear 
    arithmetic term [a] are interpreted over the integers, that is,
    if [c(x)] yields a subconstraint of {!Cnstrnt.int}. *)


(** {6 Tests} *)

val occurs : Term.t -> t -> bool
  (** [occurs x c] holds iff variable [x] is a subterm of some bound in [c]. *)
 
