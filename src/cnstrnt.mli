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

  A {b real number constraint} consists of 
  - an interpretation domain in {!Dom.t} and
  - a set of intervals.
  Hereby, an {i interval} consists of a lower bound and an upper bound,
  and a {i bound} is a pair [(alpha, a)], where [alpha] is a Boolean and
  [a] is either a term of type {!Term.t} or a representation of positive
  or negative infinity.  If [alpha] is [true] the bound is said to be
  {i nonstrict} and otherwise it is {i strict}. 

  A real number constraint [a in dom{(l1,u1),...,(ln,un)}] is equivalent
  to [a in dom] conjoined with [a >(=) li] and [a <(=) ui] for [i = 1,...,n]. 
  Here, we use strict inequalities (that is, [<] and [>]) iff the corresponding
  bound [li] or [ui] is strict.
  
 @author Harald Ruess
*)

type t

(** {6 Bounds} *)

type bound = 
  | Posinf
  | Neginf
  | Bound of bool * Term.t
      (** A bound is either a representation [Posinf] for positive
        infinite, [Neginf] for negative infinite, or a pair [(alpha, a)] *)


(** {6 Constructors} *)

val mk_empty : t
  (** The empty constraint. *)

val mk_real : t
  (** Real number constraint. *)

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
val add_diseq : Term.t -> t -> t


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
