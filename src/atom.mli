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

(** Atomic predicates.

  @author Harald Ruess
*)

(** An {i atomic predicate} is either
  - one of the constants [True] or [False],
  - an equality [a = b],
  - a disequality [a <> b], or 
  - an inequality [a >= b]. *)
type t =
  | TT
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | Cnstrnt of Term.t * Cnstrnt.t
  | FF
 
val mk_true : t
  (** Atom-index pair for representing the [true] atom. *)

val mk_false : t
  (** Atom-index pair for representing the [false] atom. *)

val mk_equal : Term.t -> Term.t -> t
  (** The atom-index pair [mk_equal s t] represents the equality [s = t]. *)

val mk_diseq : Term.t -> Term.t -> t
  (** The atom-index pair [mk_diseq s t] represents the disequality [s <> t]. *)

val mk_nonneg : Term.t -> t
  (** The atom-index pair [mk_nonneg t] represents the arithmetic inequality [t >= 0]. *)

val mk_pos : Term.t -> t
  (** The atom-index pair [mk_pos t] represents the arithmetic inequality [t > 0]. *)

val mk_cnstrnt : Term.t -> Cnstrnt.t -> t
  (** The atom-index pair [mk_cnstrnt a c] represents the constraint [a in c]. *)

val map : (Term.t -> Term.t) -> t -> t
  (** [map f atm] replaces terms [a] in [atm] with [f a]. *)

val replace: Term.t -> Term.t -> t -> t
  (** [replace a b atm] replaces terms [a] by [b] in [atm]. *)

val eval : Term.Model.t -> t -> t
  (** [eval (i, alpha) atm] computes [atm] instantiated by the term model [(i, alpha)]. *)

val validates : Term.Model.t -> t -> bool
  (** [validates (i, alpha) atm] holds iff [i, alpha |= atm], that is,
    [atm] evaluates to the trivially valid atom for the term model [(i, alpha)]. *)

val is_true : t -> bool
  (** [is_true atm] holds iff [atm] represents the [true] atom. *)

val is_false : t -> bool
  (** [is_false atm] holds iff [atm] represents the [false] atom. *)

val eq : t -> t -> bool
  (** Equality on atoms. *)

val is_negatable : t -> bool
  (** [is_negatable atm] is always true. Not deleted,
    because it is called by SAT solver *)

val is_disjoint : t -> t -> bool

val negate : (Term.t -> Term.t) -> t -> t
  (** [negate f atm] ... *)
  
val vars_of : t -> Term.Set.t
  (** [vars_of atm] collects all variables in [atm]. *)

val status : t -> Term.status

val is_connected : t -> t -> bool
  (** [is_connected atm1 atm2] holds iff [vars_of atm1] and
    [vars_of atm2] not disjoint. *)

val pp : t Pretty.printer
  (** Pretty-printing an atom. *)

val to_string : t -> string
  (** Pretty-printing an atom to a string. *)

module Set : (Sets.S with type elt = t)
  (** Sets of atoms. *)

module Map : (Maps.S with type key = t)
  (** Maplets with atoms as keys. *)
