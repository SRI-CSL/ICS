(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
