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
  - an arithmetic inequality [a > 0] or [a >= 0]. *)
type atom =
  | TT
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | FF

type t
  (** For each atom, a {i unique index} is maintained. That is, 
    - {!Atom.equal}[a b] holds iff [i = j] 
    with [i], [j] the indices associated with [a], [b], respectively. *)

val atom_of : t -> atom
  (** Retrieve the atom from an atom-index pair. *)

val index_of : t -> int
  (** Retrieve the unique index from an atom-index pair. *)

val of_atom : atom -> t
  (** Construct an atom-index pair with unique index from an atom. *)

val of_index : int -> t
  (** [of_index n] returns an atom-index pair of index [n] if such and
    atom-index pair has been created since the last {!Tools.do_at_reset}. 
    Otherwise, the result is undefined. *)

val mk_true : t
  (** Atom-index pair for representing the [true] atom. *)

val mk_false : t
  (** Atom-index pair for representing the [false] atom. *)

val mk_equal : Term.t * Term.t -> t
  (** The atom-index pair [mk_equal (a, b)] represents the equality [a = b]. *)

val mk_diseq : Term.t * Term.t -> t
  (** The atom-index pair [mk_diseq (a, b)] represents the disequality [a <> b]. *)

val mk_nonneg : Term.t -> t
  (** The atom-index pair [mk_nonneg a] represents the nonnegativity 
    constraint [a >= 0]. *)

val mk_pos : Term.t -> t
  (** The atom-index pair [mk_nonneg a] represents the positivity
    constraint [a > 0]. *)

val is_true : t -> bool
  (** [is_true atm] holds iff [atm] represents the [true] atom. *)

val is_false : t -> bool
  (** [is_false atm] holds iff [atm] represents the [false] atom. *)

val equal : t -> t -> bool
  (** Equality on atoms. *)

val is_negatable : t -> bool
  (** [is_negatable atm] is always true. Not deleted,
    because it is called by SAT solver *)

val negate : (Term.t -> Term.t) -> t -> t
  (** [negate f atm] ... *)
  
val vars_of : t -> Term.Var.Set.t
  (** [vars_of atm] collects all variables in [atm]. *)

val is_connected : t -> t -> bool
  (** [is_connected atm1 atm2] holds iff [vars_of atm1] and
    [vars_of atm2] not disjoint. *)

val pp : t Pretty.printer
  (** Pretty-printing an atom. *)

val to_string : t -> string
  (** Pretty-printing an atom to a string. *)

module Set : (Set.S with type elt = t)
  (** Sets of atoms. *)

module Map : (Map.S with type key = t)
  (** Maplets with atoms as keys. *)
