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

(** Atomic predicates.

  An atomic predicate is either the 
  - constant [True] or [False] predicate,
  - an equality, 
  - disequality, or 
  - an arithmetic constraint.

  @author Harald Ruess
*)

type atom =
  | True
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | False

type t


val atom_of : t -> atom
val index_of : t -> int

val of_atom : atom -> t
val of_index : int -> t


(** {6 Constructors} *)

val mk_true : t

val mk_false : t

val mk_equal : Term.t * Term.t -> t
val mk_diseq : Term.t * Term.t -> t
val mk_nonneg : Term.t -> t
val mk_pos : Term.t -> t
val mk_le : Term.t * Term.t -> t
val mk_lt : Term.t * Term.t -> t
val mk_ge : Term.t * Term.t -> t
val mk_gt : Term.t * Term.t -> t



(** {6 Recognizers} *)

val is_true : t -> bool
val is_false : t -> bool


(** {6 Mapping over atoms} *)

val apply : (Term.t * Term.t) list -> t -> t


(** {6 Negations of atoms} *)

val is_negatable : t -> bool

val negate : t -> t

(** {6 Accessors} *)

val vars_of : t -> Term.Var.Set.t

val is_connected : t -> t -> bool


(** {6 Pretty-printing} *)

val pp : t Pretty.printer

val to_string : t -> string


(** {6 Sets and Maplets} *)

module Set : (Set.S with type elt = t)

module Map : (Map.S with type key = t)
