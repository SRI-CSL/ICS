
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


type t =
  | True
  | Equal of Term.Equal.t
  | Diseq of Term.Diseq.t
  | Nonneg of Term.Nonneg.t
  | Pos of Term.Pos.t
  | False


val cmp : t -> t -> int
val eq : t -> t -> bool


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

val vars_of : t -> Term.Set.t

val is_connected : t -> t -> bool


(** {6 Pretty-printing} *)

val pp : t Pretty.printer


(** {6 Set of atoms} *)

module Set : (Set.S with type elt = t)
