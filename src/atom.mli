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

  @author Harald Ruess

  An atomic predicate is either 
  - one of the constants [True], [False],
  - an equality, 
  - a disequality, or 
  - an arithmetic inequality.

  For each of these kinds of atoms there is a module for constructing,
  destructing, displaying, and manipulating constraints.

  For each atom, a {i unique index} is maintained. That is, {!Atom.eq}[ a b]
  holds iff [i = j] with [i], [j] the indices associated with [a], [b], respectively.
*)

val crossmultiply : bool ref
  (** Enable/Disable crossmultiplication. *)

(** {6 Equalities} *)

module Equal : sig
  type t
    (** Representation of ordered equalities [a = b]; that is, [a <<< b] according
      to the term ordering {!Term.(<<<)}. *)
  val lhs : t -> Term.t
    (** [lhs e] returns [a] if [e] represents [a = b]. *)
  val rhs : t -> Term.t
    (** [rhs e] returns [b] if [e] represents [a = b]. *)
  val pp : t Pretty.printer
    (** Pretty-printing equality constaints. *)
  val make : Term.t * Term.t -> t
  val destruct : t -> Term.t * Term.t
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool
  val is_pure : Th.t -> t -> bool
  val is_diophantine : t -> bool
  val holds : t -> Three.t
  val eq : t -> t -> bool
  val map2 : (t -> 'c) * (t -> 'a -> 'b -> 'c) -> 'a Term.transformer * 'b Term.transformer -> t -> t * 'c
  val map : (t -> 'b) * (t -> 'a -> 'a -> 'b) -> 'a Term.transformer -> t -> t * 'b
end 
              

(** {6 Disequalities} *)

module Diseq : sig
  type t
  val lhs : t -> Term.t
  val rhs : t -> Term.t
  val make : Term.t * Term.t -> t
  val destruct : t -> Term.t * Term.t
  val pp : t Pretty.printer
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool
  val is_diophantine : t -> bool
  val d_diophantine : t -> Term.t * Mpa.Q.t
  val holds : t -> Three.t
  val map : (t -> 'b) * (t -> 'a -> 'a -> 'b) -> 'a Term.transformer -> t -> t * 'b
  val compare : t -> t -> int
end 
      
  
(** {6 Nonnegativity Constrains} *)

module Nonneg : sig
  type t
  val pp : t Pretty.printer
  val make : Term.t -> t
  val destruct : t -> Term.t
  val holds : Term.t -> Three.t
  val map : (t -> 'b) * (t -> 'a -> 'b) -> 'a Term.transformer -> t  -> t * 'b
end 


(** {6 Positive Constraints} *)

module Pos : sig
  type t
  val pp : t Pretty.printer
  val make : Term.t -> t
  val destruct : t -> Term.t
  val holds : Term.t -> Three.t
  val map : (t -> 'b) * (t -> 'a -> 'b) -> 'a Term.transformer -> t -> t * 'b
end 


(** {6 Atoms} *)

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
val mk_neg : Term.t -> t
val mk_nonpos : Term.t -> t
val mk_le : Term.t * Term.t -> t
val mk_lt : Term.t * Term.t -> t
val mk_ge : Term.t * Term.t -> t
val mk_gt : Term.t * Term.t -> t


val of_equal : Equal.t -> t
val of_diseq : Diseq.t -> t
val of_nonneg : Nonneg.t -> t
val of_pos : Pos.t -> t

(** {6 Recognizers} *)

val is_true : t -> bool
val is_false : t -> bool

val is_pure : Th.t -> t -> bool


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
