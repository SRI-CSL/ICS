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

(** A {b fact} is either 
  - an equality [a = b] between terms [a] and [b], 
  - a disequality [a <> b] between terms [a], [b], or 
  - a membership constraint of the form [a in c], where [a] is a term and [c]
    is a constraint of type {!Cnstrnt.t}.
  In addition, every fact includes an optional {b justification} in terms
  of facts sufficient to prove the fact at hand.

  @author Harald Ruess
*)


(** {6 Global variables} *)

val footprint : bool ref
  (** If [footprint] is set to [true], then instantiations of [S.make s] have the
    side effect of printing [s] on {!Fact.fmt} (default {!Format.err_formatter}) 
    using [S.pp]. *)

val fmt : Format.formatter ref
  (** Output channel for footprints (see also {!Fact.footprint}); it is initialized
    with {!Format.err_formatter}. *)

val print_justification : bool ref
  (** {!Fact.pp} prints justification only if this flag is set to [true]. *)


(** {6 Facts} *)

type t = Atom.t * Justification.t
 
val pp : t Pretty.printer

val atom_of : t -> Atom.t



(** {6 Recognizers} *)

val is_true : t -> bool

val is_false : t -> bool


(** {6 Constructors} *)

val mk_axiom : Atom.t -> t
  (** [mk_axiom a] justifies atom [a]. *)


(** {6 Equality Facts} *)

module Equal : sig

  type t

  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t

  val pp : t Pretty.printer

  val make : Term.t * Term.t * Justification.t -> t

  val destruct : t -> Term.t * Term.t * Justification.t

  val is_var : t -> bool

  val is_pure : Th.t -> t -> bool

  val map : Justification.Eqtrans.t -> t -> t

  val map_lhs : Justification.Eqtrans.t -> t -> t

  val map_rhs : Justification.Eqtrans.t -> t -> t

  val map2 : Justification.Eqtrans.t * Justification.Eqtrans.t -> t -> t

  module Inj : sig

    val apply1 : Term.apply -> t -> Justification.Eqtrans.t

    val trans : (Term.Equal.t -> Term.Equal.t) -> t -> t

    val solver : (Term.Equal.t -> Term.Equal.t list) -> t -> t list 

    val norm : Term.apply -> t list -> Justification.Eqtrans.t

    val replace : Term.map -> Justification.Eqtrans.t -> Justification.Eqtrans.t

    val mapargs : (Sym.t -> Term.t list -> Term.t * Justification.t) 
                     -> (Sym.t -> Justification.Eqtrans.t) -> Justification.Eqtrans.t
      (* [mapargs app f a] maps [f op] over the arguments [al] of
	 an application [a] of the form [op(al)]. If [a] is not
	 an application, [Not_found] is raised. *)

    val mapl : Justification.Eqtrans.t -> Term.t list -> Term.t list * Justification.t list
  end

end 
              

(** {6 Disequality Facts} *)

module Diseq : sig

  type t

  val make : Term.t * Term.t * Justification.t -> t

  val destruct : t -> Term.t * Term.t * Justification.t

  val pp : t Pretty.printer

  val map : Justification.Eqtrans.t -> t -> t

  val is_var : t -> bool

  val is_diophantine : t -> bool

  val d_diophantine : t -> Term.t * Mpa.Q.t * Justification.t


  module Set : (Set.S with type elt = t)
              
end 
      
  

(** {6 Nonnegative Constraint Facts} *)

module Nonneg : sig

  type t

  val pp : t Pretty.printer

  val make : Term.t * Justification.t -> t
  val destruct : t -> Term.t * Justification.t

  val holds : t -> Justification.Three.t

  val map : Justification.Eqtrans.t -> t -> t
              
end 


(** {6 Positive Constraint Facts} *)

module Pos : sig

  type t

  val pp : t Pretty.printer

  val make : Term.t * Justification.t -> t
  val destruct : t -> Term.t * Justification.t

  val map : Justification.Eqtrans.t -> t -> t
              
end 
              

(** {6 Constructors} *)

val mk_true : Justification.t -> t
val mk_false : Justification.t -> t
val mk_equal : Justification.Rel2.t -> Term.t * Term.t * Justification.t -> t
val mk_diseq : Justification.Rel2.t -> Term.t * Term.t * Justification.t -> t
val mk_nonneg : Justification.Rel1.t -> Term.t * Justification.t -> t
val mk_pos : Justification.Rel1.t ->  Term.t * Justification.t -> t


val map : (Justification.Rel2.t *                      (* [is_equal] *)
           Justification.Rel1.t *                      (* [is_nonneg] *)
           Justification.Rel1.t)                       (* [is_pos] *)
             -> Justification.Eqtrans.t -> t -> t


(** {6 Set of facts} *)

module Set : (Set.S with type elt = t)

 
(** {6 Stack of facts} *)

module type STACK = sig
  type t
  val clear : unit -> unit
  val push : Th.t option -> t -> unit
  val pop : unit -> Th.t option * t
  val is_empty : unit -> bool
end

module Eqs : (STACK with type t = Equal.t)
module Diseqs : (STACK with type t = Diseq.t)

val with_disabled_stacks : ('a -> 'b) -> 'a -> 'b
  (** [with_disabled_stacks f a] applies [f] to [a] in
    which pushing on stacks is disabled and popping any
    of the stacks in such a context may have catastrophic
    consequences. *)
