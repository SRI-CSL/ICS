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


val print_justification : bool ref
  (** {!Fact.pp} prints justification only if this flag is set to [true]. *)

val pp_justification : Jst.t Pretty.printer

(** {6 Facts} *)

type t = Atom.t * Jst.t

val mk_axiom : Atom.t -> t

val mk_holds : Atom.t -> t

val pp : t Pretty.printer

val map : Jst.Rel2.t * Jst.Rel1.t * Jst.Rel1.t -> Jst.Eqtrans.t -> Atom.t -> t
  (** [map (is_equal , is_nonneg, is_pos) f atm] replaces terms [a]
   in atom [atm] with [f a] to obtain a simplified atom [atm'].
    The predicates [is_equal], [is_nonneg], [is_pos] are used to
      simplify the result. The second result is a justification [rho]
     with [rho |- atm <=> atm']. *)


(** {6 Equality Facts} *)

module Equal : sig
  type t
  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t
  val pp : t Pretty.printer
  val make : Term.t * Term.t * Jst.t -> t
  val make_inorder : Term.t * Term.t * Jst.t -> t
  val of_equal : Atom.Equal.t * Jst.t -> t
  val destruct : t -> Term.t * Term.t * Jst.t
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool
  val is_pure : Th.t -> t -> bool
  val theory_of : t -> Th.t option
  val is_diophantine : t -> bool
  val map2 : Jst.Eqtrans.t * Jst.Eqtrans.t -> t -> t
  val map : Jst.Eqtrans.t -> t -> t
  val map_lhs : Jst.Eqtrans.t -> t -> t
  val map_rhs : Jst.Eqtrans.t -> t -> t
  val holds : t -> Jst.Three.t
  val equiv : (Term.t * Term.t -> Term.t * Term.t) -> t -> t
    (** If [f] transforms equalities [a = b] to equivalent 
      equalities [a' = b'] in theory [th], then [inj th f]
      is the corresponding equality constraint transformer. *)
  val equivn : (Term.t * Term.t -> (Term.t * Term.t) list) -> t -> t list

  val norm : Term.map -> t list -> Jst.Eqtrans.t
end
              

(** {6 Disequality Facts} *)

module Diseq : sig
  type t
  val make : Term.t * Term.t * Jst.t -> t
  val of_diseq : Atom.Diseq.t * Jst.t -> t
  val destruct : t -> Term.t * Term.t * Jst.t
  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t
  val pp : t Pretty.printer
  val map : Jst.Eqtrans.t -> t -> t
  val to_var : (Th.t -> Jst.Eqtrans.t) -> t -> t
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool
  val is_diophantine : t -> bool
  val d_diophantine : t -> Term.t * Mpa.Q.t * Jst.t
  module Set : (Set.S with type elt = t)            
end 
      
  
(** {6 Nonnegative Constraint Facts} *)

module Nonneg : sig
  type t
  val pp : t Pretty.printer
  val make : Term.t * Jst.t -> t
  val destruct : t -> Term.t * Jst.t
  val map : Jst.Eqtrans.t -> t -> t        
end 


(** {6 Positive Constraint Facts} *)

module Pos : sig
  type t
  val pp : t Pretty.printer
  val make : Term.t * Jst.t -> t
  val destruct : t -> Term.t * Jst.t
  val map : Jst.Eqtrans.t -> t -> t          
end 
              


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
module Nonnegs : (STACK with type t = Nonneg.t)


val with_disabled_stacks : ('a -> 'b) -> 'a -> 'b
  (** [with_disabled_stacks f a] applies [f] to [a] in
    which pushing on stacks is disabled and popping any
    of the stacks in such a context may have catastrophic
    consequences. *)
