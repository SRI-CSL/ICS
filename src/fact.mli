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

(** Datatype of facts (justified atoms)

  @author Harald Ruess
*)


type t = Atom.t * Jst.t
    (** A {b fact} is an atom together with a justification (proof) of this
      atom in terms of axioms. *)

val pp : t Pretty.printer
  (** Pretty-printing the atom of a justification. In case {!Fact.print_justification} 
    is set, this also prints the justification. *)

val print_justification : bool ref
  (** {!Fact.pp} prints justification only if this flag is set to [true]. *)

val eq : t -> t -> bool

val map : Jst.Eqtrans.t -> t -> t
val replace : Term.t * Term.t * Jst.t -> t -> t


(** Equality Facts *)
module Equal : sig
  type t = Term.t * Term.t * Jst.t
  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t
  val pp : t Pretty.printer
  val make : Term.t -> Term.t -> Jst.t -> t
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool
  val is_pure : Th.t -> t -> bool
  val status : t -> Term.status
  val map2 : Jst.Eqtrans.t * Jst.Eqtrans.t -> t -> t
  val map : Jst.Eqtrans.t -> t -> t
  val map_lhs : Jst.Eqtrans.t -> t -> t
  val map_rhs : Jst.Eqtrans.t -> t -> t
end
              

(** Disequality Facts *)
module Diseq : sig
  type t = Term.t * Term.t * Jst.t
  val make : Term.t -> Term.t -> Jst.t -> t
  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t
  val pp : t Pretty.printer
  val map : Jst.Eqtrans.t -> t -> t
  val both_sides : (Term.t -> bool) -> t -> bool
  val is_var : t -> bool  
  val is_pure : Th.t -> t -> bool
  val status : t -> Term.status
  module Set : (Set.S with type elt = t)            
end


(** Nonnegativity facts *)
module Nonneg : sig
  type t = Term.t * Jst.t
  val make : Term.t -> Jst.t -> t
  val term_of : t -> Term.t
  val pp : t Pretty.printer
  val map : Jst.Eqtrans.t -> t -> t
  val is_var : t -> bool  
  val is_pure : Th.t -> t -> bool
  val status : t -> Term.status
end

(** Nonnegativity facts *)
module Pos : sig
  type t = Term.t * Jst.t
  val make : Term.t -> Jst.t -> t
  val term_of : t -> Term.t
  val pp : t Pretty.printer
  val map : Jst.Eqtrans.t -> t -> t
  val is_var : t -> bool  
  val is_pure : Th.t -> t -> bool
  val status : t -> Term.status
end


val of_equal : Equal.t -> t
val of_diseq : Diseq.t -> t
