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

(** Datatype of facts (justified atoms)

  @author Harald Ruess

  A {b fact} is either 
  - an equality [a = b] between terms [a] and [b], 
  - a disequality [a <> b] between terms [a], [b], or 
  - a membership constraint of the form [a in c], where [a] is a term and [c]
    is a constraint of type {!Cnstrnt.t}.
  In addition, every fact includes an optional {b justification} in terms
  of facts sufficient to prove the fact at hand.

*)


val print_justification : bool ref
  (** {!Fact.pp} prints justification only if this flag is set to [true]. *)

val pp_justification : Jst.t Pretty.printer


(** Equality Facts *)
module Equal : sig
  type t
  val lhs_of : t -> Term.t
  val rhs_of : t -> Term.t
  val pp : t Pretty.printer
  val make : Term.t * Term.t * Jst.t -> t
  val of_equal : Atom.Equal.t * Jst.t -> t
  val destruct : t -> Term.t * Term.t * Jst.t
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
  type t
  val make : Term.t * Term.t * Jst.t -> t
  val of_diseq : Atom.Diseq.t * Jst.t -> t
  val destruct : t -> Term.t * Term.t * Jst.t
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


(** {6 Facts} *)
type t = 
  | Equal of Equal.t
  | Diseq of Diseq.t

type fact = t

val pp : t Pretty.printer

val of_equal : Equal.t -> t
val of_diseq : Diseq.t -> t


(** Input facts *)
module Input : sig

  type t

  val empty : t

  val is_empty : t -> bool

  val eq : t -> t -> bool

  val pp: t Pretty.printer

  val instantiate : Equal.t -> t -> t
    (** [instantiate e g] with [e] of the form [a = b]
      replaces occurrences of [b] in [g] with [a]. *)

  module Equal : sig
    val is_empty : t -> bool
    val add : t -> Equal.t -> t
    val choose : t -> Equal.t * t
  end 

  module Diseq : sig
    val is_empty : t -> bool
    val add : t -> Diseq.t -> t
    val choose : t -> Diseq.t * t
  end

  val add : t -> fact -> t

  val copy : t -> t

end 
