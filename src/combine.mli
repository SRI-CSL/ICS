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

(** {b Shostak theories}.

  @author Harald Ruess
  @author N. Shankar
*)

type t

val pp : Th.t -> t Pretty.printer

val eq : t -> t -> bool

val empty : t


val is_empty : t -> Th.t -> bool

val is_dependent : t -> Th.t -> Term.t -> bool
  (** [is_dependent th s x] iff [x = _] is in the solution set for 
    theory [th] in [s]. *)


type config = Partition.t * t


val apply : t -> Th.t -> Justification.Eqtrans.t

val find : t -> Th.t -> Justification.Eqtrans.t

val interp :  config -> Th.t -> Justification.Eqtrans.t

val inv : config -> Justification.Eqtrans.t

val dep : t -> Th.t -> Term.t -> Term.Set.t


(** {6 Process} *)

val copy : t -> t

val name : config -> Th.t -> Justification.Eqtrans.t

val process_equal :  config -> Th.t -> Fact.Equal.t -> unit

val process_nonneg : config -> Fact.Nonneg.t -> unit

val process_diseq : config -> Fact.Diseq.t -> unit

val merge :  config -> Th.t -> Fact.Equal.t -> unit

val dismerge :  config -> Th.t -> Fact.Diseq.t -> unit

val propagate : config -> Fact.Equal.t -> Th.t * Th.t -> unit
  (** [propagate s e (i, j)] propagates an equality over
    [i] terms to the solution set for theory [j]. *)


(** {6 Theory-specific operations} *)

val sigma : config -> Sym.t -> Term.t list -> Term.t * Justification.t

val can : config -> Justification.Eqtrans.t

val solve : Th.t -> Term.Equal.t -> Term.Subst.t

(** {6 Predicates} *)

val is_equal : config -> Justification.Rel2.t

val is_nonpos : config -> Justification.Rel1.t
  (** [is_nonpos s a] returns [Some(rho)] if [a <= 0] holds in [s]. 
    In this case [rho |- a <= 0]. Otherwise, [None] is returned. *)

val is_pos : config -> Justification.Rel1.t

val is_nonneg : config -> Justification.Rel1.t

val is_neg : config -> Justification.Rel1.t


val maximize : config -> Justification.Eqtrans.t

val minimize : config -> Justification.Eqtrans.t


(** {6 Model construction} *)

val model : Partition.t * t -> Term.Set.t -> Term.t Term.Map.t


(** {6 Splits} *)

module Split : sig

  type t = {
    finint: La.Finite.t Term.Map.t;
    arridx: Term.Set2.t
  }

  val pp : t Pretty.printer

  val is_empty : t -> bool

end 

val split : config -> Split.t
