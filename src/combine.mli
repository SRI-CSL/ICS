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

(** {b Equality theories}.

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


val find : t -> Th.t -> Jst.Eqtrans.t
  (** [find th s x] is [a] if [x = a] is in the solution set for theory [th]
    in [s]; otherwise, the result is just [x]. *)

val inv : config -> Jst.Eqtrans.t
  (** [inv s a] is [x] if there is [x = a] in the solution set for
    theory [th]; otherwise [Not_found] is raised. *)

val dep : t -> Th.t -> Term.t -> Term.Var.Set.t
  (** [use th s x] consists of the set of all term variables [y] such
    that [y = a] in [s], and [x] is a variable [a]. *)

val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> Th.t -> 'a -> 'a
  (** [fold f s i e] applies [f x (a, rho)] for each [x = a] in [i]
    with justification [rho] in [s] and accumulates the result starting 
    with [e]. The order of application is unspecified. *)


(** {6 Process} *)

val copy : t -> t

val name : config -> Th.t -> Jst.Eqtrans.t

val abstract : config -> Atom.t -> Fact.t

val process_nonneg : config -> Fact.Nonneg.t -> unit

val process_pos : config -> Fact.Pos.t -> unit

val merge :  Th.t option -> config -> Fact.Equal.t -> unit

val dismerge :  config -> Fact.Diseq.t -> unit

val propagate_equal : config -> Fact.Equal.t -> unit
  (** [propagate_equal s e (i, j)] propagates an equality over
    [i] terms to the solution set for theory [j]. *)

val propagate_diseq : config -> Fact.Diseq.t -> unit

val propagate_nonneg : config -> Fact.Nonneg.t -> unit



(** {6 Theory-specific operations} *)


val dom : config -> Term.t -> Dom.t * Jst.t
(** [dom p a] returns a domain [d] for a term [a] together with
  a justification [rho] such that [rho |- a in d].  This function
  is extended to arithmetic constraints using an abstract domain
  interpretation. Raises [Not_found] if no domain constraint is found. *)
  
val sigma : config -> Sym.t -> Term.t list -> Term.t * Jst.t

val can : config -> Jst.Eqtrans.t

val cheap : bool ref

val simplify : config -> Atom.t -> Fact.t

val solve : Th.t -> Term.Equal.t -> Term.Subst.t

(** {6 Predicates} *)

val cheap : bool ref

val is_equal_or_diseq : config -> Jst.Rel2.t

val is_nonpos : config -> Jst.Rel1.t
  (** [is_nonpos s a] returns [Some(rho)] if [a <= 0] holds in [s]. 
    In this case [rho |- a <= 0]. Otherwise, [None] is returned. *)

val is_pos : config -> Jst.Rel1.t

val is_nonneg : config -> Jst.Rel1.t

val is_neg : config -> Jst.Rel1.t

(** {6 Garbage collection} *)

val gc : config -> unit


val maximize : config -> Jst.Eqtrans.t

val minimize : config -> Jst.Eqtrans.t


(** {6 Model construction} *)

val model : Partition.t * t -> (Term.t * La.mode option) list -> Term.t Term.Map.t


(** {6 Splits} *)

module Split : sig

  type t = 
    | Finint of La.Finite.t
    | Equal of Term.t * Term.t

  val pp : t Pretty.printer

end 

val split : config -> Split.t
