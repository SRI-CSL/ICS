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

(** Tableau

  @author Harald Ruess
  @author N. Shankar
*)

type t

type config = Partition.t * t

val eq : t -> t -> bool


(** {6 Accessors} *)

val apply : t -> Justification.Eqtrans.t

val find : t -> Justification.Eqtrans.t

val inv : t -> Justification.Eqtrans.t

val dep : t -> Term.t -> Term.Set.t

val can : config -> Justification.Eqtrans.t

val is_dependent : t -> Term.t -> bool

val is_independent : t -> Term.t -> bool


(** {6 Manipulating tableau} *)

val empty : t
  (** Empty tableau. *)

val is_empty : t -> bool

val copy : t -> t

val name : config -> Justification.Eqtrans.t

val process_equal : config -> Fact.Equal.t -> unit

val process_nonneg : config -> Fact.Nonneg.t -> unit

val process_diseq : config -> Fact.Diseq.t -> unit


(** {6 Pretty-printing} *)

val pp : t Pretty.printer

(** {6 Maximization} *)

exception Unbounded

val maximize : config -> Justification.Eqtrans.t
  (** [max s a] returns either
    - [(b, rho)] such that [b+] is empty and [rho |- a = b], or
    - raises [Unbounded] if [a] is unbounded in [s]. *)

val minimize : config -> Justification.Eqtrans.t

val is_nonpos : config -> Justification.Pred.t
  (** [is_nonpos s a] returns [Some(rho)] if [a <= 0] holds in [s]. 
    In this case [rho |- a <= 0]. Otherwise, [None] is returned. *)

val is_nonneg : config -> Justification.Pred.t

val is_pos : config -> Justification.Pred.t
val is_neg : config -> Justification.Pred.t


(** {6 Finite Interpretations} *)

module Finite : sig

  module Zset : (Set.S with type elt = Mpa.Z.t)

  (** Finite domain interpretation with lower bound [lo],
    upper bound [hi], and a disequality set [diseqs]. The
    corresponding interpretation domain [D(fin)] is defined
    as [{z in int | lo <= z <= hi & x notin diseqs }]. *) 
  type t = {
    lo : Mpa.Z.t;
    hi : Mpa.Z.t;
    diseqs : Zset.t
  }

  val pp : t Pretty.printer

  val of_var : config -> Term.t -> t
  (** [of_var (p, s) x] returns either a finite domain
    interpretation [fin] for [x] such that [x] is
    interpreted in [D(fin)] or raises [Unbounded]. *)

  val of_config : config -> t Term.Map.t
  (** [of_config (p, s)] returns a map of finite domain
    interpretations for all variables in [s] with a 
    finite interpretation. *)
	
end 



(** {6 Interpretations} *)

val model : config -> Term.Set.t -> Term.t Term.Map.t
