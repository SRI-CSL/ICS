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

(** Decision procedures for propositional sets

  @author Harald Ruess
  @author N. Shankar
*)

type t

val eq : t -> t -> bool
val pp : t Pretty.printer
val empty : t
val is_empty : t -> bool


(** {6 Accessors} *)

val apply : t -> Term.t -> Term.t * Jst.t

val find : t -> Term.t -> Term.t * Jst.t

val inv : t -> Term.t -> Term.t * Jst.t

val dep : t -> Term.t -> Term.Var.Set.t

val is_dependent : t -> Term.t -> bool

val is_independent : t -> Term.t -> bool


(** {6 Iterators} *)

val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s e] applies [f x (a, rho)] for each [x = a] with justification
    [rho] in [s] and accumulates the result starting with [e]. The order of
    application is unspecified. *)


(** {6 Updates} *)

val copy : t -> t

val name : Partition.t * t -> Jst.Eqtrans.t

val merge : Partition.t * t -> Fact.Equal.t -> unit
