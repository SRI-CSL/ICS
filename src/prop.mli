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

(** Propositional logic

  @author Harald Ruess
*)

type t =
  | True
  | False
  | Var of Name.t
  | Atom of Atom.t
  | Disj of t list
  | Iff of t * t
  | Ite of t * t * t
  | Neg of t 
  | Let of Name.t * t * t

(** {6 Constructors} *)
 
val mk_true : t
val mk_false : t
val mk_var : Name.t -> t
val mk_poslit : Atom.t -> t
val mk_neglit : Atom.t -> t
val mk_ite : t -> t -> t -> t
val mk_conj : t list -> t
val mk_disj : t list -> t
val mk_iff : t -> t -> t
val mk_neg : t -> t
val mk_let : Name.t -> t -> t -> t


(** {6 Satisfiability checker} *)

(** Parameter settings for SAT solver *)

val set_verbose : bool -> unit
val set_remove_subsumed_clauses : bool -> unit
val set_validate_counter_example : bool -> unit
val set_polarity_optimization : bool -> unit
val set_clause_relevance : int -> unit
val set_cleanup_period : int -> unit

val sat : Context.t -> t -> Atom.Set.t option
