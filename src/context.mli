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

(** Logical contexts.

  @author Harald Ruess

  A {b logical context} is a conjunction [ctxt] of atoms (see module {!Atom}). 
  This module  provides functionality
  - for adding atoms to a context,
  - for generating models of satisfiable contexts, and
  - for generating {i small} (but not necessarily minimal) inconsistent 
  subsets of unsatisfiable contexts.
*)

type t
  (** Representation of a logical context. *)

val pp : t Pretty.printer
  (** Pretty-printing the context of a state. *)

val eq : t -> t -> bool
  (** Constant time identity test on contexts. Failure does not imply 
    that contexts are not logically equivalent. *)

val ctxt : t -> Atom.t list
  (** Returns the associated logical context. *)

val config : t -> Combine.Config.t
  (** [config ctxt] returns the configuration of the inference obtained by processing [ctxt].
    This configuration is not necessarily {i irreducable}. *)

(** Status flag for logical contexts. *)
module Status : sig
  type t =
    | Sat of Term.Model.t
    | Unsat of Judgement.unsat
    | Unknown

  val pp: Format.formatter -> t -> unit
end

val status : t -> Status.t
  (** The {i status} associated with a context [ctxt] is either
    - [Sat(mdl)], in which case [mdl] is a term model (see {!Term.Model.t}) 
    of the atoms in [ctxt],
    - [Unknown], indicating that the status of satisfiablity of [ctxt] has 
    not been established, or
    - [Unsat(jst)] with justications [jst] (see {!Jst.t}) an unsatisfiable 
    subset of [ctxt]. *)
  
val empty : t
  (** The empty logical context *)

val add : t -> Atom.t -> t
  (** [add ctxt a] returns a logical context for the conjunction of [ctxt] and the 
    atom [a]. Notice that the [status] of the resulting context might not have been 
    resolved, that is, the status might be [Unknown]. *)

val addl : t -> Atom.t list -> t
  
val resolve : t -> unit
  (** [complete ctxt] resolves the status to either [Sat] or [Unsat]. *)

val model : t -> Term.Model.t
  (** [model ctxt] 
    - returns [Some(i, alpha)] with term model [i, alpha], with [dom(i, alpha)] a subset of the
    variables in [ctxt],  validating all atoms [a] in [ctxt], or
    - raises {!Jst.Inconsistent} together with an inconsistent subset [jst] of [ctxt]. *)

val eval : t -> Atom.t -> Atom.t
  (** [eval ctxt atm] computes the value of [atm] for the model associated with
    a satisfiable [ctxt]. *)

val validates : t -> Atom.t -> bool
  (** [validates ctxt atm] holds if [ctxt] is unsatisfiable or if the model associated 
    with [ctxt] validates [atm]. *)
  
val is_inconsistent : t -> Atom.t list -> Judgement.atom option
  (** [is_inconsistent ctxt al] returns
    - [Some(jst)], for [jst] an inconsistent subset of the union of [ctxt] and [al],
    if the conjunction of [ctxt] and the atoms in [al] is inconsistent, or
    - [None] if the conjunction of [ctxt] and [al] is consistent. *)

val is_valid : t -> Atom.t -> Judgement.atom option
  (** [is_valid ctxt a] returns
    - [Some(jst)] for [jst] a subset of [ctxt] which implies atom [a], or
    - [None] if [ctxt] does not imply [a]. *)
