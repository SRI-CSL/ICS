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

(** Logical context for constraints.

  @author Harald Ruess
*)

type t 
  (** A constraint context consists of a conjunction of constraints
    of the form [x in i], where [x] is a term variable and [i] is
    a constraint of type {!Cnstrnt.t}. *)
  

(** {6 Accessors} *)

val cnstrnts : t -> (Cnstrnt.t * Fact.justification option) Var.Map.t
  (** [cnstrnts s] returns a map with bindings [x |-> (i, j)] iff
    [x in i] is stored in [s] with justification [j]. *)

val apply : t -> Term.t -> Cnstrnt.t
  (** [apply s x] returns [i] if [x in i] is in [s]. Otherwise,
    [Not_found] is raised. *)

val to_fact : t -> Term.t -> Fact.cnstrnt
  (** [to_fact s x] returns a constraint fact [c] for [x in i]
    if [apply s x] equals [i]; otherwise, [Not_found] is raised. *)


(** {6 Predicates} *)

val mem : Term.t -> t -> bool
  (** [mem x s] holds iff [x] is constraint in [s]. *)

val eq : t -> t -> bool
  (** [eq s t] when [s] and [t] are physically equal. *)


(** {6 Context manipulations} *)

val empty : t 
  (** Empty constraint context. *)

val add : Fact.cnstrnt -> t -> t

val merge : Fact.equal -> t -> t
  (** Merge a variable equality [x = y] in the constraint map by
    adding [x in ij] for the canonical variable [x], where [x in i],
    [y in j] are in the constraint map and [ij] is the intersection of
    [i] and [j], and by removing the constraint for [y]. Singleton 
    constraints are always retained in the constraint map in order to 
    keep the invariant that the best available constraint
    are always associated with canonical variables. *)

val diseq : Fact.diseq -> t -> t
  (** Propagate disequalities to the constraint part. *) 

val changed : Term.Set.t ref
  (** This global variable contains all variables [x] for which
    a constraint has been assigned. *)

(** Split. *)

val split : t -> Atom.Set.t

(** {6 Pretty-printing} *)

val pp : t Pretty.printer
