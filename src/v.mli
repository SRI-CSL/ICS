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

(** Context for handling equivalence classes of variables.

  @author Harald Ruess
  @author N. Shankar
*)

type t
  (** Elements of [t] represent a set of variable equalities.
    These equalities induce an equivalence relation. We say
    that [x] and [y] are equivalent modulo [s], if they are
    in the same equivalence class induced by [s]. *)


(** {6 Recognizers} *)

val is_equal : t -> Term.t -> Term.t -> bool
  (** [is_equal s x y] holds if and only if [x] and [y] are
    in the same equivalence class modulo [s]. *)



(** {6 Accessors} *)

val partition : t -> Term.Set.t Term.Map.t
  (** [partition s] returns a partitioning of the set of variables
    in the form of a map with a domain consisting of canonical
    representatives and the corresponding equivalence class in
    the codomain. It does only list non-singleton equivalence classes. *)

val find : t -> Term.t -> Term.t
  (** [find s x] returns the canonical representative of [x]
    of the equivalence class in [s] containing [x]. The canonical
    representative is the smallest variable in this class according
    to the variable ordering {!Var.cmp}. *)
  
val find' : t -> Term.t -> t * Term.t
  (** In addition to [find],  [find'] performs dynamic path compression as
    a side effect. *)
  
val justification : t -> Term.t -> Term.t * Fact.justification option
  (** [justification s x] returns [find s x] together with a justification
    for the equalty [x = find s x]. *)
  
val equality : t -> Term.t -> Fact.equal
  (** [equality s x] returns an equality [e] of the form [x = find s x]. *)



(** {6 Manipulating contexts} *)

val empty : t
  (** The empty variable context. *)

val merge : Fact.equal -> t -> t
  (** Adding a variable equality [x = y] to a context [s].
    As a side effect, [x] is added to the global variable {!V.changed}.
    In addition, every non-external variable [v] which is canoncal in
    [s] but not in [merge e s], is added to {!V.removable}. *)

val restrict : Term.t -> t -> t
  (** [restrict x s] removes occurrences of [x] from [s].
    Should only be called for [x] in [removable s]. *)

val changed : Term.Set.t ref
  (** The set of changed [x] in domain. *)

val removable : Term.Set.t ref
  (** Set of removable variables. In particular, all variables in
    this set are internal variables. *)


(** {6 Equality} *)

val eq : t -> t -> bool
  (** [eq s t] holds iff [s] and [t] are physically equal. Notice that
    [eq s t] equals [false] does not imply that these contexts are not
    logically equivalent. *)



(** {6 Pretty-printing} *)

val pp : t Pretty.printer


(** {6 Iterators} *)


val fold : t -> (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** Folding over the members of a specific equivalence class. *)

val iter : t -> (Term.t -> unit) -> Term.t -> unit
  (** Iterate over the extension of an equivalence class. *)

val exists : t -> (Term.t -> bool) -> Term.t -> bool
  (** [exists s p x] holds if [p y] holds for some [y] congruent
    to [x] modulo [s]. *)

val for_all : t -> (Term.t -> bool) -> Term.t -> bool
  (** [for_all s p x] holds if [p y] holds for all [y] congruent
    to [x] modulo [s]. *)

val choose : t -> (Term.t -> 'a option) -> Term.t -> 'a
  (** [choose s p x] chooses a [y] which is congruent to [x] modulo [s]
    which satisfies [p]. If there is no such [y], the exception [Not_found]
    is raised. *)

