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


(** {6 Accessors} *)

val apply : t -> Term.t -> Term.t * Fact.justification option

val find : t -> Term.t -> Term.t * Fact.justification option
  (** [find s x] returns the canonical representative of [x]
    of the equivalence class in [s] containing [x]. The canonical
    representative is the smallest variable in this class according
    to the variable ordering {!Var.cmp}. *)
  
val equality : t -> Term.t -> Fact.equal
  (** [equality s x] returns an equality [e] between [x] and [find s x]. *)


(** {6 Recognizers} *)

val is_equal : t -> Term.t -> Term.t -> bool
  (** [is_equal s x y] holds if and only if [x] and [y] are
    in the same equivalence class modulo [s]. *)


(** {6 Manipulating contexts} *)

val empty : t
  (** The empty variable context. *)

val merge : Fact.equal -> t -> Term.Set.t * t
  (** Adding a variable equality [x = y] to a context [s].
    As a side effect, [x] is added to the global variable {!V.changed}.
    In addition, every non-external variable [v] which is canoncal in
    [s] but not in [merge e s], is added to {removabl se}. *)

val gc : (Term.t -> bool) -> t -> t
  (** [gc filter s] removes variables [x] in [removable s],
    if the test [filter x] succeeds. *)

val removable : t -> Term.Set.t
  (** Set of removable variables. All variables in
    [removable s] are internal, noncanonical variables. *)


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

