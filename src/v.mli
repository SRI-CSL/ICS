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
  (** Elements of [t] represent conjunctions of variable equalities.
    These equalities induce an equivalence relation. We say
    that [x] and [y] are equivalent modulo [s], if the equality
    [x = y] follows from the equalities in [s] by equality reasoning. *)

val eq : t -> t -> bool
  (** [eq s t] holds iff [s] and [t] are identical. Notice that
    [eq s t] equals [false] does not imply that these contexts are not
    logically equivalent. *)

val pp : t Pretty.printer
  (** Pretty-printing *)

val find : t -> Jst.Eqtrans.t
  (** [find s x] returns the canonical representative of [x]
    of the equivalence class in [s] containing [x] together
    with a justification of the equality [find s x = x].  The canonical
    representative is the smallest variable in this class according
    to the variable ordering {!Var.cmp}. For nonvariable terms [a], 
    [find s a] returns [a] *)

val removable : t -> Term.Var.Set.t
  (** Set of removable variables. All variables in [removable s] 
    are {i internal}, noncanonical variables. *)

val is_equal : t -> Term.t -> Term.t -> Jst.t option
  (** For variables [x], y[], [is_equal s x y] holds if and only 
    if [x] and [y] are in the same equivalence class modulo [s]. *)

val is_canonical : t -> Term.t -> bool
  (** For a term variable [x], [is_canonical s x] holds 
    iff [find s x] returns [x]. *)

val empty : t
  (** The empty variable context. *)

val merge : Fact.Equal.t -> t -> t
  (** Adding a variable equality [x = y] to a context [s].
    As a side effect, [x] is added to the global variable {!V.changed}.
    In addition, every non-external variable [v] which is canoncal in
    [s] but not in [merge e s], is added to {removabl se}. *)

val gc : (Term.t -> bool) -> t -> t
  (** [gc filter s] removes variables [x] in [removable s],
    if the test [filter x] succeeds. Only, if {!V.garbage_collection_enabled}
    is set to [true]. *)

val garbage_collection_enabled : bool ref
  (** Switch for enabling/disabling garbage collection of noncanonical, 
    internal variables. *)

val fold : t -> (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** Folding over the members of a specific equivalence class.
    That is, if [{x1,...,xn}] is the set of variables with
    [find s xi] equals the variable [x'], then [fold s f x e]
    reduces to [f x1 (f x2 ... (f xn e)...)] if [find s x] is [x']. 
    The order of application is unspecified. *)

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




