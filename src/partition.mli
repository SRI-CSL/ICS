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


(** Variable partitioning.

  A {b variable partition} consists of a
  - set of variable equalities [x = y], 
  - a set of variable disequalities [x <> y].

  @author Harald Ruess
*)


type t
(** Type [t] for representing a partitioning consists of 
  - a set of variable equalities of type {!V.t}
  - a set of variable disequalities of type {!D.t} *)

val v_of : t -> V.t
(** Accessing the variable equalities of a partitioning. *)

val d_of : t -> D.t
(** Accessing the variable disequalities of a partititioning. *)

val eq : t -> t -> bool
(** [eq s t] holds if the respective equality, disequality, and constraint parts
  are identical, that is, stored in the same memory location. *)

val pp : t Pretty.printer
(** Pretty-printing a partitioning. *)

val is_empty : t -> bool

val find : t -> Jst.Eqtrans.t
(** [find s x] returns the canonical representative of the equivalence
  class in the partitioning [s] containing the variable [x]. *)

val diseqs : t -> Term.t -> D.Set.t
(** [diseqs s x] returns the set of all variable [y] disequal to [x] as stored
  in the variable disequality part [d] of the partitioning [s].  Disequalities as
  obtained from the constraint part [c] are not necessarily included. *)

val cnstrnt : t -> Term.t -> Var.Cnstrnt.t * Jst.t
  (** [cnstrnt s x] returns a domain constraint on [x], or raises [Not_found]
    if the interpretation of [x] is unconstrained. *)

val is_canonical : t -> Term.t -> bool

val is_int : t -> Jst.Pred.t

val is_equal : t -> Jst.Pred2.t

val is_diseq : t -> Jst.Pred2.t

val is_equal_or_diseq : t -> Jst.Rel2.t

val choose : t -> Jst.Eqtrans.t -> Jst.Eqtrans.t
  (** [choose p apply x] chooses an [x'] such that [apply x'] does
    not raise [Not_found]. If there is no such [x'], then [Not_found]
    is raised. *)

val iter_if : t -> (Term.t -> unit) -> Term.t -> unit

val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a -> 'a

val empty : t
(** The [empty] partition. *)

val merge : t -> Fact.Equal.t -> unit
(** [merge e s] adds a new variable equality [e] of the form [x = y] into
  the partition [s]. If [x] is already equal to [y] modulo [s], then [s]
  is unchanged; if [x] and [y] are disequal in [s], then the 
  exception [Exc.Inconsistent] is raised; otherwise, the equality [x = y] is added to 
  [s] to obtain [s'] such that [v s' x] is identical to [v s' y]. *)

val dismerge : t -> Fact.Diseq.t -> unit
(** [diseq d s] adds a disequality of the form [x <> y] to [s]. If [x = y] is
  already known in [s], that is, if [is_equal s x y] yields [Three.Yes], then
  an exception [Exc.Inconsistent] is raised; if [is_equal s x y] equals [Three.No]
  the result is unchanged; otherwise, [x <> y] is added using [D.add]. *)

val gc: (Term.t -> bool) -> t -> unit
  (** [gc p s] removes all noncanonical, internal variables [x] with [p x].
    [diseq] destructive updates the input partition [s]. *)

val fresh_equal : t -> Fact.Equal.t
  (** Chooses a "fresh" variable equality. Each such equality is returned
    at most once. In case there are no fresh variable equalities, [Not_found]
    is raised. *)

val fresh_diseq : t -> Fact.Diseq.t
  (** Chooses a "fresh" variable disequality. Each such disequality is returned
    at most once. In case there are no fresh variable disequalities, [Not_found]
    is raised. *)

val copy : t -> t
  (** [copy p] copies [p] to a partition which is observationally 
    equal to [p].  It is used to protect partitions against destructive
    updates when {!Tools.destructive} is set to true.  That is, 
    if [p'] is [copy p], then destructive updates in [p'] do not 
    affect [p]. *)

val diff : t -> t -> t
  (** [diff p q] contains all facts in [p] but not in [q]. *)



