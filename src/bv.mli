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

(** Bitvector decision procedures

  @author Harald Ruess
*)

type t
  (** Representation of sets of equalities of the 
    form [x = a] with [x] a variable and [a] a bitvector term. *)

val eq : t -> t -> bool
  (** [eq s1 s2] succeeds if [s1] and [s2] are identical. If
    [eq s1 s2] holds, then [s1] and [s2] are logically equivalent. *)

val pp : t Pretty.printer
  (** Pretty-printing a bitvector equality set} *)

val empty : t
  (** The empty bitvector equality set. *)

val is_empty : t -> bool
  (** [is_empty s] succeeds iff [s] represents the empty equality set. *)
  
val apply : t -> Term.t -> Term.t * Jst.t
 (** [apply s x] returns [a] if [x = a] is in [s]; 
   otherwise [Not_found] is raised. *)

val find : t -> Term.t -> Term.t * Jst.t
  (** [find s x] returns [a] if [x = a] is in [s], and [x] otherwise. *)

val inv : t -> Term.t -> Term.t * Jst.t
  (** [inv s a] returns [x] if [x = a] is in [s]; 
    otherwise [Not_found] is raised. *)

val dep : t -> Term.t -> Term.Var.Set.t
  (** [dep s y] returns the set of [x] such 
    that [x = a] in [s] and [y] occurs in [a]. *)

val is_dependent : t -> Term.t -> bool
  (** [is_dependent s x] holds iff there is an [a] such 
    that [x = a] in [s]. *)

val is_independent : t -> Term.t -> bool
  (** [is_independent s y] holds iff [y] occurs in some [a] such 
    that [x = a] in [s]. *)

val is_diseq : t -> Jst.Pred2.t
  (** [is_diseq s x y] holds if [x = b] and [x = c], where [b], [c]
    are disequal bitvector constants. *)

val copy : t -> t
 (** The update functions {!Bv.name}, {!Bv.merge},
    and {!Bv.dismerge} {b destructively} update equality
    sets. The function [copy s] can be used to protect state [s]
    against these updates. *)

val name : Partition.t * t -> Jst.Eqtrans.t
  (** [name (p, s) a] returns a canonical variable [x] 
    with [x = a] in [s].  If there is no such variable,
    it creates such a variable [v] and updates [s] to 
    include the equality [v = a]. *)

val merge : Partition.t * t -> Fact.Equal.t -> unit
  (** [merge (p, s) e] conjoins a bitvector solution
    set [s] with an equality [e] over bitvector terms.
    If [e] conjoined with [s] and [p] is {i inconsistent},
    then {!Jst.Inconsistent} is raised.  Besides 
    {i destructively} updating [s], all generated 
    variable equalities and disequalities are 
    propagated into the partitioning [p]. *)

val dismerge : Partition.t * t -> Fact.Diseq.t -> unit
  (** Propagation of variable disequalites [x <> y]. If this
    disequality together with the [p] and [s] imply an
    equality [x = b] or [y = b], then this equality is 
    merged into [s].  Otherwise, if an inconsistency is
    detected, {Jst.Inconsistent} is raised. *)
