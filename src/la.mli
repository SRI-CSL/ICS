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

(** Linear arithmetic decision procedures

  @author Harald Ruess
  @author N. Shankar

  This module provides the building blocks for a decision procedure
  for real and integer linear arithmetic based on the Simplex algorithm.
*)

type t
  (** States [s] consist of two solution sets [(r, t)] with 
    - [r] the {i regular} solution set with equalities of the 
    form [x = a] with [x] a nonslack variable (see {!Term.Var.is_slack})
    and [a] a linear arithmetic term.
    - [t] a {i tableau} with equalities [k = b] with [k] a slack variable,
    and all variables in the linear arithmetic term [b] are slack variables, too.

    A state [s] {i represents} the conjunction of equalities in [r] and [t]. 
    [s |= p] if the atom [p] is {i valid} in [s].  *)

val eq : t -> t -> bool
  (** [eq s1 s2] holds if the respective solution sets of [s1] 
    and [s2] are identical. *)

val empty : t
  (** The {i empty} state *)

val is_empty : t -> bool
  (** [is_empty s] holds iff [s] represents an empty state [s]. *)

val pp : t Pretty.printer
  (** Pretty-printing a state. *)



(** {6 Accessors} *)

val apply : t -> Jst.Eqtrans.t
  (** [apply s x] returns [a] if [x = a] is in [s]; otherwise
    [Not_found] is raised. *)
  
val find : t -> Jst.Eqtrans.t
  (** [find s x] returns [a] if [x = a] is in [s], and [x] otherwise. *)

val inv : t -> Jst.Eqtrans.t
  (** [inv s a] returns [x] if [x = a] is in [s]; otherwise
    [Not_found] is raised. *)

val dep : t -> Term.t -> Term.Var.Set.t
  (** [dep s y] returns the set of [x] such that [x = a] in [s]
    and [y] occurs in [a]. *)

val is_dependent : t -> Term.t -> bool
  (** [is_dependent s x] holds iff there is an [a] such that [x = a] in [s]. *)

val is_independent : t -> Term.t -> bool
  (** [is_independent s y] holds iff [y] occurs in some [a] such that
    [x = a] in [s]. *)


(** {6 Iterators} *)

val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s e] applies [f x (a, rho)] for each [x = a] with justification
    [rho] in [s] and accumulates the result starting with [e]. The order of
    application is unspecified. *)


(** {6 Processing} *)


val copy : t -> t
  (** The update functions {!La.name}, {!La.merge},
    and {!La.dismerge}, {!La.process_nonneg}, {!La.process_pos}, 
    and {b destructively} update equality sets. The function [copy s] 
    can be used to protect state [s] against these updates. *)


type config = Partition.t * t
    (** A {i configuration} consists of a pair [(p, s)] with
      [p] a partitioning and [s] a linear arithmetic equality set.
      A configuration {i represents{ the conjunction of variable
      equalities and disequalities in [p] and the equalities in [s]. *)

val name : config -> Jst.Eqtrans.t
  (** [name (p, s) a] returns a canonical variable [x] 
    with [x = a] in the [r] part of [s].  If there is no such 
    variable, it creates such a variable [v] and updates [s] to 
    include the equality [v = a]. *)

val merge : config -> Fact.Equal.t -> unit
  (** Given a configuration [(p, s)] and an equality [e]
    over {i pure}, linear arithmetic terms
    [merge (p, s) e] adds [e] to [(p, s)]. 
    If [e] conjoined with [s] and [p] is {i inconsistent},
    then {!Jst.Inconsistent} is raised.  Besides 
    {i destructively} updating [s], all generated variable 
    equalities and disequalities are propagated into the 
    partitioning [p].  *)

val process_nonneg : config -> Fact.Nonneg.t -> unit
  (** Given a configuration [(p, s)] and an nonnegativity
    constraint [nn] of the form [a >= 0] with [a] a pure
    linear arithmetic term, [process_nonneg (p, s) nn] adds 
    [nn] to [(p, s)]. If [nn] conjoined with [s] and [p] is 
    {i inconsistent}, then {!Jst.Inconsistent} is 
    raised.  Besides {i destructively} updating [s], all 
    generated variable equalities and disequalities are propagated
    into the partitioning [p].  *)

val process_pos : config -> Fact.Pos.t -> unit
  (** Given a configuration [(p, s)] and an positivity
    constraint [pp] of the form [a > 0] with [a] a pure
    linear arithmetic term, [process_nonneg (p, s) pp] adds 
    [pp] to [(p, s)]. If [pp] conjoined with [s] and [p] is 
    {i inconsistent}, then {!Jst.Inconsistent} is 
    raised.  Besides {i destructively} updating [s], all 
    generated variable equalities and disequalities are propagated
    into the partitioning [p].  *)

val dismerge : config -> Fact.Diseq.t -> unit
  (** Given a configuration [(p, s)] and an disequality
    constraint [a <> b] with [a], [b] either variables or
    [a] a linear term and [b] a rational constant, 
    [dismerge (p, s) d] adds 
    [d] to [(p, s)]. If [d] conjoined with [s] and [p] is 
    {i inconsistent}, then {!Jst.Inconsistent} is 
    raised.  Besides {i destructively} updating [s], all 
    generated variable equalities and disequalities are 
    propagated into the partitioning [p].  *)


(** {6 Boundary Values} *)

exception Unbounded

val upper : config -> Jst.Eqtrans.t
  (** [upper s a] returns either
    - [(b, rho)] such that [b+] is empty and [rho |- a = b], or
    - raises [Unbounded] if [a] is unbounded in [s]. *)

val lower : config -> Jst.Eqtrans.t

val is_nonpos : config -> Jst.Pred.t
  (** [is_nonpos s a] returns [Some(rho)] if [a <= 0] holds in [s]. 
    In this case [rho |- a <= 0]. Otherwise, [None] is returned. *)

val is_nonneg : config -> Jst.Pred.t
  (** [is_nonneg s a] returns [Some(rho)] if [a >= 0] holds in [s]. 
    In this case [rho |- a >= 0]. Otherwise, [None] is returned. *)

val is_pos : config -> Jst.Pred.t
  (** [is_pos s a] returns [Some(rho)] if [a > 0] holds in [s]. 
    In this case [rho |- a > 0]. Otherwise, [None] is returned. *)

val is_neg : config -> Jst.Pred.t
  (** [is_neg s a] returns [Some(rho)] if [a < 0] holds in [s]. 
    In this case [rho |- a < 0]. Otherwise, [None] is returned. *)


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

  val split : config -> t
  (** [of_config (p, s)] returns a finite domain
    interpretations for one of the variables in [s] with a 
    finite interpretation. *)
	
end 

type mode = Max | Min

val model : config -> (Term.t * mode option) list -> Term.t Term.Map.t
  (** [model (p, s) xs] returns an assignment [rho]
    for the variables in [xs] with bindings [x |-> q].
    [q] is either a rational number or a rational 
    number added to the multiple of a "small" constant [eps].
    The assignment [rho] can be extended to a model of [s]. *)
  
    
