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

(** Decision procedures for coproducts

  @author Harald Ruess
  @author N. Shankar
*)

type t
  (** Representing sets of equalities [{x1 = a1, ..., xn = an}]
    with [xi] a variable and [ai] pure array terms which do
    not contain any of the {i dependent} variables [xi]. *)

val eq : t -> t -> bool
  (** [eq s1 s2] succeeds if [s1] and [s2] are identical. If
    [eq s1 s2] holds, then [s1] and [s2] are logically equivalent. *)
  
val pp : t Pretty.printer
  (** Pretty-printing a set of solved equalities *)
  
val empty : t
  (** The empty coproduct equality set. *)
  
val is_empty : t -> bool
  (** [is_empty s] succeeds iff [s] represents the empty equality set. *)
  
val apply : t -> Term.t -> Term.t * Jst.t
  (** [apply s x] returns [a, rho] if [x = a] is in [s] and
   [rho |- x = a]; otherwise [Not_found] is raised. *)
  
val find : t -> Term.t -> Term.t * Jst.t
  (** [find s x] returns [a, rho] if [x = a] is in [s] and
    [rho |- x = a]; and [x, rho] with [rho |- x = x] otherwise. *)
  
val inv : t -> Term.t -> Term.t * Jst.t
  (** [inv s a] returns [x] if [x = a] is in [s]; 
    otherwise [Not_found] is raised. *)
  
val dep : t -> Term.t -> Term.Var.Set.t
  (** [dep s y] returns the set of [x] such that [x = a] in [s]
    and [y] occurs in [a]. *)
  
val is_dependent : t -> Term.t -> bool
  (** [is_dependent s x] holds iff there is an [a] such that [x = a] in [s]. *)
  
val is_independent : t -> Term.t -> bool
  (** [is_independent s y] holds iff [y] occurs in some [a] such 
    that [x = a] in [s]. *)
  
val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s e] applies [f x (a, rho)] for each [x = a] with justification
    [rho] in [s] and accumulates the result starting with [e]. The order of
    application is unspecified. *)


val is_diseq : Partition.t * t -> Jst.Pred2.t
  (** [is_diseq (_, s) a b] holds iff [s] implies a <> b] in the theory
    of coproduct. *)
  
val copy : t -> t
  (** The update functions {!Cop.name}, {!Cop.merge},
    and {!Cop.dismerge} {b destructively} update equality
    sets. The function [copy s] can be used to protect state [s]
    against these updates. *)
  
val name : Partition.t * t -> Jst.Eqtrans.t
  (** [name (p, s) a] returns a canonical variable [x] 
    with [x = a] in [s].  If there is no such variable,
    it creates a fresh variable [v] and updates [s] to 
    include the equality [v = a]. *)
  
val merge : Partition.t * t -> Fact.Equal.t -> unit
  (** [merge (p, s) e] conjoins a coproduct solution
    set [s] with an equality [e] over pure coproduct terms.
    If [e] conjoined with [s] and [p] is {i inconsistent}
    in theory [COP], then {!Jst.Inconsistent(rho)} is raised,
    with [rho] a justification for the inconsistency.
    Besides {i destructively} updating [s], all newly generated 
    variable equalities and disequalities are propagated into 
    the partitioning [p]. *)
  
val dismerge : Partition.t * t -> Fact.Diseq.t -> unit
