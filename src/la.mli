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

(** Linear arithmetic decision procedure 

  @author Harald Ruess
  @author N. Shankar

  This module provides the building blocks for a decision procedure
  for real and integer linear arithmetic based on the Simplex algorithm.
*)


module S : (Solution.SET with type ext = Term.Var.Set.t)
  (** States [s] consist of two solution sets [(r, t)] with 
    - [r] the {i regular} solution set with equalities of the 
    form [x = a] with [x] a nonslack variable (see {!Term.Var.is_slack})
    and [a] a linear arithmetic term.
    - [t] a {i tableau} with equalities [k = b] with [k] a slack variable,
    and all variables in the linear arithmetic term [b] are slack variables, too.

    A state [s] {i represents} the conjunction of equalities in [r] and [t]. 
    [s |= p] if the atom [p] is {i valid} in [s].  *)


type tag = R | T

val pp : tag -> S.t Pretty.printer

val can : Partition.t * S.t -> Jst.Eqtrans.t
  (** [replace s a] substitutes dependent variables [x]
    in [a] with their right hand side [b] if [x = b] in [s].
    The result is canonized using {!Arith.map}. *)


(** Inference system for linear arithmetic. *)
module Infsys : (Infsys.ARITH with type e = S.t)

exception Unbounded

val upper : Partition.t * S.t -> Jst.Eqtrans.t
  (** [upper s a] returns either
    - [(b, rho)] such that [b+] is empty and [rho |- a = b], or
    - raises [Unbounded] if [a] is unbounded in [s]. *)

val lower : Partition.t * S.t -> Jst.Eqtrans.t

val is_equal : Partition.t * S.t -> Jst.Pred2.t

(*
val is_nonpos : Partition.t * S.t -> Jst.Pred.t
  (** [is_nonpos s a] returns [Some(rho)] if [a <= 0] holds in [s]. 
    In this case [rho |- a <= 0]. Otherwise, [None] is returned. *)

val is_nonneg : Partition.t * S.t -> Jst.Pred.t
  (** [is_nonneg s a] returns [Some(rho)] if [a >= 0] holds in [s]. 
    In this case [rho |- a >= 0]. Otherwise, [None] is returned. *)

val is_pos : Partition.t * S.t -> Jst.Pred.t
  (** [is_pos s a] returns [Some(rho)] if [a > 0] holds in [s]. 
    In this case [rho |- a > 0]. Otherwise, [None] is returned. *)

val is_neg : Partition.t * S.t -> Jst.Pred.t
  (** [is_neg s a] returns [Some(rho)] if [a < 0] holds in [s]. 
    In this case [rho |- a < 0]. Otherwise, [None] is returned. *)

val is_diseq : Partition.t * S.t -> Jst.Pred2.t
  (** [is_diseq s a b] returns [Some(rho)] if [a<>b] holds in [s]. 
    In this case, [rho |- a <> b]. Otherwise, [None] is returned. *)
*)

val model : S.t -> Term.t list -> Term.t Term.Map.t
  (** [model (p, s) xs] returns an assignment [rho]
    for the variables in [xs] with bindings [x |-> q].
    [q] is either a rational number or a rational 
    number added to the multiple of a "small" constant [eps].
    The assignment [rho] can be extended to a model of [s]. *)
 
