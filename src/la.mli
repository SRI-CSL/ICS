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

(** Linear arithmetic decision procedure 

  @author Harald Ruess
  @author N. Shankar
*)

(** This module provides the building blocks for a decision procedure
  for real and integer linear arithmetic based on the Simplex algorithm. *)
 (** States [s] consist of two solution sets [(r, t)] with 
    - [r] the {i regular} solution set with equalities of the 
    form [x = a] with [x] a nonslack variable (see {!Term.Var.is_slack})
    and [a] a linear arithmetic term.
    - [t] a {i tableau} with equalities [k = b] with [k] a slack variable,
    and all variables in the linear arithmetic term [b] are slack variables, too.

    A state [s] {i represents} the conjunction of equalities in [r] and [t]. 
    [s |= p] if the atom [p] is {i valid} in [s].  *)


type t
  
val empty : t

val eq : t -> t -> bool

val pp : t Pretty.printer

val is_empty : t -> bool

val copy : t -> t

val find : t -> Jst.Eqtrans.t
val inv : t -> Jst.Eqtrans.t

val current : unit -> t
       
val initialize : t -> unit
val is_unchanged : unit -> bool
val finalize : unit -> t
val abstract : Term.t -> unit
val integer_solve: bool ref
val merge : Jst.Equal.t -> unit
val propagate : Jst.Equal.t -> unit
val dismerge : Jst.Diseq.t -> unit
val propagate_diseq : Jst.Diseq.t -> unit
val branch : unit -> unit
val normalize : unit -> unit
  (** Above is identical to {!Infsys.EQ}. *)
  
val nonneg : Jst.Nonneg.t -> unit
  (** [(g, a >= 0; e; p)] ==> [(g; e'; p')] 
    with 
    - [a] pure
    - [|= e', p' <=> |= e, a >= 0, p]
    - if [e' |= x = y] then [p' |= x = y]. *)
  
val pos : Jst.Pos.t -> unit
  (** [(g, a > 0; e; p)] ==> [(g; e'; p')] 
    with 
    - [a] pure
    - [|= e', p' <=> |= e, a > 0, p]
    - if [e' |= x = y] then [p' |= x = y]. *)
 
  
val model : t -> Term.t list -> Term.t Term.Map.t
  (** [model (p, s) xs] returns an assignment [rho]
    for the variables in [xs] with bindings [x |-> q].
    [q] is either a rational number or a rational 
    number added to the multiple of a "small" constant [eps].
    The assignment [rho] can be extended to a model of [s]. *) 
 
exception Unbounded
