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

(** Datatype for endpoints of intervals (see module [Interval])

  @author Harald Ruess
*)


type t = Extq.t * bool
    (** An {i endpoint} is a pair [(u, alpha)] consisting of an 
      extended rational [u] of type {!Extq.t} and a Boolean [alpha]. 
      If [alpha] is [false], then this endpoint is said to be
      {i strict}; otherwise it is {i nonstrict}. *)


(** {6 Comparison.} *)

val eq : t -> t -> bool
  (** Two endpoint [(u, alpha)] and [(v, beta)] are equal if
    {!Extq.eq}[u v] holds and [alpha = beta]. *)
  

(** {6 Constructor} *)

val make : Extq.t * bool -> t
  (** Constructing an endpoint. *)

val neginf : t
  (** Endpoint [(Extq.neginf, false)]. *)

val posinf : t
  (** Endpoint [(Extq.posinf, false)]. *)

val strict : Mpa.Q.t -> t
  (** [strict q] constructs the strict endpoint [(q, false)]. *)

val nonstrict : Mpa.Q.t -> t
  (** [nonstrict q] constructs the nonstrict endpoint [(q, true)]. *)


(** {6 Accessors} *)

val destruct : t -> Extq.t * bool
  (** Return endpoint as a tuple (currently just the identity ) *)

val value : t -> Extq.t
  (** Return the extended rational [x] of an endpoint [(x, _)]. *)

val kind : t -> bool
  (** Return the strictness flag [alpha] of an endpoint [(_, alpha)]. *)


(** {6 Recognizers} *)

val is_q : t -> bool
  (** [is_q e] holds if [value e] is rational. *)

val is_z : t -> bool
  (** [is_z e] holds if [value e] is integer. *)

val is_strict : t -> bool
  (** [is_strict e] holds iff [e] is strict. *)

val is_nonstrict : t -> bool
  (** [is_nonstrict e] holds iff [e] is nonstrict. *)


(** {6 Accessors} *)

val q_of : t -> Mpa.Q.t
  (** Get value of a rational endpoint. *)

val z_of : t -> Mpa.Z.t
  (** Get value of an integer endpoint. *)

