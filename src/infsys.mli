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

(** Definition of inference systems.

  @author Harald Ruess
*)

(** Equality sets inference systems are operating on. *)
module type EQS = sig
  type t
  val is_empty : t -> bool
  val pp : Format.formatter -> t -> unit
  val apply : t -> Jst.Eqtrans.t
  val inv : t -> Jst.Eqtrans.t
  val dep : t -> Term.t -> Dep.Set.t
  val mem : Term.t -> t -> bool
  val occurs : Term.t -> t -> bool
  val fold : (Jst.Equal.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Jst.Equal.t -> unit) -> t -> unit
  val model : t -> Term.Model.t
end

(** Abstract interface of an {i inference system} for equality theories.
  Such an inference system operates on configurations [(g, e, v)] with
  - [g] the global inputs (see module {!G}),
  - [e] a set of equalities of type {!Infsys.EQS}, and
  - [v] a set of variable equalities, disequalities, and other constraints (see module {!V}). *)
module type IS = sig

  module Eqs: EQS

  val current : unit -> Eqs.t
    (** Return current state of equality configuration. *)

  val reset : unit -> unit
    (** Reset to empty equality configuration. *)

  val initialize : Eqs.t -> unit
    (** Intitialize inference system with equality set. *)

  val is_unchanged : unit -> bool
    (** Holds if equality configuration is unchanged since latest [initialize] 
      or [reset]. *)

  val finalize : unit -> Eqs.t
    (** Retrieve modified equality set. *)

  val abstract : Term.t -> unit
    (** [(g[a]; e; p)] ==> [(g[x]; e, x = a; p)]
      with 
      - [a] a nonvariable term, 
      - [a] an [i]-pure term, 
      - and [x] fresh. *)

  val process_equal : (Jst.Equal.t -> unit)

  val process_diseq : (Jst.Diseq.t -> unit) 

  val process_ineq : (Jst.Ineq.t -> unit) 

  val propagate_equal : (Term.t -> unit) 

  val propagate_diseq : (Jst.Diseq.t -> unit) 

  val propagate_cnstrnt : (Term.t -> unit) 

  val propagate_nonneg : (Term.t -> unit) 

  val normalize : unit -> unit

end

module Trace(Th: sig val th : Theory.t end)(Infsys: IS): IS
   (** Tracing inference system. *)
















