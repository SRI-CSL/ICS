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

(** Combined equality sets.

  @author Harald Ruess
  
  Cross product of equality sets indexed by theories in {Theory.t}. *)

module Eqs : sig
  type t
  val theories : unit -> Theory.Set.t
  val empty : unit -> t
  val is_empty : Theory.t -> t -> bool
  val pp : Theory.t -> Format.formatter -> t -> unit
  val apply : Theory.t -> t -> Jst.Eqtrans.t
  val inv : Theory.t -> t -> Jst.Eqtrans.t
  val dep : Theory.t -> t -> Term.t -> Dep.Set.t
  val mem : Theory.t -> Term.t -> t -> bool
  val model : Theory.t -> t -> Term.Model.t
end

module type INFSYS = Infsys.IS
module type EQS = Infsys.EQS

(** Abstract interface of an {i inference system} for equality theories.
  Such an inference system operates on configurations [(g, e, v)] with
  - [g] the global inputs (see module {!G}),
  - [e] a set of equalities of type {!Infsys.EQS}, and
  - [v] a set of variable equalities, disequalities, and other constraints (see module {!V}). *)
module Infsys: sig
  module Eqs: EQS
  val current : unit -> Eqs.t
  val reset : unit -> unit
  val initialize : Eqs.t -> unit
  val is_unchanged : unit -> bool
  val finalize : unit -> Eqs.t
  val abstract : Theory.t -> Term.t -> Jst.Fact.t -> unit
  val process_equal : Theory.t -> Jst.Equal.t -> unit
  val process_diseq : Theory.t -> Jst.Diseq.t -> unit 
  val process_ineq : Theory.t -> Jst.Ineq.t -> unit
  val propagate_equal : Term.t -> unit 
  val propagate_diseq : Jst.Diseq.t -> unit 
  val propagate_cnstrnt : Term.t -> unit 
  val propagate_nonneg : Term.t -> unit
  val normalize : unit -> unit
end

module type TH = sig val th: Theory.t end

module Register(Th: TH)(Is: INFSYS): sig end

