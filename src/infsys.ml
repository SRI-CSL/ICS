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

module type IS = sig
  module Eqs: EQS
  val current : unit -> Eqs.t
  val reset : unit -> unit
  val initialize : Eqs.t -> unit
  val is_unchanged : unit -> bool
  val finalize : unit -> Eqs.t
  val abstract : Term.t -> unit
  val process_equal : (Jst.Equal.t -> unit)
  val process_diseq : (Jst.Diseq.t -> unit)
  val process_ineq : (Jst.Ineq.t -> unit)
  val propagate_equal : (Term.t -> unit)
  val propagate_diseq : (Jst.Diseq.t -> unit)
  val propagate_cnstrnt : (Term.t -> unit)
  val propagate_nonneg : (Term.t -> unit)
  val normalize : unit -> unit
end


module Trace(Th: sig val th : Theory.t end)(Infsys: IS): IS = struct

  let msg =  
    let name = Theory.to_string Th.th in
      fun str -> Format.sprintf "%s: %s" name str

  module Eqs = Infsys.Eqs

  let current = 
    Trace.func 2 (msg "current") Pretty.unit Infsys.Eqs.pp Infsys.current

  let reset = 
    Trace.proc 2 (msg "reset") Pretty.unit Infsys.reset

  let initialize = 
    Trace.func 2 (msg "initialize") Infsys.Eqs.pp Pretty.unit Infsys.initialize

  let is_unchanged = 
    Trace.func 2 (msg "is_unchanged") Pretty.unit Pretty.bool Infsys.is_unchanged

  let finalize = 
    Trace.func 2 (msg "finalize") Pretty.unit Infsys.Eqs.pp Infsys.finalize
      
  let abstract = 
    Trace.func 2 (msg "abstract") Term.pp Pretty.unit Infsys.abstract

  let process_equal =
    Trace.proc 2 (msg "process") Jst.Equal.pp Infsys.process_equal

  let process_diseq =
    Trace.proc 2 (msg "process") Jst.Equal.pp Infsys.process_diseq

  let process_ineq =
    Trace.proc 2 (msg "process") Jst.Equal.pp Infsys.process_ineq

  let propagate_equal = 
    Trace.proc 2 (msg "propagate") Term.pp Infsys.propagate_equal

  let propagate_diseq = 
    Trace.proc 2 (msg "propagate") Jst.Diseq.pp Infsys.propagate_diseq

  let propagate_cnstrnt = 
    Trace.proc 2 (msg "propagate") Term.pp Infsys.propagate_cnstrnt

  let propagate_nonneg = 
    Trace.proc 2 (msg "propagate") Term.pp Infsys.propagate_nonneg

  let normalize = 
    Trace.proc 2 (msg "normalize") Pretty.unit Infsys.normalize

end




