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

(** Simplex algorithm

  @author Harald Ruess
  @author N. Shankar

  This module provides the building blocks for a decision procedure
  for real and integer linear arithmetic based on the Simplex algorithm. *)


module Config : sig

  type t
    (** States [s] consist of two solution sets [(r, t)] with 
      - [r] the {i regular} solution set with equalities of the 
      form [x = a] with [x] a nonslack variable (see {!Term.Var.is_slack})
      and [a] a linear arithmetic term.
      - [t] a {i tableau} with equalities [k = b] with [k] a slack variable,
      and all variables in the linear arithmetic term [b] are slack variables, too.
      
      A state [s] {i represents} the conjunction of equalities in [r] and [t]. 
      [s |= p] if the atom [p] is {i valid} in [s].  *)

  val empty : unit -> t

  val is_empty : t -> bool

  val apply : t -> Term.t -> Term.t * Judgement.equal

  val inv : t ->  Term.t -> Term.t * Judgement.equal

  val pp : Format.formatter -> t -> unit

  val is_minimized : t -> Term.t -> Judgement.nonneg option

  val is_maximized : t -> Term.t -> Judgement.nonneg option

  val is_equal : t -> Term.t -> Term.t -> Judgement.equal option

  val sup : t -> Term.t -> (Mpa.Q.t * Judgement.nonneg) option

  val inf : t -> Term.t -> (Mpa.Q.t * Judgement.nonneg) option

  module Minimize : sig

    val term : t -> Term.t -> Term.t

    val justify : t -> Term.t -> Judgement.equal

  end 
   
end

module Infsys : sig
 
    
  val current : unit -> Config.t
  
  val reset : unit -> unit
    
  val initialize : Config.t -> unit
  
  val is_unchanged : unit -> bool
    
  val finalize : unit -> Config.t
    
  val abstract : Term.t -> Judgement.atom -> unit
    
  val alias : Mpa.Q.t -> Term.t * Judgement.equal
    
  val process_equal : Judgement.equal -> unit
  
  val process_diseq : Judgement.diseq -> unit
    
  val process_nonneg : Judgement.nonneg -> unit
    
  val normalize : unit -> unit

end
    
    
(** Procedures to run at updating. *)
module Effect : sig

  type t = Term.t -> unit

  val procs : t list ref 

  val register : t -> unit

end


 
