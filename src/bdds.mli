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

module type T = sig
  type t 
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val mk_zero : t
  val mk_one : t
  module Args : sig
    type triple
    val eq : triple -> triple -> bool
    val make : t -> t -> t -> triple
    val hash : triple -> int
    val get : triple -> int -> t
    val set : triple -> int -> t -> unit
  end 
  val mk_ite : Args.triple -> t
  val is_ite : t -> bool
  val d_ite : t -> Args.triple
  val mk_fresh : unit -> t
  module Subst : sig
    type map
    val empty : unit -> map
    val compose : map -> t -> t -> unit
  end
end

module Make(T: T) : sig

  val is_zero : T.t -> bool
  val is_one : T.t -> bool
  val is_const : T.t -> bool
  val is_ite : T.t -> bool

  val is_bdd : T.t -> bool

  val drop : T.t -> T.t
  val lift : T.t -> T.t

  val mk_zero : T.t
  val mk_one : T.t
  val mk_ite : T.t -> T.t -> T.t -> T.t
  val mk_conj : T.t -> T.t -> T.t
  val mk_disj : T.t -> T.t -> T.t
  val mk_neg : T.t -> T.t
  val mk_imp : T.t -> T.t -> T.t
  val mk_equiv : T.t -> T.t -> T.t

  val is_diseq : T.t -> T.t -> bool

  exception Inconsistent

  val solve : T.t -> T.t list * T.Subst.map
		     
end
