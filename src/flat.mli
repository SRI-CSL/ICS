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


type t

val is_flat : Term.t -> bool

val is_empty : t -> bool
 
val to_list : t -> Judgement.equal list

val pp : Format.formatter -> t -> unit

val apply : t -> Term.t -> Term.t * Judgement.equal
 
val iter : (Term.t -> Term.t * Judgement.equal -> unit) -> t -> unit

val iter_on : t -> Term.t -> (Term.t -> Term.t * Judgement.equal -> unit) -> unit

val fold : (Term.t -> Term.t * Judgement.equal -> 'a -> 'a) -> t -> 'a -> 'a
  
val mem : Term.t -> t -> bool
  
val dep : t -> Term.t -> Dep.Set.t
 
val occ : Term.t -> t -> bool
 
val in_dom : t -> Term.t -> bool    

val in_cod : t -> Term.t -> bool

val apply : t -> Term.t -> Term.t * Judgement.equal

val inv : t -> Term.t -> Term.t * Judgement.equal

val replace : Term.map -> t -> Judgement.equal ref -> Term.t -> Term.t

val reduce : Term.map * Term.interp -> t -> Judgement.equal ref -> Term.t -> Term.t

val is_canonical : V.Config.t -> t -> bool
   
val empty : unit -> t

val interp : t -> Term.Interp.t

val model : t -> Term.Model.t

val copy : t -> t

module type ARGS = sig
  val can : Term.interp
  val map : Term.map
  val effect : Judgement.equal -> unit
end

module type UPDATE = sig

  val extend : Judgement.equal -> t -> unit

  val add : Judgement.equal -> t -> unit

  val remove : Judgement.equal -> t -> unit

  val instantiate : Judgement.equal -> t -> unit

  val varify : t -> Judgement.atom ref -> Term.t -> Term.t

end
      
module Update(Args: ARGS) : UPDATE

module type TRACE = sig
  val level : int
  val theory : Theory.t
end

module Trace(Tr: TRACE)(U: UPDATE): UPDATE 
