(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** {i Exponential maps} 
  
  This module implements finite maps with bindings
  of the form [x |-> {x{1},...,x{n}}].  All update operations 
  over maps are destructive (side-effects).

  The implementation uses {i Splay trees} for dynamic balancing of 
  binary tree representations.  Therefore, insertion of a new element, 
  for example, is of {i amortized} logarithmic cost.
*)


(** Output signature of the functor {!Powermaps.Make}. *)
module type S = sig 
  type key
  module Values : (Sets.S with type elt = key)
  type t  
  val empty: unit -> t
  val is_empty: t -> bool
  val singleton : key -> Values.t -> t
  val is_singleton : t -> bool
  val find: key -> t -> Values.t
  val set: key -> Values.t -> t -> unit    
  val remove: key -> t -> unit
  val copy : t -> t
  val mem: key -> t -> bool    
  val iter: (key -> Values.t -> unit) -> t -> unit
  val fold: (key -> Values.t -> 'b -> 'b) -> t -> 'b -> 'b
  val cardinal : t -> int
  val map : (Values.t -> Values.t) -> t -> t
  val replace : t -> key -> key -> t
  val for_all : (key -> Values.t -> bool) -> t -> bool
  val exists : (key -> Values.t -> bool) -> t -> bool
  val choose : (key -> Values.t -> bool) -> t -> key * Values.t
  val destruct : (key -> Values.t -> bool) -> t -> key * Values.t * t
  val to_list : t -> (key * Values.t) list
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(** {i Exponential maps}. Functor building an implementation of 
  maps with bindings of the form [x |-> {x{1},...,x{n}}] given 
  a totally ordered type [T]. *)
module Make(T: Maps.OrderedType): (S with type key = T.t)
