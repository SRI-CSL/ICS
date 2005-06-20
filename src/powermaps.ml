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


module Make(T: Maps.OrderedType) = struct
  type key = T.t
  module Values = Sets.Make(T)
  module Map = Maps.Make(T)(Values)
  type t = Map.t
  let empty = Map.empty
  let is_empty = Map.is_empty
  let singleton = Map.singleton
  let is_singleton = Map.is_singleton
  let find = 
    let empty = Values.empty() in
      fun x m -> 
	try Map.find x  m with Not_found -> 
	  assert(Values.is_empty empty);
	  empty
  let set = Map.set
  let remove = Map.remove
  let copy = Map.copy
  let mem = Map.mem
  let iter = Map.iter
  let fold = Map.fold
  let cardinal = Map.cardinal
  let map = Map.map
  let replace = Map.replace
  let for_all = Map.for_all
  let exists = Map.exists
  let choose = Map.choose
  let destruct = Map.destruct
  let to_list = Map.to_list
  let equal = Map.equal
  let pp = Map.pp
end
