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

type t = Name.t

let pp = Name.pp

let eq = Name.eq

let create = Name.of_string
	 
let compare = Name.compare

let of_string = Name.of_string

let to_string = Name.to_string

module Map = Name.Map
module Set = Name.Set
module Hash = Name.Hash

module Description = struct

  let table = Hash.create 7

  let add i descr =
    Hash.add table i descr
    
  let get i =
    Hash.find table i

end 
