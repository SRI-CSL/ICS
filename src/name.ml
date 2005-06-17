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

type t = {
  mutable name : string;
  mutable hash : int;
}

type name = t  (* nickname *)

let pp fmt n =
  Format.fprintf fmt "%s@?" n.name

let hash n =
  if n.hash >= 0 then n.hash else
    let h = Hashtbl.hash_param 4 4 n.name in
      n.hash <- h; h

let length n = String.length n.name

module Table = Weak.Make(
  struct
    type t = name
    let equal n m = (n.name = m.name)
    let hash = hash
  end)

let table = Table.create 117
    
let of_string =
  let dummy = { name = Obj.magic 0; hash = -1 } in
    fun str ->
      dummy.name <- str; 
      dummy.hash <- -1;
      try Table.find table dummy with Not_found -> 
	let n = { name = str; hash = dummy.hash } in
	  Table.add table n; n

let of_int i = 
  of_string (string_of_int i)
	    
let is_defined =
  let dummy = { name = Obj.magic 0; hash = -1 } in
    fun str -> 
      dummy.name <- str;
      Table.mem table dummy
    
let fresh =
  let str = "v" in
    fun () ->
      let rec loop i =
	let s = str ^ "!" ^ string_of_int i in
	  if is_defined s then loop (i+1) else of_string s
      in
	loop 0
  
let to_string n = n.name
			 
let equal = (==)

let compare n m = 
  if n == m then 0 else
    Pervasives.compare n m
  
let fast_compare n m =
  if n == m then 0 else
    if hash n > hash m then 1 else -1
		    
module Set = Set.Make(
  struct
    type t = name
    let compare = fast_compare
  end)
  
module Map = Map.Make(
  struct
    type t = name
    let compare = fast_compare
  end)
  
module Hash = Hashtbl.Make(
  struct
    type t = name
    let equal = equal
    let hash = hash
  end)




