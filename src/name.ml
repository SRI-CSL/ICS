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

type t = string

let pp fmt s =
  Format.fprintf fmt "%s" s

let hash = Hashtbl.hash_param 4 4

module Table = Weak.Make(
  struct
    type t = string
    let equal = (=)
    let hash = hash
  end)

let table = Table.create 117

let _ = 
  Tools.add_at_exit 
    (fun () -> 
       let (length, entries, _, smallest, median, biggest) = Table.stats table in
	 if entries > 0 then
	   Format.eprintf 
	     "\nName hash: length = %d; entries = %d; smallest = %d; median = %d; biggest = %d"
	        length entries smallest median biggest)
    
let of_string str =
  Table.merge table str

let is_defined str =
  Table.mem table str

let fresh str =
  let rec loop i =
    let s = str ^ "!" ^ string_of_int i in
      if is_defined s then loop (i+1) else of_string s
  in
    loop 0
  
let to_string s = s

let idx s = hash s
			 
let eq = (==)
  
let compare s t =
  if s == t then 0 else Pervasives.compare s t
		    
module Set = Set.Make(
  struct
    type t = string
    let compare = compare
  end)
  
module Map = Map.Make(
  struct
    type t = string
    let compare = compare
  end)
  
module Hash = Hashtbl.Make(
  struct
    type t = string
    let equal = eq
    let hash = idx
  end)



