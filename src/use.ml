
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
(*i*)

type t = Set.t Map.t

let mem = Map.mem

let apply u a = Map.find a u

let find u a =
  try Map.find a u with Not_found -> Set.empty

let set = Map.add

(*s empty use list. *)

let empty = Map.empty

(*s [add x a use] adds [x] to the use of [y] for each toplevel
 uninterpreted term in [a]. *)

let add fold x =
  fold 
    (fun y acc ->
       try 
	 let uy = Map.find y acc in
	 let uy' = Set.add x uy in
	 if uy == uy' then acc else Map.add y uy' acc
       with
	   Not_found ->
	     Map.add y (Set.singleton x) acc)

(*s [remove x a s] deletes [x] from the use of [y] for each toplevel
 uninterpreted term in [a]. *)

let remove fold x =
  fold
    (fun y acc ->
       try 
	 let uy = Map.find y acc in
	 let uy' = Set.remove x uy in
	 if Set.is_empty uy' then
	   Map.remove y acc
	 else if uy == uy' then 
	   acc 
	 else 
	   Map.add y uy' acc
       with
	   Not_found -> acc)


(*s Pretty-printing. *)

let rec pp fmt u =
  Pretty.map Term.pp (Pretty.set Term.pp) fmt (to_list u)

and to_list u =
  Map.fold (fun x ys acc -> (x, Set.elements ys) :: acc) u []
