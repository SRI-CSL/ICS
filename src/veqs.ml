
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
 * Author: Harald Ruess
 i*)


(*s Set of variable equalities encoded as a list. *)

module Set = Set.Make(
  struct
    type t = Veq.t
    let compare = Veq.cmp
  end)

type t = Set.t

let empty = Set.empty

let is_empty = Set.is_empty

let destruct es = 
  try
    let e = Set.choose es in
    (e, Set.remove e es)
  with
      Not_found -> assert false

let singleton = Set.singleton

let add x y es = 
  assert(Term.is_var x && Term.is_var y);
  Set.add (Veq.make x y) es

let mem = Set.mem

let remove = Set.remove
 
let union = Set.union

let fold f =
 Set.fold
   (fun e ->
      let (x,y) = Veq.destruct e in
      f x y)

let to_list es = 
  List.map Veq.destruct (Set.elements es)

let pp fmt es =
  Pretty.list Veq.pp fmt (Set.elements es)
