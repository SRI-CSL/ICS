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

type t = disjunction * Jst.t

and disjunction = Atom.Set.t

let unsat rho = (Atom.Set.empty, rho)

let is_unsat (ds, _) = Atom.Set.is_empty ds

let of_list (dl, rho) =
  let rec loop acc = function
    | [] -> acc
    | d :: dl ->
	if Atom.is_false d then Atom.Set.empty
	else if Atom.is_true d then acc
	else loop (Atom.Set.add d acc) dl
  in
    (loop Atom.Set.empty dl, rho)
       
let to_list (ds, _) = Atom.Set.elements ds

let pp fmt ds =
  Pretty.infixl Atom.pp " OR " fmt (to_list ds)
  
let singleton (atm, rho) =
  (Atom.Set.singleton atm, rho)

let d_singleton (ds, rho) =
  if Atom.Set.cardinal ds = 1 then
    (Atom.Set.choose ds, rho)
  else 
    raise Not_found

let eq (ds1, _) (ds2, _) = ds1 == ds2
