
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

(*i*)
open Term
(*i*)

type t = Cnstrnt.t Term.Map.t


(*s Check if [x] is in the domain. *)

let mem = Term.Map.mem


(*s Constraints as a list. *)

let to_list s =
  Term.Map.fold (fun x c acc -> (x,c) :: acc) s []


(*s Pretty-printing. *)

let rec pp fmt s = 
  Pretty.list pp_in fmt (to_list s)

and pp_in fmt =
  Pretty.infix Term.pp "in" Cnstrnt.pp fmt


(*s Accessors. *)

let cnstrnt x s =
  try 
    Some(Term.Map.find x s)
  with 
      Not_found -> None


(*s The empty constraint map. *)

let empty = Term.Map.empty


(*s Test for emptyness. *)

let is_empty s = (s == empty)


(*s Extend domain. *)

let extend (x,c) s =
  assert(not(mem x s));
  Trace.msg 6 "Extend(c)" (x,c) pp_in;
  if Cnstrnt.is_empty c then
    raise Exc.Inconsistent
  else
    Term.Map.add x c s


(*s Restrict domain. *)

let restrict x s =
  Trace.msg 6 "Restrict(c)" x Term.pp;
  Term.Map.remove x s


(*s Adding a constraint. *)
    
let rec add (x,c) s =
  Trace.msg 6 "Add(c)" (x,c) pp_in;
  try
    let d = Term.Map.find x s in
    match Cnstrnt.cmp c d with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | (Binrel.Super | Binrel.Same) ->
	  (s, [])
      | Binrel.Sub ->
	  (Term.Map.add x c s, [])
      | Binrel.Singleton(q) ->
	  (Term.Map.remove x s, [(x, Arith.mk_num q)])
      | Binrel.Overlap(cd) ->
	  (Term.Map.add x cd s, [])
  with
      Not_found -> 
	if Cnstrnt.is_empty c then
	  raise Exc.Inconsistent
	else match Cnstrnt.d_singleton c with
	  | Some(q) ->
	      (Term.Map.remove x s, [(x, Arith.mk_num q)])
	  | None ->
	      (Term.Map.add x c s, [])
