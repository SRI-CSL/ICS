
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

type t = Supinf.t Term.Map.t


(*s Pretty-printing. *)

let pp fmt s = 
  let bndngs = Term.Map.fold (fun x c acc -> (x,c) :: acc) s [] in
  Pretty.map Term.pp Supinf.pp fmt bndngs

let ppin =
  Pretty.infix Term.pp " in " Cnstrnt.pp 


(*s Check if [x] is in the domain. *)

let mem x s = Term.Map.mem x s


(*s Constraints as a list. *)

let cnstrnts s =
  Term.Map.fold (fun x c acc -> (x,c) :: acc) s []
  

(*s Accessors. *)

let sup_of x s = 
  try 
    (Term.Map.find x s).Supinf.sup 
  with 
      Not_found -> Term.Set.empty

let inf_of x s = 
  try 
    (Term.Map.find x s).Supinf.inf 
  with 
      Not_found -> Term.Set.empty

let diseqs_of x s =
  try 
    (Term.Map.find x s).Supinf.diseqs 
  with 
      Not_found -> Term.Set.empty

let cnstrnt_of x s =
  try 
    Some((Term.Map.find x s).Supinf.cnstrnt) 
  with 
      Not_found -> None


(*s The empty constraint map. *)

let empty = Term.Map.empty

(*s Test for emptyness. *)

let is_empty s = (s == Term.Map.empty)

(*s Adding a constraint. *)

let rec add (a,c) s =
  Trace.call 6 "Add(c)" (a,c) ppin;
  if Term.is_var a then
    addvar (a,c) s
  else 
    failwith "C.add: to do"

and addvar (x,c) s =
  assert(Term.is_var x);
  try
    let rho = Term.Map.find x s in
    let d = rho.Supinf.cnstrnt in
    match Cnstrnt.cmp c d with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | (Binrel.Super | Binrel.Same) ->
	  (Veqs.empty, s)
      | Binrel.Sub ->
	  (Veqs.empty, failwith "C.addvar: to do")
      | Binrel.Singleton(q) ->
	  (Veqs.empty, failwith "C.addvar: to do")
      | Binrel.Overlap ->
	  (Veqs.empty, failwith "C.addvar: to do")
  with
      Not_found -> 
	(Veqs.empty, Term.Map.add x (Supinf.of_cnstrnt c) s)
  

(*s Merging in an equality between variables. *)

let rec merge1 (x,y) s =
  try
    let c = Term.Map.find x s in
    try
      let d = Term.Map.find y s in
      let cd = Supinf.inter c d in
      if Supinf.is_empty cd then
	raise Exc.Inconsistent
      else 
	(Veqs.empty, Term.Map.add y cd (Term.Map.remove x s))
    with
	Not_found ->
	  let c' = Supinf.inst x y c in
	  (Veqs.empty, Term.Map.add y c' (Term.Map.remove x s))
  with
      Not_found ->
	(Veqs.empty, s)


(*s Iterating [merge1] on a set of variables equalities. *)

and merge el s =
  List.fold_right
    (fun e (es,s) ->
       let (es',s') = merge1 e s in
       (Veqs.union es es', s'))
    el
    (Veqs.empty, s)


(*s Extend domain. *)

let extend (x,c) s =
  assert(not(mem x s));
  Trace.call 6 "Extend(c)" (x,c) ppin;
  Term.Map.add x (Supinf.of_cnstrnt c) s

