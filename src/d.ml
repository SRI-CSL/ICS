(*
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
*)

open Sym
open Mpa
open Status

(** Known disequalities; [x |-> {y,z}] is interpreted as [x <> y & x <> z].
  The function is closed in that forall [x], [y] such that [x |-> {...,y,...} ]
  then also [y |-> {....,x,....}] *)

type t = Term.Set.t Term.Map.t

let empty = Term.Map.empty

let eq s t = (s == t)

let deq_of s = s

let to_list s =
  let eq (x1,y1) (x2,y2) =
    (Term.eq x1 x2 && Term.eq y1 y2) ||
    (Term.eq x1 y2 && Term.eq y1 x2)
  in
  let mem (x,y) = List.exists (eq (x,y)) in
  Term.Map.fold
    (fun x ys acc ->
       Term.Set.fold
	 (fun y acc ->
	    if mem (x, y) acc then acc else (x, y) :: acc)
	 ys acc)
    s []

let pp fmt s = 
  let l = to_list s in
  if l <> [] then
    begin
      Format.fprintf fmt "\nd:";
      Pretty.list Term.pp_diseq fmt l
    end


(** All terms known to be disequal to [a]. *)

let deq s a =
  try Term.Map.find a s with Not_found -> Term.Set.empty


(** Check if two terms are known to be disequal. *)

let is_diseq s a b =
  Term.Set.mem b (deq s a)


(** Adding a disequality over variables *)

let rec add d s =
  Trace.msg "d" "Add" d Fact.pp_diseq;
  let (x,y,_) = Fact.d_diseq d in
  let xd = deq s x in
  let yd = deq s y in
  let xd' = Term.Set.add y xd in
  let yd' = Term.Set.add x yd in
  match xd == xd', yd == yd' with
    | true, true -> 
	(Term.Set.empty, s)
    | true, false ->
	Trace.msg "d" "Update" (x, y) Term.pp_diseq;
	(Term.Set.singleton y, Term.Map.add y yd' s)
    | false, true -> 
	Trace.msg "d" "Update" (x, y) Term.pp_diseq;
	(Term.Set.singleton x, Term.Map.add x xd' s)
    | false, false -> 
	Trace.msg "d" "Update" (x, y) Term.pp_diseq;
	let ch' = Term.Set.add x (Term.Set.singleton y) in
	let s' = Term.Map.add x xd' (Term.Map.add y yd' s) in
	  (ch', s')


(** Propagating an equality between variables. *)

let merge e s = 
  Trace.msg "d" "Merge" e Fact.pp_equal;
  let (a, b, _) = Fact.d_equal e in
  let da = deq s a and db = deq s b in
  if Term.Set.mem a db || Term.Set.mem b da then
    raise Exc.Inconsistent
  else
    let dab = Term.Set.union da db in
    if db == dab then
      (Term.Set.empty, Term.Map.remove a s)
    else
      let ch' = Term.Set.singleton b in
      let s' = Term.Map.add b dab (Term.Map.remove a s) in
	(ch', s')

(** Return disequalities for [x]. *)

let d s x =
  Term.Set.fold
    (fun y acc -> 
       (y, None) :: acc)
    (deq s x)
    []
