
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
open Sym
open Mpa
open Status
(*i*) 

(*s Known disequalities; [x |-> {y,z}] is interpreted as [x <> y & x <> z].
  The function is closed in that forall [x], [y] such that [x |-> {...,y,...} ]
  then also [y |-> {....,x,....}] *)

type t = Term.Set.t Term.Map.t

let empty = Term.Map.empty

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


(*s All terms known to be disequal to [a]. *)

let deq s a =
  try Term.Map.find a s with Not_found -> Term.Set.empty


(*s Check if two terms are known to be disequal. *)

let is_diseq s a b =
  Term.Set.mem b (deq s a)


(*s Set of changed disequalities. *)

module Pairs = Set.Make(
  struct
    type t = Term.t * Term.t
    let compare = Pervasives.compare
  end)

type focus = Pairs.t

module Focus = struct
  let empty = Pairs.empty
  let is_empty = Pairs.is_empty
  let singleton = Pairs.singleton
  let add = Pairs.add
  let union = Pairs.union
  let fold = Pairs.fold
end

(*s Adding a disequality over variables *)

let add d s =
  let (x,y,_) = Fact.d_diseq d in
  let xd = deq s x in
  let yd = deq s y in
  let xd' = Term.Set.add y xd in
  let yd' = Term.Set.add x yd in
  match xd == xd', yd == yd' with
    | true, true -> (s, Focus.empty)
    | true, false -> (Term.Map.add y yd' s, Focus.singleton (x,y))
    | false, true -> (Term.Map.add x xd' s, Focus.singleton (x, y))
    | false, false -> (Term.Map.add x xd' (Term.Map.add y yd' s),
                       Focus.singleton (x, y))


(*s Propagating an equality between uninterpreted terms. *)

let merge e s =
  let (a,b,_) = Fact.d_equal e in
  let da = deq s a and db = deq s b in
  if Term.Set.mem a db || Term.Set.mem b da then
    raise Exc.Inconsistent
  else
    let dab = Term.Set.union da db in
    if db == dab then
      Term.Map.remove a s
    else
      let s' = Term.Map.remove a s in
      Term.Map.add b dab s'


(*s Removing disequalities for [a] by recursively 
 removing [a |-> {...,x,...}] and all [x |-> ...]. *)

and remove a c (s, derived) =
  Term.Set.fold 
    (fun x (acc,derived) ->
       (Term.Map.remove x acc, Atom.mk_in c x :: derived))
    (deq s a)
    (Term.Map.remove a s, derived)


(*s Instantiation. *)

let inst f s =
  Term.Map.fold
    (fun x ys ->
       let x' = f x in
       assert(Term.is_var x');
       let ys' = Term.Set.fold (fun y -> Term.Set.add (f y)) ys Term.Set.empty in
       Term.Map.add x' ys')
    s
    Term.Map.empty
