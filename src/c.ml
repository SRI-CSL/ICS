
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
open Sym
open Mpa
(*i*)

type t = (Cnstrnt.t * Fact.justification option) Var.Map.t

let empty = Var.Map.empty

let eq s t = (s == t)

let cnstrnts s = s

let to_list s =
  Var.Map.fold (fun x (i, _) acc -> (x, i) :: acc) s []

let changed = ref Set.empty

let apply s = function
  | Var(x) -> fst(Var.Map.find x s)
  | App _ -> raise Not_found

let find s = function
  | Var(x) ->
      (try 
	 fst(Var.Map.find x s) 
       with 
	   Not_found -> Cnstrnt.mk_real)
  | _ -> Cnstrnt.mk_real

let justification s = function
  | Var(x) -> Var.Map.find x s
  | _ -> raise Not_found

let to_fact s x =
  let (i, prf) = justification s x in
    Fact.mk_cnstrnt x i prf

let mem a s = 
  match a with
    | Var(x) -> Var.Map.mem x s
    | App _ -> false


(*s [update x i s] updates the constraint map with the constraint [x in i]. *)

let update a i s =
  match a with
    | Var(x) ->
	Trace.msg "c" "Update" (a, i) Term.pp_in;
	changed := Term.Set.add a !changed;
	Var.Map.add x (i, None) s
    | _ -> s


(*s Restrict the map. *)

let restrict a s =
  match a with
    | Var(x) when Var.Map.mem x s ->
	Trace.msg "c" "Restrict" a Term.pp;
	changed := Term.Set.remove a !changed;
	Var.Map.remove x s
    | _ -> s


(*s Adding a new constraint. *)

let rec add c s =
  let (x, i, _) = Fact.d_cnstrnt c in
  try
    let j = apply s x in
      (match Cnstrnt.cmp i j with
	 | Binrel.Disjoint -> 
	     raise Exc.Inconsistent
	 | (Binrel.Same | Binrel.Super) -> 
	     s
	 | Binrel.Sub -> 
	     update x i s
	 | Binrel.Singleton(q) -> 
	     update x (Cnstrnt.mk_singleton q) s
	 | Binrel.Overlap(ij) -> 
	     update x ij s)
  with
      Not_found -> 
	update x i s



(*s Merge a variable equality [x = y] in the constraint map by
 adding [x in ij] for the canonical variable [x], where [x in i],
 [y in j] are in the constraint map and [ij] is the intersection of
 [i] and [j], and by removing the constraint for [y]. In case, [ij]
 is a singleton constraint with element [q], an equality [x = q] is
 generated. Singleton constraints are always retained in the constraint
 map in order to keep the invariant that the best available constraint
 are always associated with canonical variables. *)


let merge e s =  
  Trace.msg "c1" "Equal" e Fact.pp_equal;
  let (x, y, _) = Fact.d_equal e in
  try
    let i = find s x in
    let s' = restrict x s in
    try
      let j = find s y in
      match Cnstrnt.cmp i j with
	| Binrel.Disjoint -> raise Exc.Inconsistent
	| (Binrel.Same | Binrel.Super) -> s'
	| Binrel.Sub -> update y i s'
	| Binrel.Singleton(q) -> update y (Cnstrnt.mk_singleton q) s'
	| Binrel.Overlap(ij) -> update y ij s'
    with
	Not_found -> update y i s'     
  with
      Not_found -> s


(*s Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let diseq d s =
  Trace.msg "c1" "Diseq" d Fact.pp_diseq;
  let (x, y, _) = Fact.d_diseq d in
  try
    let i = find s x in
    let j = find s y in
    match Cnstrnt.d_singleton i, Cnstrnt.d_singleton j with
      | Some(q), Some(p) ->
	  if Mpa.Q.equal q p then
	    raise Exc.Inconsistent
	  else 
	    s
      | Some(q), None ->
	  let j' = Cnstrnt.inter j (Cnstrnt.mk_diseq q) in
	    add (Fact.mk_cnstrnt y j' None) s
      | None, Some(q) -> 
	  let i' = Cnstrnt.inter i (Cnstrnt.mk_diseq q) in
	    add (Fact.mk_cnstrnt x i' None) s
      | None, None ->
	  s
  with
      Not_found -> s
	
(*s Split. *)

let split s =
  Var.Map.fold
    (fun x (i, prf) acc ->
       if Cnstrnt.is_finite i then
	 Atom.Set.add (Atom.mk_in (Fact.mk_cnstrnt (Var(x)) i prf)) acc
       else 
	 acc)
    s Atom.Set.empty


(*s Pretty-printing. *)

let pp fmt s =
  let l = to_list s in
    if l <> [] then
      begin
	Format.fprintf fmt "\nc:";
	Pretty.map Var.pp Cnstrnt.pp fmt l
      end

