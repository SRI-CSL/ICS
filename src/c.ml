
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

type t = {
  c : Cnstrnt.t Map.t;
  changed : Set.t;
  singletons : Set.t
}

let empty = {
  c = Map.empty;
  changed = Set.empty;
  singletons = Set.empty
}

let eq s t = s.c == t.c

let cnstrnts s = s.c

let singletons s = s.singletons

let apply s x = Map.find x s.c 

let find s x = try Map.find x s.c with Not_found -> Cnstrnt.mk_real

let mem x s = Map.mem x s.c

(*s Constraint for an arithmetic term. *)

let cnstrnt s = 
  Arith.cnstrnt (fun x -> Map.find x s.c)

let cnstrnt_split s =
  Arith.split (fun x -> Map.find x s.c)

(*s [update x i s] updates the constraint map with the constraint [x in i]. *)

let update x i s = 
  Trace.msg "c" "Update" (x, i) Term.pp_in;
  if not(is_var x) then s else 
    {c = Map.add x i s.c; 
     changed = Set.add x s.changed;
     singletons = match Cnstrnt.d_singleton i with 
       | Some _ -> Set.add x s.singletons
       | None -> s.singletons}


(*s Restrict the map. *)

let restrict x s =
  if mem x s then 
    begin
      Trace.msg "c" "Restrict" x Term.pp;
      {s with 
	 c = Map.remove x s.c;
	 changed = Set.remove x s.changed;
	 singletons = Set.remove x s.singletons}
    end 
  else 
    s


(*s Adding a new constraint. *)

let rec add c s =
  Trace.msg "c1" "Add" c Fact.pp_cnstrnt;
  let (x, i, _) = Fact.d_cnstrnt c in
    refine x i s

and refine x i s =
  try
    let j = apply s x in
      (match Cnstrnt.cmp i j with
	 | Binrel.Disjoint -> raise Exc.Inconsistent
	 | (Binrel.Same | Binrel.Super) -> s
	 | Binrel.Sub -> update x i s
	 | Binrel.Singleton(q) -> update x (Cnstrnt.mk_singleton q) s
	 | Binrel.Overlap(ij) -> update x ij s)
  with
      Not_found -> update x i s



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
	  refine y j' s
      | None, Some(q) -> 
	  let i' = Cnstrnt.inter i (Cnstrnt.mk_diseq q) in
	  refine x i' s
      | None, None ->
	  s
  with
      Not_found -> s
	
(*s Split. *)

let split s =
  Term.Map.fold
    (fun x i acc ->
       if Cnstrnt.is_finite i then
	 Atom.Set.add (Atom.mk_in i x) acc
       else 
	 acc)
    s.c Atom.Set.empty

(*s Changes. *)

let changed s = s.changed

let reset s = 
  if Set.is_empty(s.changed) then s else 
    {s with changed = Set.empty}

(*s Pretty-printing. *)

let pp fmt s =
  let l = Map.fold (fun x i acc -> (x, i) :: acc) s.c [] in
  if l <> [] then
    begin
      Format.fprintf fmt "\nc:";
      Pretty.map Term.pp Cnstrnt.pp fmt l
    end


(*s Computing constraints for terms. *)


let rec of_term (v, s) a = 
  match a with  
    | Var _ -> 
	apply s (V.find v a)
    | App(Arith(op), xl) ->
	of_arith (v, s) op xl
    | App(Builtin(op), xl) ->
	of_builtin (v, s) op xl
    | _ ->
	raise Not_found

and of_arith vs op al = 
  match op, al with
    | Num(q), [] -> 
	Cnstrnt.mk_singleton q
    | Multq(q), [x] -> 
	Cnstrnt.multq q (of_term vs x)
    | Add, [x; y] ->
	Cnstrnt.add (of_term vs x) (of_term vs y)
    | Add, _ -> 
	Cnstrnt.addl (List.map (of_term vs) al)
    | _ -> 
	raise Not_found

and of_builtin vs op al =
  match op, al with
    | Mult, _ ->  
	Cnstrnt.multl (List.map (of_term vs) al)
    | Floor, [x] -> 
	Cnstrnt.mk_int
    | Sin, [_] 
    | Cos, [_] ->
	Cnstrnt.mk_cc Dom.Real (Q.of_int (-1)) (Q.of_int 1)
    | Unsigned, [_] ->
	Cnstrnt.mk_nat
    | Expt, [x; _] when Arith.is_q Q.two x ->
	Cnstrnt.mk_ge Dom.Real Q.zero
    | Div, [x; y] ->
	Cnstrnt.div (of_term vs x) (of_term vs y)
    | Apply(Some(Real(n, i))), _ :: xl when List.length xl = n ->
	i
    | _ ->
	raise Not_found
	
