
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

let eq s t =
  s.c == t.c

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

(*s [update x i s] updates teh constraint map with the constraint [x in i]. *)

let update x i s =
  {c = Map.add x i s.c; 
   changed = Set.add x s.changed;
   singletons = 
     match Cnstrnt.d_singleton i with 
       | Some _ -> Set.add x s.singletons
       | None -> s.singletons}

(*s Extend the constraint map. *)

let extend i s =
  let k = Term.mk_fresh_var (Name.of_string "k") None in
  Trace.msg "c" "Extend" (k,i) Term.pp_in;
  (k, update k i s)

(*s Adding a new constraint. *)

let add c s =
  let (x, i, _) = Fact.d_cnstrnt c in
   try
     let j = find s x in
     match Cnstrnt.cmp i j with
       | Binrel.Disjoint -> raise Exc.Inconsistent
       | (Binrel.Same | Binrel.Super) -> s
       | Binrel.Sub -> update x i s
       | Binrel.Singleton(q) -> update x (Cnstrnt.mk_singleton q) s
       | Binrel.Overlap(ij) -> update x ij s
   with
       Not_found -> update x i s


(*s Restrict the map. *)

let restrict x s =
  {s with 
     c = Map.remove x s.c;
     changed = Set.remove x s.changed;
     singletons = Set.remove x s.singletons}


(*s Merge a variable equality [x = y] in the constraint map by
 adding [x in ij] for the canonical variable [x], where [x in i],
 [y in j] are in the constraint map and [ij] is the intersection of
 [i] and [j], and by removing the constraint for [y]. In case, [ij]
 is a singleton constraint with element [q], an equality [x = q] is
 generated. Singleton constraints are always retained in the constraint
 map in order to keep the invariant that the best available constraint
 are always associated with canonical variables. *)


let merge e s = 
  let (x, y, _) = Fact.d_equal e in
  Trace.msg "c" "Merge" (x, y) Term.pp_equal;
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
  let (x, y, _) = Fact.d_diseq d in
  Trace.msg "c" "Diseq" (x, y) Term.pp_diseq;
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
	  update y j' s
      | None, Some(q) -> 
	  let i' = Cnstrnt.inter i (Cnstrnt.mk_diseq q) in
	  update x i' s
      | None, None ->
	  s
  with
      Not_found -> s



(*s Deduce new constraints from newly derived arithmetic facts. *)

let rec deduce (x, b) c = 
  Trace.msg "c" "Deduce" (x, b) Term.pp_equal;
  try
    let i = apply c x in                                  
    match cnstrnt_split c b with
      | Some(_, j), None ->
	  (match Cnstrnt.cmp i j with
	     | Binrel.Disjoint -> raise Exc.Inconsistent
	     | Binrel.Same -> 
		 c
	     | Binrel.Sub -> 
		 propagate b i c
	     | Binrel.Super -> 
		 update x j (propagate b j c)
	     | Binrel.Overlap(ij) -> 
		 update x ij (propagate b ij c)
	     | Binrel.Singleton(q) ->
		 let k = Cnstrnt.mk_singleton q in
		 update x k (propagate b k c))
      | Some(_, j), Some(b'')  ->
	  if is_var b'' then
	    let k = Cnstrnt.subtract i j in
	    update b'' k c
	  else 
	    c
      | _ -> 
	  c
  with
      Not_found ->
	refine (x, b) c

(*s Propagate constraints for [b in i] for each variable [x] in [b].
  Suppose [b] is of the form [pre + q * x + post'], then
  [x in 1/q * (i - (j + k))] is derived, where [pre in j] and
  [post' in k]. Following should be optimized. *)


and propagate b i c =
  let rec loop j post c = 
    match post with
      | [] -> c
      | m :: post' ->
	  try
	    let (q, x) = Arith.mono_of m in
	    let k = cnstrnt c (Arith.mk_addl post') in
	    let j' = Cnstrnt.add (Cnstrnt.multq q (cnstrnt c x)) j in
            let i' = Cnstrnt.multq (Mpa.Q.inv q) (Cnstrnt.subtract i (Cnstrnt.add j k)) in
	    let c' = add (Fact.mk_cnstrnt x i' None) c in
	    loop j' post' c'
	  with
	      Not_found -> c  (* should not happen. *)
  in
  loop Cnstrnt.mk_zero (Arith.monomials b) c
  
and refine (x, b) c =
  try
    let j = cnstrnt c b in
    match Cnstrnt.status j with
      | Status.Empty -> 
	  raise Exc.Inconsistent
      | _ ->
	  update x j c
  with
      Not_found -> c

		
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

let reset s = {s with changed = Set.empty}

(*s Pretty-printing. *)

let pp fmt s =
  let l = Map.fold (fun x i acc -> (x, i) :: acc) s.c [] in
  if l <> [] then
    begin
      Format.fprintf fmt "\nc:";
      Pretty.map Term.pp Cnstrnt.pp fmt l
    end
