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
 * 
 * Author: Harald Ruess
 *)

open Term
open Sym
open Mpa

type t = (Sign.t * Fact.justification option) Term.Map.t

let empty = Term.Map.empty

let eq s t = (s == t)

let cnstrnts s = s

let to_list s =
  Term.Map.fold 
    (fun x (i, _) acc -> (x, i) :: acc) 
    s []

let pp fmt s =
  let l = to_list s in
    if l <> [] then
      begin
	Pretty.string fmt "\nc:";
	Pretty.list (fun fmt (a, i) -> Term.pp fmt a; Sign.pp fmt i) fmt l
      end

let apply s a = Term.Map.find a s

let cnstrnt s x =
  let (i, prf) = apply s x in
    Fact.mk_cnstrnt x i prf

let mem a s = Term.Map.mem a s


(** [update x i s] updates the constraint map with the constraint [x in i]
   and modifies the use lists accordingly. As a side-effect, {!C.changed} 
  is updated. *)

let changed = ref Set.empty

let update c s =
  let update1 (a, i, rho) = 
    if i = Sign.F then
      raise Exc.Inconsistent
    else 
      begin
	Trace.msg "c" "Update" (a, i) (Pretty.infix Term.pp " in " Sign.pp);
	changed := Term.Set.add a !changed;
	Term.Map.add a (i, rho) s
      end 
  in
  let (a, i, rho) = Fact.d_cnstrnt c in
    try
      let (j, sigma) = apply s a in
	if Sign.sub j i then 
	  s 
	else 
	  update1 (a, Sign.inter i j, Fact.mk_rule "Conj" [rho; sigma])
    with
	Not_found -> 
	  update1 (a, i, rho)
	  

(** Restrict the map. *)
let restrict a s =
  try
    let (i, _) = apply s a in
      Trace.msg "c" "Restrict" a Term.pp;
      changed := Term.Set.remove a !changed;
      Term.Map.remove a s
  with
      Not_found -> s


(** Asserting an inequality *)

let rec add c s =  
  Trace.msg "c1" "Add" c Fact.pp_cnstrnt;
  changed := Term.Set.empty;
  let s' = update c s in
    (!changed, s')


(** Propagating a variable equality. *)
let rec merge e s =
  changed := Term.Set.empty;
  let s' = merge1 e s in
    (!changed, s')
    
and merge1 e s =
  Trace.msg "c1" "Merge(c)" e Fact.pp_equal;
  let (x, y, _) = Fact.d_equal e in
    try
      let (i, _) = apply s x in
	(try
	   let (j, _) = apply s y in
	   let c1 = Fact.mk_cnstrnt y (Sign.inter i j) None in
	     update c1 (restrict x s)
	 with
	     Not_found -> 
	       let c1 = Fact.mk_cnstrnt y i None in
		 update c1 (restrict x s))
    with
	Not_found -> s


(** Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let rec diseq d s =
  changed := Term.Set.empty;
  let s' = diseq1 d s in
    (!changed, s')

and diseq1 d s =
  let (x, y, prf) = Fact.d_diseq d in
    try
      let (i, prf1) = apply s x 
      and (j, prf2) = apply s y in
	if i = Sign.Zero && j = Sign.Zero then
	  raise Exc.Inconsistent
	else 
	  s
    with
	Not_found -> s
