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

type t = (Interval.t * Fact.justification option) Term.Map.t

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
	Format.fprintf fmt "\nc:";
	Pretty.map Term.pp Interval.pp fmt l
      end

let changed = ref Set.empty

let apply s a = Term.Map.find a s

let cnstrnt s x =
  let (i, prf) = apply s x in
    Fact.mk_cnstrnt x i prf

let mem a s = Term.Map.mem a s


(** {6 Abstract interpretation} *)

let of_term s a =
  let lookup x = fst(apply s x) in
  let rec term a =
    match a with
      | Term.App(Arith(op), xl) -> 
	  arith op xl
      | Term.App(Pp(op), xl) -> 
	  pprod op xl
      | Term.App(Bvarith(op), [x]) -> 
	  bvarith op x
      | Term.App(Fun(Apply(Some(d))), [_]) -> 
	  Interval.mk_dom d
      | _ -> 
	  lookup a
  and arith op al = 
    try
      match op, al with
	| Num(q), [] -> 
	    Interval.mk_singleton q
	| Multq(q), [x] -> 
	    Interval.multq q (term x)
	| Add, [x; y] -> 
	    Interval.add (term x) (term y)
	| Add, xl -> 
	    Interval.addl (List.map term xl)
	| _ -> 
	    assert false
      with
	  Not_found -> Interval.mk_real
  and bvarith op a =
    match op with
      | Unsigned -> Interval.mk_nat
  and pprod op al =
    try
      match op, al with
	| Expt(n), [x] -> 
	    Interval.expt n (try term x with Not_found -> Interval.mk_real)
	| Mult, [] -> 
	    Interval.mk_one
	| Mult, [x] -> 
	    term x
	| Mult, [x; y] -> 
	    Interval.mult (term x) (term y)
	| Mult, xl -> 
	    Interval.multl (List.map term xl)
	| _ -> 
	    assert false
      with
	  Not_found -> Interval.mk_real
  in
    term a

let of_term s =
  Trace.func "foo" "C.of_term" Term.pp Interval.pp (of_term s)

(** [update x i s] updates the constraint map with the constraint [x in i]
   and modifies the use lists accordingly. As a side-effect, {!C.changed} 
  is updated. *)
let update a i prf s =
  Trace.msg "c" "Update" (a, i) (Pretty.infix Term.pp " in " Interval.pp);
  if Interval.is_empty i then
    raise Exc.Inconsistent
  else 
    begin
      changed := Term.Set.add a !changed;
      Term.Map.add a (i, prf) s
    end 


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
  changed := Term.Set.empty;
  let (e', s') = add1 c s in
    (!changed, e', s')

and add1 c s =
  Trace.msg "c1" "Add(c)" c Fact.pp_cnstrnt;
  let (x, i, rho) = Fact.d_cnstrnt c in
    try
      let (j, sigma) = apply s x in
	if Interval.is_sub j i then
	  (None, s)
	else
	  let ij = Interval.inter i j in
	    match Interval.d_singleton ij with
	      | Some(q) ->
		   let e' = Fact.mk_equal x (Arith.mk_num q) None 
		   and s' = restrict x s in
		     (Some(e'), s')
	      | None ->
		  (None, update x ij None s)
    with
	Not_found -> 
	  (match Interval.d_singleton i with
	     | Some(q) -> 
		 let e' = Fact.mk_equal x (Arith.mk_num q) None in
		   (Some(e'), s)
	     | None ->
		 (None, update x i rho s))


(** Propagating a variable equality. *)
let rec merge c s =
  changed := Term.Set.empty;
  let (e', s') = merge1 c s in
    (!changed, e', s')

and merge1 e s =
  Trace.msg "c1" "Merge(c)" e Fact.pp_equal;
  let (x, y, _) = Fact.d_equal e in
    try
      let (i, _) = apply s x in
	(try
	   let (j, _) = apply s y in
	   let ij = Interval.inter i j in
	     (match Interval.d_singleton ij with
		| Some(q) -> 
		    let e' = Fact.mk_equal y (Arith.mk_num q) None in
		    let s' = restrict x (restrict y s) in
		      (Some e', s')
		| _ ->
		    let s' = update y ij None (restrict x s) in
		      (None, s'))
	 with
	     Not_found -> 
	       (None, update y i None s))
    with
	Not_found -> 
	  (None, s)


(** Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

let rec diseq d s =
  changed := Term.Set.empty;
  let (e', s') = diseq1 d s in
    (!changed, e', s')

and diseq1 d s =
  let (x, a, _) = Fact.d_diseq d in
  try
    let (i, _) = apply s x in
      (match Arith.d_num a with
	| None -> 
	    (None, s)
	| Some(q) -> 
	    (match Interval.lo i, Interval.hi i with
	       | lo, Some(h, true) when Q.equal q h ->
		   let j = Interval.make (Interval.dom i, lo, Some(h,false)) in
		     (match Interval.d_singleton j with
			| Some(q) -> 
			    let e = Fact.mk_equal x (Arith.mk_num q) None in
			      (Some(e), restrict x s)
			| None -> 
			    (None, update x j None s))
	       | Some(true, l), hi when Q.equal q l ->
		   let j = Interval.make (Interval.dom i, Some(false,l), hi) in
		     (match Interval.d_singleton j with
			| Some(q) -> 
			    let e = Fact.mk_equal x (Arith.mk_num q) None in
			      (Some(e), restrict x s)
			| None -> 
			    (None, update x j None s))
	       | _ ->
		   (None, s)))	   
  with
      Not_found -> 
	(None, s)
	  

(** {6 Predicates} *)

let holds s (a, i) = 
  try
    let j = of_term s a in
      if Interval.is_disjoint i j then
	Three.No
      else if Interval.is_sub j i then
	Three.Yes
      else 
	Three.X
  with
      Not_found -> Three.X


let rec is_diophantine s a =
  try
    let rec loop = function
      | Term.App(Arith(Num(_)), []) -> 
	  true
      | Term.App(Arith(Multq(_)), [x]) -> 
	  loop x
      | Term.App(Arith(Add), xl) -> 
	  List.for_all loop xl
      | a -> 
	  let (i, _) = apply s a in
	    Dom.eq (Interval.dom i) Dom.Int
    in 
      loop a
  with
      Not_found -> false


(** Finite ranges *)

let split s = 
  failwith "split: to do"





