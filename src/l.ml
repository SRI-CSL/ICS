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

open Mpa

module L = Eqs.Make0(
  struct
    let th = Th.app
    let nickname = Th.to_string Th.app
    let map = Apply.map
    let is_infeasible _ = false
  end)

module S = L

type t = S.t

let eq = S.eq
let empty = S.empty
let is_empty = S.is_empty
let pp = S.pp
let copy = S.copy

let apply = S.apply
let find = S.find
let inv = S.inv
let dep = S.dep

(** Either return a fully interpreted term or a canonical variable. *)
let interp (p, s) =
  Jst.Eqtrans.replace Apply.map
    (Jst.Eqtrans.compose
       (Partition.find p)
       (Jst.Eqtrans.totalize
	  (Partition.choose p (apply s))))


let is_dependent = S.is_dependent
let is_independent = S.is_independent

let fold = S.fold

let name = S.name


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x' = b] in [s] with [x = x'] in [p]. *)
let replace (p, s) =
  Jst.Eqtrans.replace Apply.map 
    (Jst.Eqtrans.totalize
       (Partition.choose p (apply s)))

(** [a <> b] if [solve(S[a] = S[b])] is inconsistent. *)
let is_diseq ((_, s) as cfg) a b =
  if is_empty s || not(Term.is_pure Th.cop a) || not(Term.is_pure Th.cop b) then
    None
  else 
    let (a', rho) = replace cfg a
    and (b', tau) = replace cfg b in
      try
	let _ = Apply.solve (a', b') in
	  None
      with
	  Exc.Inconsistent -> Some(Jst.dep2 rho tau)

  
let solve = 
  Trace.func "l" "Solve" Fact.Equal.pp (Pretty.list Fact.Equal.pp)
    (Fact.Equal.equivn Apply.solve) 

let merge ((p, s) as cfg) e =
  Trace.msg "l" "Process" e Fact.Equal.pp;
  let e' = Fact.Equal.map (replace cfg) e in
    try
      let sl = solve e' in
	S.compose (p, s) sl
    with
	Exc.Incomplete ->
	  let e'' = Fact.Equal.map_lhs (S.name cfg) e' in
	    S.compose cfg [e'']


let dismerge (p, s) d =
  if not(is_empty s) then
    let d = Fact.Diseq.map (replace (p, s)) d in
    let (a, b, rho) = Fact.Diseq.destruct d in
      if Apply.solve (a, b) = [] then 
	raise(Jst.Inconsistent(rho))


exception Found	of Term.t * Term.t
 
let rec split (p, s) =
  try
    S.iter
      (fun x (a, _) -> 
	 try split_in_term a with Not_found -> ())
      s;
    raise Not_found
  with
      Found(b, c) -> (b, c)

and split_in_term a =
  match Apply.d_interp a with
    | Sym.C, [b; c; _; _] -> 
	raise(Found(b, c))
    | _, al ->
	List.iter split_in_term al
