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

module P = Eqs.Make0(
  struct
    let th = Th.p
    let nickname = Th.to_string Th.p
    let map = Product.map
    let is_infeasible _ = false
  end)


module S = P

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

let is_dependent = S.is_dependent
let is_independent = S.is_independent

let fold = S.fold


let name = S.name


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized. *)
let replace s = Jst.Eqtrans.replace Product.map (find s)


(** [a <> b] if [solve(S[a] = S[b])] is inconsistent. *)
let is_diseq ((_, s) as cfg) a b =
  if is_empty s || not(Term.is_pure Th.p a) || not(Term.is_pure Th.p b) then
    None
  else 
    let (a', rho) = replace s a
    and (b', tau) = replace s b in
      try
	let _ = Product.solve (a', b') in
	  None
      with
	  Exc.Inconsistent -> Some(Jst.dep2 rho tau)


let solve = Fact.Equal.equivn Product.solve

let merge ((p, s) as cfg) e =  
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "p" "Process" e' Fact.Equal.pp;
    let sl = solve e' in
      S.compose (p, s) sl

let dismerge (p, s) d =
  if not(is_empty s) then
    let d = Fact.Diseq.map (replace s) d in
    let (a, b, rho) = Fact.Diseq.destruct d in
      if Product.solve (a, b) = [] then 
	raise(Jst.Inconsistent(rho))
