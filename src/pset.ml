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
    let th = Th.set
    let nickname = Th.to_string Th.set
    let map = Propset.map
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
let replace s = 
  Jst.Eqtrans.replace Propset.map (find s)

let solve sl = 
  Fact.Equal.equivn Propset.solve sl
 
let merge ((p, s) as cfg) e =  
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "pset" "Process" e' Fact.Equal.pp;
    let sl = solve e' in
      S.compose (p, s) sl

