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

let name = S.name


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x' = b] in [s] with [x = x'] in [p]. *)
let replace (p, s) =
  Jst.Eqtrans.replace Apply.map 
    (Jst.Eqtrans.totalize
       (Partition.choose p (apply s)))


let merge ((p, s) as cfg) e =
  Trace.msg "l" "Process" e Fact.Equal.pp;
  let e = Fact.Equal.map_lhs (name cfg) e in
    S.compose cfg [e]
