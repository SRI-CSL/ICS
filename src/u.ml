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

let is_flat = 
  function
    | Term.Var _ -> false
    | Term.App(f, xl, _) ->
	Sym.Uninterp.is f && List.for_all Term.is_var xl


let map f a =
  assert(is_flat a);
  let (sym, zl) = Term.App.destruct a in
  let zl' = Term.mapl f zl in
    if zl == zl' then a else Term.App.mk_app sym zl'

module U = Eqs.Make0(
  struct
    let th = Th.u
    let nickname = Th.to_string Th.u
    let map = map
    let is_infeasible _ = false
  end)


type t = U.t

let eq = U.eq
let empty = U.empty
let is_empty = U.is_empty
let pp = U.pp
let copy = U.copy

let apply = U.apply
let find = U.find
let inv = U.inv
let dep = U.dep

let is_dependent = U.is_dependent
let is_independent = U.is_independent

let name (p, s) = Jst.Eqtrans.compose (Partition.find p) (U.name (p, s))

let merge ((p, s) as cfg) e =
  Trace.msg "u" "Process" e Fact.Equal.pp;
  assert(Fact.Equal.is_var e);
  U.fuse  (p, s) [e]
