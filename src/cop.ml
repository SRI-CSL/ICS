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


(** As a side effect, generate disequalities from rhs. *)
module Diseqs: Eqs.EXT = struct
  type t = unit
  let empty = ()
  let pp _ () = ()
  let do_at_restrict _ _ = ()
  let do_at_add (p, (), find) (x, a, rho) =   (* [rho |- x = a] *)
    Term.Var.Map.iter
      (fun y (b, tau) ->                      (* [tau |- y = b] *)
	 if Coproduct.is_diseq a b then       (* [a <> b] in [COP]. *)
	   let sigma = Jst.dep2 rho tau in
	   let d = Fact.Diseq.make (x, y, sigma) in
	     Partition.dismerge p d)
      find
end

module Cop = Eqs.Make(
  struct
    let th = Th.cop
    let nickname = Th.to_string Th.cop
    let map = Coproduct.map
    let is_infeasible _ = false
  end)
  (Diseqs)


module S = Cop

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

let fold = S.fold

let is_dependent = S.is_dependent
let is_independent = S.is_independent

let uninterp (p, s) =
  Jst.Eqtrans.compose 
    (Partition.find p)
    (Jst.Eqtrans.totalize (inv s))

let name = S.name

(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized. *)
let replace s =
  Jst.Eqtrans.replace Coproduct.map (find s)


(** [a <> b] if [solve(S[a] = S[b])] is inconsistent. *)
let is_diseq ((_, s) as cfg) a b =
  if is_empty s || not(Term.is_pure Th.cop a) || not(Term.is_pure Th.cop b) then
    None
  else 
    let (a', rho) = replace s a
    and (b', tau) = replace s b in
      try
	let _ = Coproduct.solve (a', b') in
	  None
      with
	  Exc.Inconsistent -> Some(Jst.dep2 rho tau)

let merge ((p, s) as cfg) e =  
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "cop" "Process" e' Fact.Equal.pp;
    let sl = Fact.Equal.equivn Coproduct.solve e' in
      S.compose (p, s) sl

(** [x <> y] implies [inl(x) = inl(y)],  [inr(x) = inr(y)]. *) 
let rec dismerge ((p, s) as cfg) d = 
  assert(Fact.Diseq.is_var d);
  Trace.msg "cop" "Process" d Fact.Diseq.pp;
  () (* to do *)
