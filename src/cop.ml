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


module Cop = Eqs.Make(
  struct
    let th = Th.cop
    let nickname = Th.to_string Th.cop
    let apply = Coproduct.apply
    let is_infeasible _ _ = None
  end)


(** As a side effect, generate disequalities from rhs. *)
module Diseqs = struct
  type t = Cop.t
  type ext = unit
  let empty = ()
  let pp _ () = ()
  let eq () () = true
  let do_at_restrict _ _ = ()
  let do_at_add (p, (), s) (x, a, rho) =   (* [rho |- x = a] *)
    Cop.fold
      (fun y (b, tau) _ ->                 (* [tau |- y = b] *)
	 if Term.eq x y then () else 
	   if Coproduct.is_diseq a b then  (* [a <> b] in [COP]. *)
	     let sigma = Justification.dependencies [rho; tau] in
	     let d = Fact.Diseq.make (x, y, sigma) in
	       Partition.dismerge p d)
      s () 
end

module S = Eqs.Extend(Cop)(Diseqs)

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

let is_dependent = S.is_independent
let is_independent = S.is_independent


let name = S.name


(** [replace s a] substitutes dependent variables [x]
  in [a] with their right hand side [b] if [x = b] in [s].
  The result is canonized. *)
let replace s =
  Fact.Equal.Inj.replace Coproduct.map
    (find s)

let solve = 
  Fact.Equal.Inj.solver Bitvector.solve


let process_equal ((p, s) as cfg) e =  
  let e' = Fact.Equal.map (replace s) e in
    Trace.msg "cop" "Process" e' Fact.Equal.pp;
    let sl = solve e' in
      S.compose (p, s) sl

let rec process_diseq ((p, s) as cfg) d = 
  assert(Fact.Diseq.is_var d);
  Trace.msg "cop" "Process" d Fact.Diseq.pp;
  ()
