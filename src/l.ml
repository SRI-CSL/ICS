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

(** Inference system for the theory of combinatory logic. *)

open Mpa

(*

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
*)

module T: Shostak.T = struct
  let th = Th.app
  let map = Apply.map
  let solve e = 
    let (a, b, rho) = Fact.Equal.destruct e in
      try
	let sl = Apply.solve (a, b) in
	let inj (a, b) = Fact.Equal.make (a, b, rho) in
	  List.map inj sl
      with
	  Exc.Inconsistent -> raise(Jst.Inconsistent(rho))
  let disjunction _ = raise Not_found
end


module E = Shostak.E(T)


module Infsys0: (Infsys.IS with type e = E.t) = struct

  type e = E.t

  module I = Shostak.Make(T)

  let ths = I.ths

  (** Replace foreign first-order terms. *)
  let rec export (g, s, p) = 
    E.S.fold export1 s (g, s, p)

  and export1 e (g, s, p) =
    let (x, a, rho) = Fact.Equal.destruct e in
      if is_first_order a then
	let g = Fact.Input.Equal.add g e
	and s = E.S.restrict s x in
	  (g, s, p)
      else
	(g, s, p)

  and is_first_order a =
    try
      let f = Term.App.sym_of a in
      let i = Sym.theory_of f in
	not(i = Th.app)
    with
	Not_found -> false

  let abstract = I.abstract

  let merge i e (g, s, p) = 
    let (g, s, p) = I.merge i e (g, s, p) in
      export (g, s, p)

  let dismerge i d (g, s, p) = 
    let (g, s, p) = I.dismerge i d (g, s, p) in
      export (g, s, p)

  let branch = I.branch
 
 let normalize = I.normalize

  let propagate e (g, s, p) = 
    let (g, s, p) = I.propagate e (g, s, p) in
      export (g, s, p)

   (** Apply disequality [x <> y] to [C x y a b]. *)
  let propagate_diseq d (g, s, p) =
    let (g, s, p) = I.propagate_diseq d (g, s, p) in
    let (x, y, rho) = Fact.Diseq.destruct d in
    let disapply e (p, s) = 
      let (z, a, tau) = Fact.Equal.destruct e in
      let a' = Apply.disapply (x, y) a in
	if a == a' then (p, s) else 
	  let e' = Fact.Equal.make (z, a', Jst.dep2 rho tau) in
	    E.S.update (p, s) e'
    in
    let (p, s) = E.S.Dep.fold s disapply x (p, s) in
    let (p, s) = E.S.Dep.fold s disapply y (p, s) in
      export (g, s, p)
			  
end



(** Tracing inference system. *)
module Infsys: (Infsys.IS with type e = E.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = E.t
       let level = "cl"
       let eq = E.S.eq
       let diff = E.S.diff
       let pp = E.S.pp
     end)
