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

(** Inference system for nonlinear multiplication. *)


module T = Ac.Make(Pprod.Sig)

module E = Can.E(Ac.Ac(Pprod.Sig))


let iter s f = 
  let f' e = 
    let (x, a, rho) = Fact.Equal.destruct e in
      try
	let (y, z) = T.d_interp a in   (* [rho |- x = y*z]. *)
	  f x (y, z) rho
      with
	  Not_found -> ()
  in
    E.S.iter f' s

exception Found of Fact.Equal.t * Fact.Equal.t

(** {i Implied disjunctions.} From equality [z' = x*y] in [e] deduce the 
  disjunction 
  - [x = 0] or [y = 1] if  [z' = x] modulo [p]
  - [y = 0] or [x = 1] if [z' = y] modulo [p]. *)
let disjunction (e, p) =
  try
    (iter e
       (fun z' (x, y) rho ->
	  let (z, tau) = Partition.find p z' in
	    match Partition.is_equal p z x with
	      | Some(sigma) -> 
		  let theta = Jst.dep3 rho tau sigma in
		  let d1 = Fact.Equal.make (x, Arith.mk_zero(), theta)
		  and d2 = Fact.Equal.make (y, Arith.mk_one(), theta) in
		    raise(Found(d1, d2))
	      | None -> 
		  (match Partition.is_equal p z y with
		     | Some(sigma) ->   
			   let theta = Jst.dep3 rho tau sigma in
			   let d1 = Fact.Equal.make (y, Arith.mk_zero(), sigma)
			   and d2 = Fact.Equal.make (x, Arith.mk_one(), sigma) in
			     raise(Found(d1, d2))
		     | None -> 
			 ())));
    raise Not_found;
  with
      Found(d1, d2) -> (d1, d2)


(** Deduce new variable inequalities from entries [x = y*z] in [nl]. *)
let rec deduce (nl, p) =
  E.S.fold 
    (fun e ((nl, p) as acc) -> 
       try
	 let (x, rho) = deduce1 p e in
	 let k = Term.Var.mk_slack None (Var.nonneg Dom.Real) in
	 let e' = Fact.Equal.make (x, k, rho) in
	 let p = Partition.merge p e' in
	   (nl, p)
       with
	   Not_found -> acc)
    nl (nl, p)

and deduce1 p e =
  let (x, a, rho) = Fact.Equal.destruct e in
  let (y, tau) = Partition.find p x in
  let (a1, a2) = Pprod.d_interp a in
    match Term.Var.is_slack a1, Term.Var.is_slack a2 with
      | true, true ->
	  if Term.Var.is_slack y then raise Not_found else
	    (y, Jst.dep2 rho tau)
      | true, false -> 
	  if Term.Var.is_slack y then 
	    (a2, Jst.dep2 rho tau)
	  else 
	    raise Not_found
      | false, true -> 
	  if Term.Var.is_slack y then 
	    (a1, Jst.dep2 rho tau)
	  else 
	    raise Not_found
      | false, false -> 
	  raise Not_found




(** {6 Inference system} *)

(** Inference system for nonlinear multiplication as 
  an extension of the AC inference system with deduction
  of new slack equalities. *)
module Infsys0: (Infsys.IS with type e = E.t) = struct

  module Ac = Ac.Infsys(Pprod.Sig)

  type e = E.t

  let ths = Th.Set.singleton Th.nl

  let abstract i a (g, s, p) =
    assert(i = Th.nl);
    Ac.abstract i a (g, s, p)

  let merge i e (g, s, p) =
    let (g, s, p) = Ac.merge i e (g, s, p) in
    let (s, p) = deduce (s, p) in
      (g, s, p)

  let propagate e (g, s, p) =
    assert(Fact.Equal.is_var e);
    let (g, s, p) = Ac.propagate e (g, s, p) in
    let (s, p) = deduce (s, p) in
      (g, s, p)
    
  let dismerge i d c =
    assert(Fact.Diseq.is_pure i d);
    let (g, s, p) = Ac.dismerge i d c in
    let (s, p) = deduce (s, p) in
      (g, s, p)
  
  let propagate_diseq _ c = c
    
  let branch (g, s, p) =  
    try
      let (e1, e2) = disjunction (s, p) in
      let g1 = Fact.Input.Equal.add g e1
      and g2 = Fact.Input.Equal.add g e2 in
	[(g1, s, p); (g2, s, p)]
    with
	Not_found -> 
	  [(g, s, p)]

  let normalize c = c

end


(** Tracing inference system. *)
module Infsys: (Infsys.IS with type e = E.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = E.t
       let level = "nl"
       let eq = E.S.eq
       let diff = E.S.diff
       let pp = E.S.pp
     end)
