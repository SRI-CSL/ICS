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

module S = Solution.Set

module T = Ac.Make(Pprod.Sig)

let iter s f = 
  let f' e = 
    let (x, a, rho) = e in
      try
	let (y, z) = T.d_interp a in   (* [rho |- x = y*z]. *)
	  f x (y, z) rho
      with
	  Not_found -> ()
  in
    S.iter f' s

exception Found of Atom.t * Atom.t * Jst.t


(** Transform the equation [e] of the form [a = b] to the equivalent [x = c].  
  Assumes that [x] is a subterm of [a - b]. Otherwise [Not_found] is raised. *)
let isolate x e =
  let (a, b, rho) = e in
    let (y, c) = Arith.isolate x (a, b) in
      assert(Term.eq x y);
      Fact.Equal.make x c rho


(** [apply e a], for [e] of the form [x = b], applies the
  substition [x := b] to [a]  *)
let apply_subst e = 
  let (x, a, rho) = e in
  let lookup y = if Term.eq x y then (a, rho) else (y, Jst.dep0) in
    Jst.Eqtrans.replace Pprod.map lookup


module Ops = Ac.Ops(Pprod.Sig)


(** Inference system for nonlinear multiplication as 
  an extension of the AC inference system with deduction
  of new slack equalities. *)
module Infsys0: (Infsys.EQ with type e = S.t) = struct

  module Ac = Ac.Infsys(Pprod.Sig)

  type e = S.t

  let current = Ac.current
  let initialize = Ac.initialize
  let finalize = Ac.finalize

  open Infsys

  let abstract a =
    assert(Term.is_pure Th.nl a);
    Ac.abstract a


  (** Deduce new variable inequalities from entries [x = y*z] in [nl]. *)
  let rec infer_nonneg () =
    let infer (x, a, rho) = 
      let (y, tau) = Partition.find !p x in
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
    in
      S.iter
	(fun e -> 
	   try
	     let (x, rho) = infer e in
	     let k = Term.Var.mk_slack None (Var.nonneg Dom.Real) in
	     let e' = Fact.Equal.make x k rho in
	       Partition.merge !p e'
	   with
	       Not_found -> ())
	(current())

  let rec merge e =
    assert(Fact.Equal.is_pure Th.nl e || Fact.Equal.is_pure Th.la e);
    if Fact.Equal.is_pure Th.nl e then
      merge_nl e
    else 
      merge_la e

  and merge_nl e = 
    assert(Fact.Equal.is_pure Th.nl e);
    Ac.merge e ;
    infer_nonneg ()

  (** Infer from an equality [a = b], where [a], [b] are linear
    arithmetic terms by instantiating all nonlinear rhs in [s]
    with solved forms of [a = b]. *)
  and merge_la ((a, b, rho) as e) =
    assert(Fact.Equal.is_pure Th.la e);
    if not(S.is_empty (current())) then
      let a_sub_b = Arith.mk_sub a b in
      let e' = Fact.Equal.make a_sub_b (Arith.mk_zero()) rho in
	Arith.iter 
	  (fun y -> 
	     try infer_la (isolate y e') with Not_found -> ())
	  a_sub_b

  and infer_la ((x, a, _) as e) =
    assert(Term.is_var x && Arith.is_interp a);
    S.Dep.iter (current())           
      (fun (y, b, rho) ->                    (* [rho |- y = b] *)
	 let (b', tau) = apply_subst e b in  (* [tau |- b = b'] *)
	   if b == b' then () 
	   else if Term.is_var b' then
	     begin
	       Partition.merge !p (Fact.Equal.make y b' (Jst.dep2 rho tau));
	       S.restrict (current()) y;
	     end 
	   else if Term.is_pure Th.nl b' then
	     ()
	   else if Term.is_pure Th.la b' then
	     begin
	       S.restrict (current()) y; 
	       match La.is_equal (!p, La.Infsys.current()) y b' with
		 | Some _ -> ()
		 | None ->   
		     let (y', sigma) = Partition.find !p y in
		     let rho' = Jst.dep3 rho tau sigma in
		     let e' = Fact.Equal.make y' b' rho' in
		       La.Infsys.merge e'
	     end
	   else  
	     let (y', sigma) = Partition.find !p y in
	       match La.is_equal (!p, La.Infsys.current()) y' b' with
		 | Some _ -> ()
		 | None -> 
		     let rho' = Jst.dep3 rho tau sigma in
		     let e' = Fact.Equal.make y' b' rho' in
		     let fct' = Fact.of_equal(Fact.Equal.make y' b' rho') in
		       G.put fct' !g)
      x


  let nl_merge_la_eqs e =
    assert(Fact.Equal.is_pure Th.la e);
    if not(S.is_empty (current())) then
      merge_la e

  (* [nl_merge_la_eqs] can not be called in module [la] because of mutual
   dependency of [La] and [Nl], so we use a hack and install this
   function into a global variable of [La]. *)
  let _ =
    La.nl_merge := nl_merge_la_eqs

  let propagate e =
    assert(Fact.Equal.is_var e);
    Ac.propagate e;
    infer_nonneg ()
    
  let dismerge d =
    assert(Fact.Diseq.is_pure Th.nl d);
    Ac.dismerge d;
    infer_nonneg ()
  
  let propagate_diseq _ = ()
    
  let rec branch () =
    try
      let cl = disjunction (current(), !p) in
	G.put_clause cl !g
    with
	Not_found -> ()

  (** {i Implied disjunctions.} From equality [z' = x*y] in [e] deduce the 
    disjunction 
    - [x = 0] or [y = 1] if  [z' = x] modulo [p]
    - [y = 0] or [x = 1] if [z' = y] modulo [p]. *)
  and disjunction (e, p) =
    try
      (iter e
	 (fun z' (x, y) rho ->
	    let (z, tau) = Partition.find p z' in
	      match Partition.is_equal p z x with
		| Some(sigma) -> 
		    let theta = Jst.dep3 rho tau sigma in
		    let d1 = Atom.mk_equal (x, Arith.mk_zero())
		    and d2 = Atom.mk_equal (y, Arith.mk_one()) in
		      raise(Found(d1, d2, theta))
		| None -> 
		    (match Partition.is_equal p z y with
		       | Some(sigma) ->   
			   let theta = Jst.dep3 rho tau sigma in
			   let d1 = Atom.mk_equal (y, Arith.mk_zero())
			   and d2 = Atom.mk_equal (x, Arith.mk_one()) in
			     raise(Found(d1, d2, theta))
		       | None -> 
			   ())));
      raise Not_found;
    with
	Found(d1, d2, rho) -> 
	  Clause.of_list ([d1; d2],  rho)


  (** No normalization. *)
  let normalize _ = ()
		      
end

(** Tracing inference system. *)
module Infsys: (Infsys.EQ with type e = S.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = S.t
       let level = "nl"
       let eq = S.eq
       let diff = S.diff
       let pp = S.pp
     end)
