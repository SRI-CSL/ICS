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

let nl = Some(Th.nl)

module Eqs = Eqs.Close(Eqs.Make(
  struct
    let th = Th.nl
    let nickname = Th.to_string Th.nl
    let apply = Pprod.apply
    let disapply _ a = a
    let is_infeasible _ _ = None
  end))

type t = Eqs.t

let eq = Eqs.eq
let empty = Eqs.empty
let is_empty = Eqs.is_empty
let pp = Eqs.pp
let apply = Eqs.apply
let find = Eqs.find
let inv = Eqs.inv
let dep = Eqs.dep

let is_dependent = Eqs.is_dependent
let is_independent = Eqs.is_independent

let copy = Eqs.copy

let name (p, s) = Eqs.name (p, s)

let merge s e = 
  Trace.msg "nl" "Merge" e Fact.Equal.pp;
  Eqs.fuse  s [e]

(** Propagate an equality [x = a], where [a] is a linear
  arithmetic term by instantiating rhs with this equality.
  This may introduce linear arithmetic subterms, which need
  to be renamed. *)
let rec propagate (p, la, nl) e =                
  Trace.msg "nl" "Merge" e Fact.Equal.pp;
  let (x, a, rho) = Fact.Equal.destruct e in    (* [rho |- x = a] *)
    assert(Term.is_var x && Term.is_pure Th.a a);
    let instantiate e =
      let (y, b, tau) = Fact.Equal.destruct e in  (* [tau |- y = b] *)
      let c = Nonlin.apply (x, a) b in
	if not(b == c) then                       (* [sigma |- b = c] *)
	  let sigma = Justification.dependencies [rho] in 
	  let (d, upsilon) = purify (p, la, nl) c in (* [upsilon |- c = d] *)
	  let omega' = Justification.trans (y, b, c) tau sigma in
	  let e' = Fact.Equal.make (y, c, omega') in  (* [omega' |- y = c] *)
	    if Pprod.is_interp d then
	      Eqs.update (p, nl) e'
	    else 
	      begin
		Eqs.restrict (p, nl) y;
		Fact.Eqs.push (Some(Th.nl)) e';
		let omega'' = Justification.trans (y, c, d) omega' upsilon in
		let e'' = Fact.Equal.make (y, d, omega'') in
		  Fact.Eqs.push (Some(Th.nl)) e''
	      end 
  in
    Eqs.Dep.iter nl instantiate x

(** Replace linear arithmetic subterms by variables.
  The result is either a nonarithmetic term or a power product. *) 
and purify (p, la, nl) a = 
  let hyps = ref [] in
  let name_la a =
    let (x, rho) = La.name (p, la) a in
      if not(x == a) then hyps := rho :: !hyps;
      x
  and name_nl a =
    let (x, rho) = Eqs.name (p, nl) a in
      if not(x == a) then hyps := rho :: !hyps;
      x
  in  
  let b = 
    try
      (match Arith.d_interp a with
	 | Sym.Num _, [] -> name_la a
	 | Sym.Multq _, [b] -> 
	     let b' = name_nl b in
	       if b == b' then a else 
		 name_la a
	 | Sym.Add, bl -> 
	     let bl' = Term.mapl name_nl bl in
	       if bl == bl' then a else
		 name_la (Arith.mk_addl bl')
	 | _ ->
	     a)
    with
	Not_found -> a
  in
  let rho = Justification.dependencies !hyps in
    (b, rho)
	     
			      


(* {6 Constructing models} *)
     
let models (is_num, lookup) (p, s) =
  failwith "to do"
       


(*

(** Deduce new constraints from nonlinear equalities. *)
and deduce s e =
  let e' = Fact.Equal.map (v s) e in
  let (a, b, rho) = Fact.Equal.destruct e' in
  let sign = to_option (sign s) in
    match sign a, sign b with
      | None, None -> ()
      | None, Some(d, sigma) ->
	  let omega = Justification.dependencies [rho; sigma] in
	    process_sign s ((a, d), omega)   (* [omega |- a in d] *)
      | Some(c, tau), None ->
	  let omega = Justification.dependencies [rho; tau] in
	    process_sign s ((b, c), omega)   (* [omega |- b in c] *)
      | Some(c, tau), Some(d, sigma) -> 
	  let cd = Sign.inter c d in
	    (let omega1 = Justification.dependencies [rho; tau; sigma] in
	       process_sign s ((a, cd), omega1));
	    (let omega2 = Justification.dependencies  [rho; tau; sigma] in
	       process_sign s ((b, cd), omega2))

*)
