(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)


open Mpa

module T: Shostak.T = struct
  let th = Theory.of_string "l"
  let map = Apply.map

  let solve e = 
    let (a, b, rho) = e in
      try
	let sl = Apply.solve (a, b) in
	let inj (a, b) = (a, b, rho) in
	  List.map inj sl, Term.Varset.empty
      with
	  Exc.Inconsistent -> raise(Jst.Inconsistent(rho))

  exception Found of Term.t * Term.t

  let disjunction (_, a, _) =
    failwith "to do"
(*
    let rec split_in_term a =
      match Apply.d_interp a with
	| Sym.C, [b; c; _; _] -> 
	    raise(Found(b, c))
	| _, al ->
	    List.iter split_in_term al
    in
      try
	split_in_term a;
	raise Not_found
      with
	  Found(b, c) -> 
	    let e = Atom.mk_equal (b, c)
	    and d = Atom.mk_diseq (b, c) in
	      Clause.of_list ([e; d],Jst.dep0)
*)
end

module S: Shostak.S = struct

  module I = Shostak.Make(T)


  type t = I.t

  let eq = I.eq
  let empty = I.empty
  let is_empty = I.is_empty
  let pp = I.pp
  let apply = I.apply
  let inv = I.inv

  let current = I.current
  let initialize = I.initialize
  let finalize = I.finalize
  let is_unchanged = I.is_unchanged

  (** Replace foreign first-order terms. *)
  let rec export () = 
    failwith "l: to do"
  (*  S.iter export1 (current()) *)

  and export1 ((x, a, rho) as e) =
    failwith "to do"
(*
    if is_first_order a then
      begin
	G.put (Fact.of_equal e);
	(* S.restrict (current()) x *)
	failwith "l: to do"
      end 
*)

  and is_first_order a =
    failwith "to do"
(*
    try
      let f = Term.sym_of a in
      let i = Sym.theory_of f in
	not(i = Th.L)
    with
	Not_found -> false
*)

  let abstract = I.abstract

  let merge e = 
    failwith "to do"
    (*

    assert(Fact.Equal.is_pure Th.L e);
    I.merge e;
    export ()
    *)

  let dismerge d = 
    I.dismerge d;
    export ()

  let branch = I.branch
 
 let normalize = I.normalize

  let propagate e = 
    I.propagate e;
    export ()

   (** Apply disequality [x <> y] to [C x y a b]. *)
  let propagate_diseq d =
    failwith "to do"
(*
    I.propagate_diseq d;
    let (x, y, rho) = d in
    let disapply e = 
      let (z, a, tau) = e in
      let a' = Apply.disapply (x, y) a in
	if a == a' then () else 
	  let e' = Fact.Equal.make z a' (Jst.dep2 rho tau) in
	    failwith "l: to do"
(*
	    S.update (!p, current()) e'
*)
    in
      failwith "l: to do"
(*
      S.Dep.iter (current()) disapply x;
      S.Dep.iter (current()) disapply y;
      export ()
*)
*)
			  
end


(*
module Cl = Shostak.Register(Apply.Infsys)
*)
