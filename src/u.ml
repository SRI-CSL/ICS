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

(** Inference system for the uninterpreted theory. *)

module G = Fact.Input
module P = Partition

let is_flat = function
  | Term.Var _ -> 
      false
  | Term.App(f, xl, _) ->
      Sym.Uninterp.is f && List.for_all Term.is_var xl
      
let is_pure = Term.is_pure Th.u


module S: Solution.SET0 = Solution.Make0(
  struct 
    let th = Th.u
    let map f a = 
      assert(is_flat a);
      let (sym, zl) = Term.App.destruct a in
      let zl' = Term.mapl f zl in
	if zl == zl' then a else Term.App.mk_app sym zl'
  end)

let inv (s, p) =
  Jst.Eqtrans.compose (P.find p) (S.inv s)


(** Congruence closure inference system. *)
module Infsys0: (Infsys.IS with type e = S.t) = struct

  type e = S.t

  let ths = Th.Set.singleton Th.u

  let mk_rename () =
    let u = Name.of_string "u" in
      Term.Var.mk_rename u None Var.Cnstrnt.Unconstrained    

  let name (s, p) a =
    assert(is_pure a);
    if Term.is_var a then
      (a, Jst.dep0, s, p)
    else 
      let hyps = ref Jst.dep0 in
      let s = ref s in
      let p = ref p in
      let add rho = hyps := Jst.dep2 rho !hyps in
      let upd e = 
	let (p', s') =  S.update (!p, !s) e in
	  s := s'; p := p'
    in
      let rec args al =
	Term.mapl term al
      and term a = 
	match a with
	  | Term.Var _ ->  
	      var a
	  | Term.App(f, al, _) ->
	      assert(Sym.theory_of f = Th.u);
	      try
		let (y, rho) = inv (!s, !p) a in
		  add rho; y
	      with
		  Not_found -> 
		    let xl = args al in     
		    let a' =                      
		      if Term.eql xl al then a else 
		      Term.App.mk_app f xl
		    in
		    let u = mk_rename () in
		    let e = Fact.Equal.make (u, a', Jst.dep0) in
		      upd e; u
      and var x = 
	let (y, rho) = P.find !p x in
	  add rho; y
      in
      let x = term a in
	(x, !hyps, !s, !p)

  let rec abstract i a (g, s, p) =
    assert(is_pure a);
    let (x, rho, s, p) = name (s, p) a in
    let e = Fact.Equal.make (x, a, rho) in
    let g = G.instantiate e g in
      (g, s, p)

  and mk_rename () =
    let u = Name.of_string "u" in
      Term.Var.mk_rename u None Var.Cnstrnt.Unconstrained
    
  let merge i e (g, s, p) =
    assert(Fact.Equal.is_pure i e);
    let (a, b, rho) = Fact.Equal.destruct e in
    let (x, tau, s, p) = name (s, p) a in
    let (y, sigma, s, p) = name (s, p) b in
    let e = Fact.Equal.make (x, y, Jst.dep3 rho tau sigma) in
      assert(Fact.Equal.is_var e);
      let p = Partition.merge p e in
      let (p, s) = S.fuse1 (p, s) e in
	(g, s, p)

  let propagate e ((g, s, p) as c) =
    assert(Fact.Equal.is_var e);
    if S.is_empty s then c else
      let (p, s) = S.fuse1 (p, s) e in
	(g, s, p)
      
  let dismerge i d (g, s, p) =
    assert(i = Th.u);
    assert(Fact.Diseq.is_pure Th.u d);
    let (a, b, rho) = Fact.Diseq.destruct d in
    let (x, tau, s, p) = name (s, p) a in
    let (y, sigma, s, p) = name (s, p) b in
    let d = Fact.Diseq.make (x, y, Jst.dep3 rho tau sigma) in
      assert(Fact.Diseq.is_var d);
      let p = Partition.dismerge p d in
	(g, s, p)

  let propagate_diseq _ c = c

  (** No branching. *)
  let branch c = [c]

  (** No normalization. *)
  let normalize c = c
      
end 

(** Congruence closure inference system. *)
module Infsys: (Infsys.IS with type e = S.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = S.t
       let level = "u"
       let eq = S.eq
       let diff = S.diff
       let pp = S.pp
     end)
