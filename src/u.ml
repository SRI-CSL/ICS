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

module P = Partition

let is_flat = function
  | Term.Var _ -> 
      false
  | Term.App(f, xl, _) ->
      Sym.Uninterp.is f && List.for_all Term.is_var xl
      
let is_pure = Term.is_pure Th.u

let map f a = 
  assert(is_flat a);
  let (sym, zl) = Term.App.destruct a in
  let zl' = Term.mapl f zl in
    if zl == zl' then a else Term.App.mk_app sym zl'


module S: Solution.SET0 = Solution.Set

let inv (s, p) =
  Jst.Eqtrans.compose (P.find p) (S.inv s)

let s = ref S.empty

let protected = ref false

let update e =
  if !protected then
    S.update (!Infsys.p, !s) e
  else 
    begin
      s := S.copy !s;
      protected := true;
      S.update (!Infsys.p, !s) e
    end

let mk_rename () =
  let u = Name.of_string "u" in
    Term.Var.mk_rename u None Var.Cnstrnt.Unconstrained    

let name a =
  assert(is_pure a);
  if Term.is_var a then Jst.Eqtrans.id a else 
    let hyps = ref Jst.dep0 in
    let addhyp rho = hyps := Jst.dep2 rho !hyps in
    let rec args al =
	Term.mapl term al
    and term a = 
      match a with
	| Term.Var _ ->  
	    var a
	| Term.App(f, al, _) ->
	    assert(Sym.theory_of f = Th.u);
	    try
	      let (y, rho) = inv (!s, !Infsys.p) a in
		addhyp rho; y
	    with
		Not_found -> 
		  let xl = args al in     
		  let a' =                      
		    if Term.eql xl al then a else 
		      Term.App.mk_app f xl
		  in
		  let u = mk_rename () in
		  let e = Fact.Equal.make u a' Jst.dep0 in
		    update e; u
    and var x = 
      let (y, rho) = P.find !Infsys.p x in
	addhyp rho; y
    in
    let x = term a in
      (x, !hyps)
	

(** Congruence closure inference system. *)
module Infsys: (Infsys.EQ with type e = S.t) = struct

  type e = S.t

  let find a = a          (* [find] is identity! *)
  let inv a = S.inv !s a
  
  let current () = !s

  open Infsys

  let initialize s0 =
    protected := false;
    s := s0   

  let finalize () = !s

  let rec abstract a =
    assert(is_pure a);
    let (x, rho) = name a in
    let e = Fact.Equal.make x a rho in
      G.replace e !g
    
  let rec merge e =
    assert(Fact.Equal.is_pure Th.u e);
    let e = Fact.Equal.map name e in
      assert(Fact.Equal.is_var e);
      Partition.merge !p e;
      fuse e

  and fuse (x, b, rho) = 
    let apply_to_term a = 
      let lookup y = if Term.eq x y then b else y in
      let a' = map lookup a in
	if a == a' then Jst.Eqtrans.id a else (a', rho)
    in
    let apply_to_equality e = 
      update (Fact.Equal.map_rhs apply_to_term e)
    in
      S.Dep.iter !s apply_to_equality x


  let propagate e =
    assert(Fact.Equal.is_var e);
    if not(S.is_empty !s) then
      fuse e
      
  let dismerge d =
    assert(Fact.Diseq.is_pure Th.u d);
    let d = Fact.Diseq.map name d in
      assert(Fact.Diseq.is_var d);
      Partition.dismerge !p d

  let propagate_diseq _ = ()

  (** No branching. *)
  let branch c = raise Not_found

  (** No normalization. *)
  let rec normalize _ = ()


  (** Test if all left hand sides are canonical. *)
  and is_canonical () =
    S.for_all
      (fun (_, a, rho) -> 
	 (try
	    let xl = Term.App.args_of a in
	      List.for_all (Partition.is_canonical !p) xl
	  with
	      Not_found -> false))
      !s
	 
      
end 

(*
(** Congruence closure inference system. *)
module Infsys: (Infsys.EQ with type e = S.t) =
  Infsys.Trace(Infsys0)
    (struct
       type t = S.t
       let level = "u"
       let eq = S.eq
       let diff = S.diff
       let pp = S.pp
     end)
*)
