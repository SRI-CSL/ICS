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
open Status
open Binrel
open Sym

type t =
  | True
  | Equal of Fact.equal
  | Diseq of Fact.diseq
  | In of Fact.cnstrnt
  | False



(** {6 Constructors} *)

let mk_true () = True

let mk_false () = False

let mk_equal e =
  let (a, b, _) = Fact.d_equal e in
    if Term.eq a b then 
      mk_true()
    else if Term.is_interp_const a && Term.is_interp_const b then
      mk_false()
    else
      Equal(e)

let rec mk_in c =
  let (a, c, j) = Fact.d_cnstrnt c in
  if Cnstrnt.is_empty c then
    False
  else 
    match Cnstrnt.d_singleton c with
    | Some(q) ->
	mk_equal (Fact.mk_equal a (Arith.mk_num q) j)
    | None -> 
	(match a with
	   | Term.App(Sym.Arith(Sym.Num(q)), []) -> 
	       if Cnstrnt.mem q c then True else False
	   | _ ->
	       let (a', c') = normalize (a, c) in
		 In(Fact.mk_cnstrnt a' c' j))

and normalize (a, c) =
  match a with
    | Term.App(Arith(Multq(q)), [x]) when not(Mpa.Q.is_zero q) ->
	(x, Cnstrnt.multq (Mpa.Q.inv q) c)
    | Term.App(Arith(Add), m1 :: m2 :: ml) ->
	(match m1, m2 with
	   | Term.App(Arith(Sym.Num(q)), []), 
	     Term.App(Arith(Multq(p)), [x])
	       when not(Mpa.Q.is_zero p) ->   (* [q + p*x +ml in c] iff *)
	       let pinv = Mpa.Q.inv p in      (* [x + 1/p * ml in 1/ p * (c - q)] *)
	       let c' = Cnstrnt.multq pinv (Cnstrnt.addq (Q.minus q) c) in
	       let a' = Arith.mk_add x (Arith.mk_multq pinv (Arith.mk_addl ml)) in
		 (a', c')
	   | _ -> (a, c))
    | _ -> (a, c)

let normalize =
  Trace.func "foo" "Normalize" Term.pp_in Term.pp_in normalize

let rec mk_diseq d =
  let (a, b, j) = Fact.d_diseq d in
    if Term.eq a b then 
      mk_false()
    else if Term.is_interp_const a && Term.is_interp_const b then
      mk_true()
    else if Term.eq a (Boolean.mk_true()) then
      mk_equal(Fact.mk_equal b (Boolean.mk_false()) j)
    else if Term.eq a (Boolean.mk_false()) then
      mk_equal(Fact.mk_equal b (Boolean.mk_true()) j)
    else if Term.eq b (Boolean.mk_true()) then
      mk_equal(Fact.mk_equal a (Boolean.mk_false()) j)
    else if Term.eq b (Boolean.mk_false()) then
      mk_equal(Fact.mk_equal a (Boolean.mk_true()) j)
    else
      match Arith.d_num a, Arith.d_num b with
	| Some(q), _ -> 
	    mk_in(Fact.mk_cnstrnt b (Cnstrnt.mk_diseq q) j)
	| _, Some(p) -> 
	    mk_in(Fact.mk_cnstrnt a (Cnstrnt.mk_diseq p) j)
	| None, None -> 
	    Diseq(Fact.mk_diseq a b j)



(** {6 Pretty-printing} *)

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | False -> Pretty.string fmt "False"
  | Equal(e) -> Fact.pp_equal fmt e
  | Diseq(d) -> Fact.pp_diseq fmt d
  | In(c) -> Fact.pp_cnstrnt fmt c


(** {6 Set of atoms} *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare a b =
      if a = b then 0 else Pervasives.compare a b
  end)

