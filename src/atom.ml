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
  Trace.msg "foo" "Atom.mk_in" c Fact.pp_cnstrnt;
  let (a, c, j) = Fact.d_cnstrnt c in
    match Cnstrnt.status c with
      | Status.Empty ->
	  False
      | Status.Singleton(q) ->
	  mk_equal (Fact.mk_equal a (Arith.mk_num q) j)
      | _ ->
	  (match a with
	     | Term.App(Sym.Arith(Sym.Num(q)), []) -> 
		 if Cnstrnt.mem q c then True else False
	     | _ ->
		 let (a', c') = Arith.normalize (a, c) in
		   In(Fact.mk_cnstrnt a' c' j))

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

