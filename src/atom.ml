
(*i
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
 * 
 * Author: Harald Ruess
 i*)

(*i*)
open Mpa
open Status
open Binrel
open Sym
(*i*)

type t =
  | True
  | Equal of Fact.equal
  | Diseq of Fact.diseq
  | In of Fact.cnstrnt
  | False



(*s Constructors. *)

let mk_true () = True

let mk_false () = False

let mk_equal e =
  let (a, b, _) = Fact.d_equal e in
    if Term.eq a b then 
      mk_true()
    else if Term.is_interp_const a && Term.is_interp_const b then
      mk_false()
    else
      Equal(e)    (* Larger Term on rhs *)

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
	       if Cnstrnt.mem q c then True else raise Exc.Inconsistent
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


(*s Constructing inequalities. *)

let rec mk_lt a b =
  lower (Q.lt, Cnstrnt.mk_lt Dom.Real, Cnstrnt.mk_gt Dom.Real) (a,b)
	
and mk_le a b =
  lower (Q.le, Cnstrnt.mk_le Dom.Real, Cnstrnt.mk_ge Dom.Real) (a,b)

and lower (f,less, greater) (a,b) =
  let (q, ml) = Arith.poly_of (Arith.mk_sub a b) in 
  match ml with
    | [] ->                                  
	if f q Q.zero then mk_true() else mk_false()
    | m :: ml ->                                   (*s case [p * x + ml < q'] *)
	let (p,x) = Arith.mono_of m in          (*s case [q + p * x + ml < 0] *)
	assert(not(Q.is_zero p));        
	let rel = (if Q.gt p Q.zero then less else greater) in
	let c = rel (Q.minus (Q.div q p)) in
	let ml' = List.map (Arith.mk_multq (Q.inv p)) ml in
	let a = Arith.of_poly Q.zero ml' in
	  mk_in (Fact.mk_cnstrnt (Arith.mk_add x a) c None)



(*s Pretty-printing. *)

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | False -> Pretty.string fmt "False"
  | Equal(e) -> Fact.pp_equal fmt e
  | Diseq(d) -> Fact.pp_diseq fmt d
  | In(c) -> Fact.pp_cnstrnt fmt c

(*s Set of atoms. *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare a b =
      if a = b then 0 else Pervasives.compare a b
  end)

