
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
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | In of Cnstrnt.t * Term.t
  | False

let eq a b =
  match a, b with
    | True, True -> true  
    | False, False -> true
    | Equal(x1,y1), Equal(x2,y2) ->  (* equalities are ordered. *)
	Term.eq x1 x2 && Term.eq y1 y2
    | Diseq(x1,y1), Diseq(x2,y2) ->
	Term.eq x1 x2 && Term.eq y1 y2
    | In(c1,x1), In(c2,x2) -> 
	Term.eq x1 x2 && Cnstrnt.eq c1 c2
    | _ -> 
	false

(*s Constructors. *)

let mk_true = True

let mk_false = False

let mk_equal a b =
  if Term.eq a b then 
    mk_true
  else if Term.is_interp_const a && Term.is_interp_const b then
    mk_false
  else
    let a',b' = Term.orient(a,b) in
    Equal(a',b')       (* Larger Term on rhs *)

let mk_in c a =
  if Cnstrnt.is_empty c then
    False
  else 
    match Cnstrnt.d_singleton c with
    | Some(q) ->
	mk_equal a (Arith.mk_num q)
    | None -> 
	In(c, a)

let rec mk_diseq a b =
  if Term.eq a b then 
    mk_false
  else if Term.is_interp_const a && Term.is_interp_const b then
    mk_true
  else if Term.eq a Boolean.mk_true then
    mk_equal b Boolean.mk_false
  else if Term.eq a Boolean.mk_false then
    mk_equal b Boolean.mk_true
  else if Term.eq b Boolean.mk_true then
    mk_equal a Boolean.mk_false
  else if Term.eq b Boolean.mk_false then
    mk_equal a Boolean.mk_true
  else
    match Arith.d_num a, Arith.d_num b with
      | Some(q), _ -> 
	  mk_in (Cnstrnt.mk_diseq q) b
      | _, Some(p) -> 
	  mk_in (Cnstrnt.mk_diseq p) a
      | None, None -> 
	  let a',b' = Term.orient(a,b) in
	  Diseq(a',b')

(*s Transforming terms in an atom *)

let map f a =
  match a with
    | True -> True
    | Equal(x,y) -> mk_equal (f x) (f y)
    | Diseq(x,y) -> mk_diseq (f x) (f y)
    | In(c,x) -> mk_in c (f x)
    | False -> False

(*s Constructing inequalities. *)

let rec mk_lt a b =
  lower (Q.lt, Cnstrnt.mk_lt Dom.Real, Cnstrnt.mk_gt Dom.Real) (a,b)
	
and mk_le a b =
  lower (Q.le, Cnstrnt.mk_le Dom.Real, Cnstrnt.mk_ge Dom.Real) (a,b)

and lower (f,less,greater) (a,b) =
  let (q, ml) = Arith.poly_of (Arith.mk_sub a b) in 
  match ml with
    | [] ->                                  
	if f q Q.zero then mk_true else mk_false
    | m :: ml ->                                   (*s case [p * x + ml < q'] *)
	let (p,x) = Arith.mono_of m in          (*s case [q + p * x + ml < 0] *)
	assert(not(Q.is_zero p));        
	let rel = (if Q.gt p Q.zero then less else greater) in
	let c = rel (Q.minus (Q.div q p)) in
	let ml' = List.map (Arith.mk_multq (Q.inv p)) ml in
	let a = Arith.of_poly Q.zero ml' in
	mk_in c (Arith.mk_add x a)


(*s Comparison  of atoms. *)

let (<<<) a b =
  match a, b with
    | Equal _, (Diseq _| In _) -> false
    | (Diseq _| In _), Equal _ -> true
    | In(c,_), In(d,_) -> not(Cnstrnt.sub c d)
    | _ -> Pervasives.compare a b <= 0


(*s Pretty-printing. *)

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | False -> Pretty.string fmt "False"
  | Equal(x,y) -> Term.pp_equal fmt (x,y)
  | Diseq(x,y) -> Term.pp_diseq fmt (x,y)
  | In(c,x) -> Term.pp_in fmt (x,c)

(*s Set of atoms. *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare a b =
      if eq a b then 0 else Pervasives.compare a b
  end)
