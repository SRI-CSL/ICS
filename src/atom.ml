
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
open Term
open Hashcons
open Mpa
open Status
open Binrel
open Sym
open Term
(*i*)

type t =
  | True
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | In of Number.t * Term.t
  | False

let eq a b =
  match a, b with
    | True, True -> true  
    | False, False -> true
    | Equal(x1,y1), Equal(x2,y2) ->  (* equalities are ordered. *)
	x1 === x2 && y1 === y2
    | Diseq(x1,y1), Diseq(x2,y2) ->
	x1 === x2 && y1 === y2
    | In(c1,x1), In(c2,x2) -> 
	x1 === x2 && Number.eq c1 c2
    | _ -> 
	false

(*s Constructors. *)

let mk_true = True

let mk_false = False

let mk_equal a b =
  if a === b then 
    mk_true
  else if is_interp_const a && is_interp_const b then
    mk_false
  else
    let a',b' = order a b in
    Equal(a',b')       (* Larger Term on rhs *)

let mk_in c a =
  match Number.analyze c with
    | Status.Singleton(u) ->
	mk_equal a (Linarith.mk_num u)
    | Status.Empty ->
	False
    | Status.Full ->
	True
    | Status.Other ->
	In(c, a)

let rec mk_diseq a b =
  if a === b then 
    mk_false
  else if is_interp_const a && is_interp_const b then
    mk_true
  else if a === Bool.mk_tt then
    mk_equal b Bool.mk_ff
  else if a === Bool.mk_ff then
    mk_equal b Bool.mk_tt
  else if b === Bool.mk_tt then
    mk_equal a Bool.mk_ff
  else if b === Bool.mk_ff then
    mk_equal a Bool.mk_tt
  else
    match Linarith.d_num a, Linarith.d_num b with
      | Some(q), _ -> 
	  mk_in (Number.mk_diseq q) b
      | _, Some(p) -> 
	  mk_in (Number.mk_diseq p) a
      | None, None -> 
	  let a',b' = order a b in
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
  lower (Q.lt, Number.mk_lt Dom.Real, Number.mk_gt Dom.Real) (a,b)
	
and mk_le a b =
  lower (Q.le, Number.mk_le Dom.Real, Number.mk_ge Dom.Real) (a,b)

and lower (f,less,greater) (a,b) =
  let (q, ml) = Linarith.poly_of (Linarith.mk_sub a b) in 
  match ml with
    | [] ->                                  
	if f Q.zero q then mk_true else mk_false
    | m :: ml ->                                   (*s case [p * x + ml < q'] *)
	let (p,x) = Linarith.mono_of m in          (*s case [q + p * x + ml < 0] *)
	assert(not(Q.is_zero p));        
	let rel = (if Q.gt p Q.zero then less else greater) in
	let c = rel (Q.minus (Q.div q p)) in
	let ml' = List.map (Linarith.mk_multq (Q.inv p)) ml in
	let a = Linarith.of_poly Q.zero ml' in
	mk_in c (Linarith.mk_add x a)


(*s Comparison  of atoms. *)

let (<<<) a b =
  match a, b with
    | Equal _, (Diseq _| In _) -> false
    | (Diseq _| In _), Equal _ -> true
    | In(c,_), In(d,_) -> not(Number.sub c d)
    | _ -> Pervasives.compare a b <= 0


(*s Make negation. *)

let mk_neg a =
  match a with
    | True -> False
    | False -> True
    | Equal(a,b) -> mk_diseq a b
    | Diseq(a,b) -> mk_equal a b
    | In(c,x) -> mk_in (Number.compl c) x

(*s Set of atoms. *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom 
    let compare a b =
      if eq a b then 0 else Pervasives.compare a b
  end)

