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
  | Equal of Term.t * Term.t           (* represents [a = b]. *)
  | Diseq of Term.t * Term.t           (* represents [a <> b]. *)
  | In of Term.t * Interval.t          (* represents [a in d] *)
  | False

let eq a b =
  match a, b with
    | True, True -> 
	true
    | False, False -> 
	true
    | Equal(a1, b1), Equal(a2, b2) -> 
	Term.eq a1 a2 && Term.eq b1 b2
    | Diseq(a1, b1), Diseq(a2, b2) -> 
	Term.eq a1 a2 && Term.eq b1 b2
    | In(a1, d1), In(a2, d2) -> 
	Term.eq a1 a2 && Interval.eq d1 d2
    | _ -> 
	false


(** {6 Set of atoms} *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare a b =
      if a = b then 0 else Pervasives.compare a b
  end)

open Term

(** {6 Constructors} *)

let mk_true = True

let mk_false = False

let mk_equal (a, b) =
  if Term.eq a b then 
    mk_true
  else if Term.is_interp_const a && Term.is_interp_const b then
    mk_false
  else
    Equal(a, b)


let rec mk_diseq (a, b) =
  if Term.eq a b then 
    mk_false
  else if Term.is_interp_const a && Term.is_interp_const b then
    mk_true
  else if Term.eq a Boolean.mk_true then
    mk_equal (b, Boolean.mk_false)
  else if Term.eq a Boolean.mk_false then
    mk_equal (b, Boolean.mk_true)
  else if Term.eq b Boolean.mk_true then
    mk_equal(a, Boolean.mk_false)
  else if Term.eq b Boolean.mk_false then
    mk_equal (a, Boolean.mk_true)
  else
    Diseq(a, b)
	  
  
let mk_in (a, i) =
  if Interval.is_empty i then
    False
  else match Arith.linearize a with
    | Arith.Const(q) -> 
	if Interval.mem q i then True else False
    | Arith.Linear(p, q, x, b) ->   (* [p + q * x + b in i] *)
	assert(not(Q.is_zero q));
	if Q.is_pos q then          (* [x + 1/q * b in 1/q*(-p + i) ] *)
	  let a' = Arith.mk_add x (Arith.mk_multq (Q.inv q) b)
	  and i' = Interval.multq (Q.inv q) (Interval.addq (Q.minus p) i) in
	    In(a', i')
	else 
	(*  try
	    let ci = Interval.complement i in
	    let a' = Arith.mk_add x (Arith.mk_multq (Q.inv q) b)
	    and i' = Interval.multq (Q.inv q) (Interval.addq (Q.minus p) ci) in
	      In(a', i') 
	  with
	      Invalid_argument _ -> *)
		let a' = Arith.mk_add (Arith.mk_multq q x) b
		and i' = Interval.addq (Q.minus p) i in
		  In(a', i')
	

(** {6 Pretty-printing} *)

let pp fmt = function
  | True -> 
      Pretty.string fmt "True"
  | False -> 
      Pretty.string fmt "False"
  | Equal(a, b) ->
      Term.pp fmt a;
      Pretty.string fmt " = ";
      Term.pp fmt b
  | Diseq(a, b) -> 
      Term.pp fmt a;
      Pretty.string fmt " <> ";
      Term.pp fmt b
  | In(a, i) ->
      Term.pp fmt a;
      Pretty.string fmt " in ";
      Interval.pp fmt i

let mk_in =
  Trace.func "foo" "Atom.mk_in" (Pretty.pair Term.pp Interval.pp) pp mk_in
  

(** {6 Negations of atoms} *)

let is_negatable = function
  | In(_, i) -> Interval.is_complementable i
  | _ -> true

let negate = function
  | True -> mk_false
  | False -> mk_true
  | Equal(a, b) -> mk_diseq (a, b)
  | Diseq(a, b) -> mk_equal (a, b)
  | In(a, i) when Interval.is_complementable i ->
      mk_in (a, Interval.complement i)
  | (In(x, i) as a) ->
      let str = Pretty.to_string pp a in
	raise (Invalid_argument ("Atom " ^ str ^ " not negatable."))

let _ = Callback.register "atom_negate" negate


(** {6 Miscellaneous} *)

let vars_of = function
  | True -> Term.Set.empty
  | False -> Term.Set.empty
  | Equal(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Diseq(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | In(a, _) -> Term.vars_of a

let list_of_vars a = 
  Term.Set.elements (vars_of a)

let occurs x a =
  let rec term_occurs = function
    | Var(y) -> Var.eq x y
    | App(_, sl) -> List.exists term_occurs sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(s, t) -> term_occurs s || term_occurs t
      | Diseq(s, t) -> term_occurs s || term_occurs t
      | In(s, _) -> term_occurs s

let is_connected a b =
  let rec term_is_connected = function
    | Var(x) -> occurs x b
    | App(_, sl) -> List.exists term_is_connected sl
  in
  let terms_is_connected (s, t) = 
    term_is_connected s || term_is_connected t
  in
    match a with
      | True -> false
      | False -> false
      | Equal(s, t) -> terms_is_connected (s, t)
      | Diseq(s, t) -> terms_is_connected (s, t)
      | In(s, _) -> term_is_connected s
