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


(** {6 Atoms} *)

type t =
  | True
  | Equal of Term.Equal.t
  | Diseq of Term.Diseq.t
  | Nonneg of Term.Nonneg.t
  | Pos of Term.Pos.t
  | False

let cmp a b =
  match a, b with
    | True, True -> 0
    | False, False -> 0
    | Equal(e1), Equal(e2) -> Term.Equal.compare e1 e2
    | Diseq(d1), Diseq(d2) -> Term.Diseq.compare d1 d2
    | Nonneg(c1), Nonneg(c2) -> Term.Nonneg.compare c1 c2
    | Pos(c1), Pos(c2) -> Term.Pos.compare c1 c2
    | _ -> Pervasives.compare a b

let eq a b = (cmp a b = 0)

let is_true = function True -> true | _ -> false

let is_false = function False -> true | _ -> false


(** {6 Set of atoms} *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare = cmp
  end)


(** {6 Constructors} *)

let mk_true = True

let mk_false = False

let mk_equal (a, b) =
  if Term.eq a b then mk_true else 
    let (a', b') = Nonlin.crossmultiply (a, b) in
      Equal(Term.Equal.make (a', b'))

let mk_diseq (a, b) =
  if Term.eq a b then mk_false else
    if Term.eq a Boolean.mk_true then
      mk_equal (b, Boolean.mk_false)
    else if Term.eq a Boolean.mk_false then
      mk_equal (b, Boolean.mk_true)
    else if Term.eq b Boolean.mk_true then
      mk_equal (a, Boolean.mk_false)
    else if Term.eq b Boolean.mk_false then
      mk_equal (a, Boolean.mk_true)
    else
      let (a', b') = Nonlin.crossmultiply (a, b) in
	Diseq(Term.Diseq.make (a', b'))

let mk_nonneg a =
  match a with
    | Term.App(Sym.Arith(Sym.Num(q)), []) ->
	if Mpa.Q.is_nonneg q then mk_true else mk_false
    | _ -> 
	Nonneg(Term.Nonneg.make a)

let mk_pos a =
  match a with
    | Term.App(Sym.Arith(Sym.Num(q)), []) ->
	if Mpa.Q.is_pos q then mk_true else mk_false
    | _ -> 
	if Arith.is_int a then         (* [a > 0] iff [a - 1 >= 0] for [a] an integer. *)
	  mk_nonneg (Arith.mk_decr a)
	else 
	  Pos(Term.Pos.make a)

let mk_neg a = mk_pos (Arith.mk_neg a)
let mk_nonpos a = mk_nonneg (Arith.mk_neg a)

let mk_ge (a, b) = mk_nonneg (Arith.mk_sub a b)  (* [a >= b] iff [a - b >= 0] *)
let mk_gt (a, b) = mk_pos (Arith.mk_sub a b)     (* [a > b] iff [a - b > 0] *)
let mk_le (a, b) = mk_nonneg (Arith.mk_sub b a)  (* [a <= b] iff [b - a >= 0] *)
let mk_lt (a, b) = mk_pos (Arith.mk_sub b a)     (* [a < b] iff [b - a > 0] *)


let apply rho = failwith "atom.apply: to do" 

(** {6 Pretty-printing} *)

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | False -> Pretty.string fmt "False"
  | Equal(e) -> Term.Equal.pp fmt e
  | Diseq(d) -> Term.Diseq.pp fmt d
  | Nonneg(nn) -> Term.Nonneg.pp fmt nn
  | Pos(p) -> Term.Pos.pp fmt p
  

(** {6 Negations of atoms} *)

let is_negatable _ = true

let negate = function
  | True -> False
  | False -> True
  | Equal(e) -> mk_diseq (Term.Equal.destruct e)
  | Diseq(d) -> mk_equal (Term.Diseq.destruct d)
  | Nonneg(a) -> mk_pos (Arith.mk_neg a)  (* [not(a >= 0)] iff [-a > 0] *)
  | Pos(a) -> mk_nonneg (Arith.mk_neg a)  (* [not(a > 0)] iff [-a >= 0] *)

let _ = Callback.register "atom_negate" negate


(** {6 Miscellaneous} *)

let vars_of = function
  | True -> Term.Set.empty
  | False -> Term.Set.empty
  | Equal(e) -> Term.Set.union (Term.vars_of (Term.Equal.lhs e)) (Term.vars_of (Term.Equal.rhs e))
  | Diseq(d) -> Term.Set.union (Term.vars_of (Term.Equal.lhs d)) (Term.vars_of (Term.Equal.rhs d))
  | Nonneg(nn) -> Term.vars_of (Term.Nonneg.term_of nn)
  | Pos(p) -> Term.vars_of (Term.Pos.term_of p)

let list_of_vars a = 
  Term.Set.elements (vars_of a)


let occurs ((x, a) as p) =
  let rec term_occurs = function
    | Term.Var _ as y -> Term.eq x y
    | Term.App(_, sl) -> List.exists term_occurs sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(e) -> term_occurs (Term.Equal.lhs e) || term_occurs (Term.Equal.rhs e)
      | Diseq(d) -> term_occurs (Term.Equal.lhs d) || term_occurs (Term.Equal.rhs d)
      | Nonneg(nn) -> term_occurs (Term.Nonneg.term_of nn)
      | Pos(p) -> term_occurs (Term.Pos.term_of p)

let is_connected a b =
  let rec term_is_connected = function
    | Term.Var _ as x -> occurs (x, b)
    | Term.App(_, sl) -> List.exists term_is_connected sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(a, b) -> term_is_connected a || term_is_connected b
      | Diseq(a, b) -> term_is_connected a || term_is_connected b
      | Nonneg(a) -> term_is_connected a
      | Pos(a) -> term_is_connected a




