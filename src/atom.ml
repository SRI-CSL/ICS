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
  | Equal of Term.t * Term.t
  | Diseq of Term.t * Term.t
  | Nonneg of Term.t
  | Pos of Term.t
  | False

(** Following is not a particularly good `hash' function,
  but serves its purpose as a quick failure criteria for 
  equality test. *)
let hash = function
  | True -> 0
  | Equal(_, b) -> Term.hash b
  | Diseq(_, b) -> Term.hash b
  | Nonneg(a) -> Term.hash a
  | Pos(a) -> Term.hash a
  | False -> 1

let rec eq a b =
  (hash a = hash b) &&    (* quick failure test *)
  match a, b with
    | True, True -> true
    | False, False -> true
    | Equal(a1, b1), Equal(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
    | Diseq(a1, b1), Diseq(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
    | Nonneg(a), Nonneg(b) -> Term.eq a b
    | Pos(a), Pos(b) -> Term.eq a b
    | _ -> false

let compare a b =
  if eq a b then 0 else if hash a < hash b then -1 else 1


let is_true = function True -> true | _ -> false

let is_false = function False -> true | _ -> false


(** {6 Set of atoms} *)

type atom = t

module Set = Set.Make(
  struct
    type t = atom
    let compare = compare
  end)


(** {6 Constructors} *)

let mk_true = True

let mk_false = False

let mk_equal (a, b) =
  if Term.eq a b then mk_true else 
    let (a', b') = Term.orient(Nonlin.crossmultiply (a, b)) in
      Equal(a', b')

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
      let (a', b') = Term.orient(Nonlin.crossmultiply (a, b)) in
	Diseq(a', b')

let mk_nonneg a =
  try
    let q = Arith.d_num a in
      if Mpa.Q.is_nonneg q then mk_true else mk_false
  with
      Not_found -> 
	(try
	   let (q, x) = Arith.d_multq a in
	     if Mpa.Q.is_nonneg q then 
	       Nonneg(x)
	     else 
	       Nonneg(a)
	 with
	     Not_found -> Nonneg(a))

let mk_pos a =
  try
    let q = Arith.d_num a in
      if Mpa.Q.is_pos q then mk_true else mk_false
  with
      Not_found ->
	if Arith.is_int a then         (* [a > 0] iff [a - 1 >= 0] for [a] an integer. *)
	  mk_nonneg (Arith.mk_decr a)
	else 
	  Pos(a)

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
  | Equal(a, b) -> Pretty.infix Term.pp "=" Term.pp fmt (a, b)
  | Diseq(a, b) -> Pretty.infix Term.pp "<>" Term.pp fmt (a, b)
  | Nonneg(a) -> Term.pp fmt a; Pretty.string fmt " >= 0"
  | Pos(a) ->  Term.pp fmt a; Pretty.string fmt " > 0"
  

(** {6 Negations of atoms} *)

let is_negatable _ = true

let negate = function
  | True -> False
  | False -> True
  | Equal(a, b) -> mk_diseq (a, b)
  | Diseq(a, b) -> mk_equal (a, b)
  | Nonneg(a) -> mk_pos (Arith.mk_neg a)  (* [not(a >= 0)] iff [-a > 0] *)
  | Pos(a) -> mk_nonneg (Arith.mk_neg a)  (* [not(a > 0)] iff [-a >= 0] *)

let _ = Callback.register "atom_negate" negate


(** {6 Miscellaneous} *)

let vars_of = function
  | True -> Term.Set.empty
  | False -> Term.Set.empty
  | Equal(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Diseq(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Nonneg(a) -> Term.vars_of a
  | Pos(a) -> Term.vars_of a

let list_of_vars a = 
  Term.Set.elements (vars_of a)


let occurs ((x, a) as p) =
  let rec term_occurs = function
    | Term.Var _ as y -> Term.eq x y
    | Term.App(_, sl, _) -> List.exists term_occurs sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(a, b) -> term_occurs a || term_occurs b
      | Diseq(a, b) -> term_occurs a || term_occurs b
      | Nonneg(a) -> term_occurs a
      | Pos(a) -> term_occurs a

let is_connected a b =
  let rec term_is_connected = function
    | Term.Var _ as x -> occurs (x, b)
    | Term.App(_, sl, _) -> List.exists term_is_connected sl
  in
    match a with
      | True -> false
      | False -> false
      | Equal(a, b) -> term_is_connected a || term_is_connected b
      | Diseq(a, b) -> term_is_connected a || term_is_connected b
      | Nonneg(a) -> term_is_connected a
      | Pos(a) -> term_is_connected a




