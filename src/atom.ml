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
  | Less of Term.t * bool * Term.t     (* represents [a < q] or [a <= q]. *)
  | Greater of Term.t * bool * Term.t  (* represents [a > q] or [a >= q]. *)
  | In of Term.t * Dom.t               (* represents [a in d] *)
  | False

let eq a b =
  match a, b with
    | True, True -> true
    | False, False -> true
    | Equal(a1, b1), Equal(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
    | Diseq(a1, b1), Diseq(a2, b2) -> Term.eq a1 a2 && Term.eq b1 b2
    | Less(a1, alpha1, b1), Less(a2, alpha2, b2) -> Term.eq a1 a2 && alpha1 = alpha2 && Term.eq b1 b2
    | Greater(a1, alpha1, b1), Greater(a2, alpha2, b2) -> Term.eq a1 a2 && alpha1 = alpha2 && Term.eq b1 b2
    | In(a1, d1), In(a2, d2) -> Term.eq a1 a2 && Dom.eq d1 d2
    | _ -> false


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
	  

let mk_less (a, alpha, b) =
  match Arith.mk_less (a, alpha, b) with
    | Arith.True -> True
    | Arith.False -> False
    | Arith.Less(x, beta, c) -> Less(x, beta, c)
    | Arith.Greater(x, beta, c) -> Greater(x, beta, c)

let mk_greater (a, alpha, b) =              (* [a >(=) b] *)
  mk_less (b, alpha, a)

let mk_in (a, d) =
  match Arith.d_num a with
    | Some(q) -> 
	if Q.is_integer q then
	  (match d with
	     | (Dom.Int | Dom.Real) -> True
	     | Dom.Nonint -> False)
	else 
	  (match d with
	     | (Dom.Nonint | Dom.Real) -> True
	     | Dom.Int -> False)
    | _ ->
	In(a, d)
  

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
  | Less(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " <= " else " < ");
      Term.pp fmt b
  | Greater(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " >= " else " > ");
      Term.pp fmt b;
  | In(a, d) ->
      Term.pp fmt a;
      Pretty.string fmt " in ";
      Dom.pp fmt d

(** {6 Negations of atoms} *)

let is_negatable = function
  | In _ -> false
  | _ -> true

let _ = Callback.register "atom_is_negatable" is_negatable

let negate = function
  | True -> mk_false
  | False -> mk_true
  | Equal(a, b) -> mk_diseq (a, b)
  | Diseq(a, b) -> mk_equal (a, b)
  | Less(a, kind, q) -> mk_greater (a, not kind, q)
  | Greater(a, kind, q) -> mk_less (a, not kind, q)
  | a -> 
      let str = Pretty.to_string pp a in
      raise (Invalid_argument ("Atom " ^ str ^ " not negatable."))

let _ = Callback.register "atom_negate" negate

let of_ineq = function
  | Arith.True -> True
  | Arith.False -> False
  | Arith.Less(x, alpha, a) -> Less(x, alpha, a)
  | Arith.Greater(x, alpha, a) -> Greater(x, alpha, a)


(** {6 Miscellaneous} *)

let vars_of = function
  | True -> Term.Set.empty
  | False -> Term.Set.empty
  | Equal(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Diseq(a, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Less(a, _, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | Greater(a, _, b) -> Term.Set.union (Term.vars_of a) (Term.vars_of b)
  | In(a, _) -> Term.vars_of a

let list_of_vars a = 
  Term.Set.elements (vars_of a)

let _ = Callback.register "atom_list_of_vars" list_of_vars

let is_connected a b =
  let xs = vars_of a 
  and ys = vars_of b in
    not(Term.Set.is_empty (Term.Set.inter xs ys))
  
