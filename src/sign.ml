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

type t =
  | F
  | Zero
  | Pos
  | Neg
  | Nonneg
  | Nonpos
  | T

let eq s t = (s = t)


(** {6 Constructors} *)

let of_q q =
  let res = Q.compare q Q.zero in
    if res = 0 then Zero
    else if res > 0 then Pos
    else Neg

(** {6 Connectives} *)

let inter s t =
  match s, t with
    | F, _ -> F
    | _, F -> F
    | T, _ -> t
    | _, T -> s
    | Zero, (Pos | Neg) -> F
    | Zero, _ -> Zero
    | (Pos | Neg), Zero -> F
    | _, Zero -> Zero
    | Pos, Pos -> Pos
    | Pos, Nonneg -> Pos
    | Pos, _ -> F
    | Nonneg, Pos -> Pos
    | _, Pos -> F
    | Neg, Neg -> Neg
    | Neg, Nonpos -> Neg
    | Neg, _ -> F
    | Nonpos, Neg -> Neg
    | _, Neg -> F
    | Nonneg, Nonneg -> Nonneg
    | Nonneg, Nonpos -> Zero
    | Nonpos, Nonneg -> Zero
    | Nonpos, Nonpos -> Nonpos


let complement = function
  | F -> T
  | Pos -> Nonpos
  | Neg -> Nonneg
  | Nonneg -> Neg
  | Nonpos -> Pos
  | T -> F
  | _ -> raise (Invalid_argument "not complementable")


(** {6 Predicates} *)

let sub s t = 
  match s, t with
    | F, _ -> true
    | _, F -> false
    | _, T -> true
    | T, _ -> false
    | Zero, (Zero | Nonpos | Nonneg) -> true
    | Zero, _ -> false
    | Neg, (Neg | Nonpos) -> true
    | Neg, _ -> false
    | Pos, (Pos | Nonneg) -> true
    | Pos, _ -> false
    | Nonneg, Nonneg -> true
    | Nonneg, _ -> false
    | Nonpos, Nonpos -> true
    | Nonpos, _ -> false

let cmp s t =
  if eq s t then 0
  else if sub s t then -1
  else if sub t s then 1
  else Pervasives.compare s t

let disjoint s t =
  match s, t with
    | F, _ -> true
    | _, F -> true
    | Zero,  (Neg | Pos) -> true
    | Neg, (Zero | Pos | Nonneg) -> true
    | Pos, (Zero | Neg | Nonpos) -> true
    | Nonneg, Neg -> true
    | Nonpos, Pos -> true
    | _ -> false

let mem q = function
  | F -> false
  | Zero -> Q.equal q Q.zero
  | Pos -> Q.gt q Q.zero
  | Neg -> Q.lt q Q.zero
  | Nonneg -> Q.ge q Q.zero
  | Nonpos -> Q.le q Q.zero
  | T -> true

let complementable = function
  | Zero -> false
  | _ -> true


(** {6 Pretty-printing} *)

let pp fmt = function
  | Zero -> Pretty.string fmt "Zero"
  | Pos -> Pretty.string fmt "Pos"
  | Neg -> Pretty.string fmt "Neg"
  | Nonneg -> Pretty.string fmt "Nonneg"
  | Nonpos -> Pretty.string fmt "Nonpos"
  | F -> Pretty.string fmt "Bot"
  | T -> Pretty.string fmt "Real"


(** {6 Sign abstraction} *)

let num q = 
  let cmp = Q.compare q Q.zero in
    if cmp = 0 then Zero
    else if cmp > 0 then Pos
    else Neg

let add s t =
  match s, t with
    | Zero, _ -> t
    | _, Zero -> s
    | F, _ -> F
    | _, F -> F
    | T, _ -> T
    | _, T -> T
    | Pos, (Pos | Nonneg) -> Pos
    | Pos, (Neg | Nonpos) -> T
    | Neg, (Neg | Nonpos) -> Neg
    | Neg, (Pos | Nonneg) -> T
    | Nonneg, Nonneg -> Nonneg
    | Nonneg, Pos -> Pos
    | Nonneg, (Neg | Nonpos) -> T
    | Nonpos, Nonpos -> Nonpos
    | Nonpos, Neg -> Neg
    | Nonpos, (Pos | Nonneg) -> T

let rec addl = function
  | [] -> Zero
  | [s] -> s
  | [s; t] -> add s t
  | s :: sl -> add s (addl sl)

let multq q s = 
  if Q.is_zero q then Zero
  else if Q.is_one q then s 
  else match s with
    | Zero -> Zero
    | T -> T
    | F -> F
    | Pos -> if Q.is_pos q then Pos else Neg
    | Neg -> if Q.is_pos q then Neg else Pos
    | Nonneg -> if Q.is_pos q then Nonneg else Nonpos
    | Nonpos -> if Q.is_pos q then Nonpos else Nonneg

let mult s t =
  match s, t with
    | F, _ -> F
    | _, F -> F
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | T, _ -> T
    | _, T -> T
    | Pos, Pos -> Pos
    | Pos, Nonneg -> Nonneg
    | Pos, Neg -> Neg
    | Pos, Nonpos -> Nonpos
    | Neg, Neg -> Pos
    | Neg, Pos -> Neg
    | Neg, Nonpos -> Nonneg
    | Neg, Nonneg -> Nonpos
    | Nonpos, Pos -> Nonpos
    | Nonpos, Neg -> Nonneg
    | Nonpos, Nonpos -> Nonneg
    | Nonpos, Nonneg -> Nonpos
    | Nonneg, Pos -> Nonneg
    | Nonneg, Neg -> Nonpos
    | Nonneg, Nonneg -> Nonneg
    | Nonneg, Nonpos -> Nonneg

let rec multl = function
  | [] -> Pos
  | [c] -> c
  | [c; d] -> mult c d
  | c :: cl -> mult c (multl cl)

let inv = function
  | Nonneg -> Pos
  | Nonpos -> Neg
  | Zero -> F
  | sgn -> sgn

let expt n s =
  if n = 0 then 
    Pos
  else if n = 1 then 
    s
  else if n = (-1) then
    inv s
  else if n = 2 then 
    inter Nonneg (mult s s)
  else
    T

let div s t =
  match s, t with
    | F, _ -> F
    | _, F -> F
    | _, Zero -> F
    | Zero, _ -> Zero
    | T, _ -> T
    | _, T -> T
    | Pos, (Pos | Nonneg) -> Pos
    | Pos, (Neg | Nonpos) -> Neg
    | Neg, (Pos | Nonneg) -> Neg
    | Neg, (Neg | Nonpos) -> Pos 
    | Nonneg, (Pos | Nonneg) -> Nonneg
    | Nonneg, (Neg | Nonpos) -> Nonpos
    | Nonpos, (Pos | Nonneg) -> Nonpos
    | Nonpos, (Neg | Nonpos) -> Nonneg
