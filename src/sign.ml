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
open Sym
open Term

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
  | Nonneg -> Pos
  | Nonpos -> Neg
  | T -> F
  | _ -> raise (Invalid_argument "not complementable")


(** {6 Predicates} *)

let sub s t = 
  match s, t with
    | F, _ -> true
    | _, F -> false
    | _, T -> true
    | T, _ -> false
    | Zero, Zero -> true
    | Zero, _ -> false
    | Neg, (Neg | Nonpos) -> true
    | Neg, _ -> false
    | Pos, (Pos | Nonneg) -> true
    | Pos, _ -> false
    | Nonneg, Nonneg -> true
    | Nonneg, _ -> false
    | Nonpos, Nonpos -> true
    | Nonpos, _ -> false

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
  | Zero -> Pretty.string fmt "=0"
  | Pos -> Pretty.string fmt ">0"
  | Neg -> Pretty.string fmt "<0"
  | Nonneg -> Pretty.string fmt ">=0"
  | Nonpos -> Pretty.string fmt "<=0"
  | F -> Pretty.string fmt "bot"
  | T -> Pretty.string fmt "real"


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

let of_term lookup a = 
  let rec term a =
    match a with
      | Term.App(Arith(op), xl) -> arith op xl
      | Term.App(Pp(op), xl) -> pprod op xl
      | Term.App(Bvarith(op), [x]) -> bvarith op x
      | _ -> lookup a
  and arith op al = 
    try
      match op, al with
	| Num(q), [] -> of_q q
	| Multq(q), [x] -> multq q (term x)
	| Add, [x; y] -> add (term x) (term y)
	| Add, xl -> addl (List.map term xl)
	| _ -> assert false
      with
	  Not_found -> T
  and bvarith op a =
    match op with
      | Unsigned -> Nonneg
  and pprod op al =
    try
      match op, al with
	| Expt(n), [x] -> expt n (try term x with Not_found -> T)
	| Mult, [] -> of_q Q.one
	| Mult, [x] -> term x
	| Mult, [x; y] -> mult (term x) (term y)
	| Mult, xl -> multl (List.map term xl)
	| _ -> assert false
      with
	  Not_found -> T
  in
    term a

let of_term lookup =
  Trace.func "foo6" "Sign.of_term" Term.pp pp (of_term lookup)

let inter s t =
  Trace.call "foo6" "Sign.inter" (s, t) (Pretty.pair pp pp);
  let res = inter s t in
    Trace.exit "foo6" "Sign.inter" res pp;
    res
