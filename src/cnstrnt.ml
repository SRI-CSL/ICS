(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

type t = 
  | Int
  | Real
  | Nonint
  | Bitvector of int

let pp fmt = function
  | Real -> Pretty.string fmt "real"
  | Int -> Pretty.string fmt "int"
  | Nonint -> Pretty.string fmt "nonint"
  | Bitvector(n) -> Format.fprintf fmt "bitvector[%d]" n

exception Empty

let inter c d =
  match c, d with
    | Bitvector(n), Bitvector(m) -> if n = m then c else raise Empty
    | Bitvector _, _ -> raise Empty
    | _, Bitvector _ -> raise Empty
    | Int, Int -> Int
    | _, Real -> c
    | Real, _ -> d
    | Nonint, Nonint -> Nonint
    | Int, Nonint -> raise Empty
    | Nonint, Int -> raise Empty

let disjoint c d =
  try
    let _ = inter c d in
      false
  with
      Empty -> true

let sub c d =
  (c = d) ||
  (match c, d with
     | (Int | Nonint), Real -> true
     | Real, (Int | Nonint) -> true
     | _ -> false)
   

let is_real = function
  | Real | Int | Nonint -> true
  | _ -> false

let is_int = function
  | Int -> true
  | _ -> false

