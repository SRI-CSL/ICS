
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
 * Authors: Ritvik Sahajpa, Harald Ruess
 i*)

type t = Int | Nonint | Real

let eq d1 d2 = (d1 = d2)

(*s Union of two domains*)

let union d1 d2 =
  if d1 = d2 then d1 else Real         (*s e.g [union Int Int = Int] *)

(*s Intersection of two domains*)

exception Empty

let inter d1 d2 =
  if d1 = d2 then d1 else
    match d1, d2 with
      | Int, Real -> Int
      | Real, Int -> Int
      | Nonint, Real -> Nonint
      | Real, Nonint -> Nonint
      | Int, Nonint -> raise Empty
      | Nonint, Int -> raise Empty
      | _ -> assert false

(*s Complement domain. *)

let compl = function
  | Real -> Real
  | Int -> Nonint
  | Nonint -> Int
   

(*s Testing for disjointness. *)

let disjoint d1 d2 =
  match d1, d2 with
    | Int, Nonint -> true
    | Nonint, Int -> true
    | _ -> false

(*s Testing for subdomains. *)

let sub d1 d2 =
  d1 = d2 ||
  (match d1, d2 with
     | Int, Real -> true
     | Nonint, Real -> true
     | _ -> false)

let cmp d1 d2 = 
  if d1 = d2 then Binrel.Same else
    match d1, d2 with
      | Real, _ -> Binrel.Super
      | _ , Real -> Binrel.Sub
      | Int, Nonint -> Binrel.Disjoint
      | Nonint, Int -> Binrel.Disjoint
      | _ -> assert false

let of_q q =
  if Mpa.Q.is_integer q then Int else Nonint

let pp fmt d =
  let s = match d with
    | Real -> "real"
    | Int -> "int"
    | Nonint -> "nonint"
  in
  Format.fprintf fmt "%s" s
