
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

type t = Int | Real

let eq d1 d2 = (d1 = d2)

(*s Union of two domains*)

let union d1 d2 =
  match d1, d2 with
    | Int, Int -> Int
    | _ -> Real

(*s Intersection of two domains *)

let inter d1 d2 =
  match d1, d2 with
    | Real, Real -> Real
    | _ -> Int

(*s Testing for disjointness. *)

let disjoint d1 d2 = false

(*s Testing for subdomains. *)

let sub d1 d2 =
  match d1, d2 with
    | Real, Int -> false
    | _ -> true

let cmp d1 d2 = 
  match d1, d2 with
    | Int, Int -> Binrel.Same
    | Int, Real -> Binrel.Sub
    | Real, Int -> Binrel.Super
    | Real, Real -> Binrel.Same

let of_q q =
  if Mpa.Q.is_integer q then Int else Real

let mem q = function
  | Real -> true
  | Int -> Mpa.Q.is_integer q

let pp fmt d =
  let s = match d with
    | Real -> "real"
    | Int -> "int"
  in
  Format.fprintf fmt "%s" s
