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

type t = Int | Real | Nonint

let eq d1 d2 = (d1 = d2)

(** {6 Connectives} *)

exception Empty

(** Intersection of two domains *)
let inter d1 d2 =
  match d1, d2 with
    | Real, _ -> d2
    | _, Real -> d1
    | Int, Int -> Int
    | Nonint, Nonint -> Nonint
    | Int, Nonint -> raise Empty
    | Nonint, Int -> raise Empty


(** Union of two domains*)
let union d1 d2 =
  match d1, d2 with
    | Int, Int -> Int
    | Nonint, Nonint -> Nonint
    | Real, _ -> Real
    | _, Real -> Real
    | Int, Nonint -> Real
    | Nonint, Int -> Real
  

(** Testing for disjointness. *)
let disjoint d1 d2 =
  match d1, d2 with
    | Int, Nonint -> true
    | Nonint, Int -> true
    | _ -> false

let cmp d e =
  match d, e with   
    | Real, Real -> 0
    | Real, _ -> 1
    | _, Real -> -1
    | Int, Int -> 0
    | Nonint, Nonint -> 0
    | Int, Nonint -> 1  (* arbitrary nonzero *)
    | Nonint, Int -> -1

let cmp d e =
  match d with
    | Real ->
	(match e with
	   | Real -> 0
	   | Int -> 1
	   | Nonint -> 1)
    | Int ->
	(match e with
	   | Real -> -1
	   | Int -> 0
	   | Nonint -> 1)
    | Nonint ->
	(match e with
	   | Real -> -1
	   | Int -> -1
	   | Nonint -> 0)


(** Testing for subdomains. *)
let rec sub d1 d2 =
  match d1, d2 with
    | _, Real -> true
    | Int, Int -> true
    | Nonint, Nonint -> true
    | _ -> false

let of_q q =
  if Mpa.Q.is_integer q then Int else Nonint

let add d1 d2 =
  match d1, d2 with
    | Int, Int -> Int
    | _ -> Real

let addl dl =
  let is_int = function Int -> true | _ -> false in
    if dl = [] then Int
    else if List.for_all is_int dl then Int 
    else Real

let expt n d = d

let mult d1 d2 =
  match d1, d2 with
    | Int, Int -> Int
    | _ -> Real

let multq q = mult (of_q q)

let multl dl = 
 let is_int = function Int -> true | _ -> false in
   if dl = [] then Int
   else if List.for_all is_int dl then Int
   else Real

let mem q = function
  | Real -> true
  | Int -> Mpa.Q.is_integer q
  | Nonint -> not(Mpa.Q.is_integer q)


let to_string = function
  | Real -> "real"
  | Int -> "int"
  | Nonint -> "nonint"
	
let pp fmt d =
  Format.fprintf fmt "%s" (to_string d)


let to_string = Pretty.to_string pp

let inj =
  let int = Some(Int)
  and real = Some(Real)
  and nonint = Some(Nonint) in
    function
      | Int -> int
      | Real -> real
      | Nonint -> nonint
