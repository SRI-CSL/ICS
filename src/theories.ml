
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
 * Author: Harald Ruess
i*)

(*i*)
open Mpa
(*i*)

(*s Classification of function symbols. *)


type t = U | T | BV | A | S


let to_string = function
  | U -> "u"
  | A -> "a"
  | BV -> "bv"
  | T -> "t"
  | S -> "s"

let pp fmt i =
  Format.fprintf fmt "%s" (to_string i)

let of_string = function
  | "u" -> U
  | "a" -> A
  | "t" -> T
  | "bv" -> BV
  | "s" -> S
  | x -> raise (Invalid_argument x)

let iter f =
  f A; f T; f BV; f U; f S

let for_all p =
  p A && p T && p BV && p U && p S


module Array = struct

  type 'a arr = {
    mutable u : 'a;
    mutable t : 'a;
    mutable bv : 'a;
    mutable a : 'a;
    mutable s : 'a
  }

  let create x = {u = x; a = x; t = x; bv = x; s = x}

  let copy arr = {u = arr.u; a = arr.a; t = arr.t; bv = arr.bv; s = arr.s}
     
  let get arr = function
    | U -> arr.u
    | A -> arr.a
    | T -> arr.t
    | BV -> arr.bv
    | S -> arr.s

  let set arr i x =
    match i with
      | U -> arr.u <- x
      | A -> arr.a <- x
      | T -> arr.t <- x
      | BV -> arr.bv <- x
      | S -> arr.s <- x

  let reset arr x =
    arr.u <- x; arr.a <- x; arr.t <- x; arr.bv <- x; arr.s <- x

  let iter f arr =
    f U arr.u; f A arr.a; f T arr.t; f BV arr.bv; f S arr.s

  let for_all p arr =
    p arr.u && p arr.a && p arr.t && p arr.bv && p arr.s

  let for_all2 p arr1 arr2 =
    p arr1.u arr2.u && p arr1.a arr2.a && 
    p arr1.t arr2.t && p arr1.bv arr2.bv && p arr1.s arr2.s
	  
  let to_list arr =
    [(U, arr.u); (A, arr.a); (T, arr.t); (BV, arr.bv); (S, arr.bv)]

  let pp p fmt arr =
    Pretty.map pp p fmt (to_list arr)

end
