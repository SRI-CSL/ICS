
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
open Sign
(*i*)

type extq =
  | Inject of Q.t
  | Posinf
  | Neginf

and t = extq

let destruct x = x


let inject q = Inject(q)

let posinf = Posinf

let neginf = Neginf


(*s Miscellaneous. *)

let zero = inject Q.zero

let is_zero x =
  match x with
    | Inject q -> Q.is_zero q
    | _ -> false

let is_q x =
  match x with
    | Inject _ -> true
    | _ -> false

let to_q x =
  match x with
    | Inject q -> Some q
    | _ -> None

let of_q = inject

let pp fmt x =
  match x with
    | Inject q -> Q.pp fmt q
    | Posinf -> Format.fprintf fmt "inf"
    | Neginf -> Format.fprintf fmt "-inf"


(*s Test if argument is an integer. *)

let is_int x =
  match x with
    | Inject q -> Q.is_integer q
    | _ -> false

(*s Equality is just pointer comparison for hash-consing. *)

let eq x y =
  match x, y with
    | Inject(q), Inject(p) -> Q.equal q p
    | Posinf, Posinf -> true
    | Neginf, Neginf -> true
    | _ -> false


(*s Ordering relation. *)

let lt x y =
  match x with
    | Inject q ->
	(match y with
	   | Inject p -> Q.lt q p  
           | Posinf -> true
           | Neginf -> false)
    | Neginf -> true
    | Posinf -> false

let le x y =
  eq x y || lt x y

(*s Minimum and maximum. *)

let min x y = if lt x y then x else y

let max x y = if lt x y then y else x

(*s Comparisons. *)

let cmp x y =
  match x, y with
    | Inject u, Inject v -> Q.cmp u v
    | Inject _, Posinf -> Q.Less
    | Inject _, Neginf -> Q.Greater
    | Neginf, Neginf -> Q.Equal
    | Neginf, _ -> Q.Less
    | Posinf, Posinf -> Q.Equal
    | Posinf, _ -> Q.Greater

(*s Sign computation. *)

let sign x =
  match x with
    | Inject q -> Q.sign q
    | Posinf -> Sign.Pos
    | Neginf -> Sign.Neg



(*s Arithmetic operations *)

exception Undefined

let add x y = 
  match x with
    | Inject p -> 
	(match y with
	   | Inject q -> inject (Q.add p q)
	   | Posinf -> posinf
	   | Neginf -> neginf)
    | Neginf ->
	(match y with
	   | Posinf -> raise Undefined
	   | _ -> neginf)
    | Posinf ->
	(match y with
	   | Neginf -> raise Undefined
	   | _ -> posinf)

let sub x y =
  match x with
    | Inject p ->
	(match y with
	   | Inject q -> inject (Q.sub p q)
	   | Posinf -> neginf
	   | Neginf -> posinf)
    | Neginf -> 
	(match y with
	   | Neginf -> raise Undefined
	   | _ -> neginf)
    | Posinf ->
	(match y with
	   | Posinf -> raise Undefined
	   | _ -> posinf)

let mult x y =
  match x with
    | Inject p ->
	(match y with
	   | Inject q -> inject (Q.mult p q)
	   | Neginf -> 
	       (match Q.sign p with
		  | Zero -> raise Undefined
		  | Neg -> posinf
		  | Pos -> neginf)
	   | Posinf ->
	       (match Q.sign p with
		  | Zero -> raise Undefined
		  | Neg -> neginf
		  | Pos -> posinf))
    | Posinf ->
	(match y with
	   | Inject q ->
	       (match Q.sign q with
		  | Neg -> neginf
		  | Zero -> raise Undefined
		  | Pos -> posinf)
	   | Posinf -> posinf
	   | Neginf -> neginf)
    | Neginf ->
	(match y with
           | Neginf -> 
	       posinf
	   | Inject q ->
	       (match Q.sign q with
		  | Neg -> posinf
		  | Zero -> raise Undefined
		  | Pos -> neginf)
	   | Posinf -> 
	       neginf)
	       
let div x y =
  match y with
    | Inject q ->
	(match Q.sign q with
	   | Zero -> 
	       raise Undefined
	   | Neg -> 
	       (match x with
		  | Inject p -> inject (Q.div p q)
		  | Neginf -> neginf 
		  | Posinf -> posinf)
	   | Pos ->
	       (match x with
		  | Inject p -> inject (Q.div p q)
		  | Neginf -> posinf
		  | Posinf -> neginf))
    | Posinf -> 
	(match x with
	   | Neginf | Posinf -> raise Undefined
	   | _ -> zero)
    | Neginf ->
	(match x with
	   | Neginf | Posinf -> raise Undefined
	   | _ -> zero)
