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

open Sym
open Term
open Mpa

let rec mk_mult a b =
  if Term.eq a b then        (* [ a * a --> a^2] *)
    mk_expt 2 a
  else if Pp.is_one a then
    b
  else if Pp.is_one b then
    a
  else 
    match a, b with
      | App(Arith(op), xl), _ ->
	  mk_mult_arith op xl b
      | _, App(Arith(op), yl) ->
	  mk_mult_arith op yl a
      | _ ->
	  mk_inj (Pp.mk_mult a b)

and mk_inj pp =
  if Pp.is_one pp then
    Arith.mk_one
  else 
    pp
	   

and mk_mult_arith op yl b =
  match op, yl with
    | Num(q), [] -> 
	Arith.mk_multq q b
    | Multq(q), [x] -> 
	if Pp.is_one x then
	  Arith.mk_multq q b
	else 
	  Arith.mk_multq q (mk_mult x b)
    | Add, _ ->
	mk_mult_add b yl
    | _ ->
	assert false

and mk_mult_add a yl =
  Arith.mk_addl (mapl (mk_mult a) yl)


and mk_multl al =
  List.fold_left mk_mult Arith.mk_one al


and mk_expt n a =
  if n = 0 then 
    Arith.mk_one
  else if n = 1 then
    a
  else match a with
    | App(Arith(op), xl) ->
	mk_expt_arith n op xl
    | _ ->
	mk_inj (Pp.mk_expt n a)

and mk_expt_arith n op xl =
  match op, xl with
    | Num(q), [] ->                       (* case [q^n] *)
	if n >= 0 then
	  Arith.mk_num (Mpa.Q.expt q n)
	else if n = -1 && not(Q.is_zero q) then
	  Arith.mk_num (Mpa.Q.inv q)
	else 
	  mk_inj (Pp.mk_expt (-1) (Arith.mk_num q))
    | Multq(q), [x] ->                    (* [(q * x)^n = q^n * x^n] *)
	if n >= 0 then
	  Arith.mk_multq (Mpa.Q.expt q n) (mk_expt n x)
	else if n = -1 && not(Q.is_zero q) then
	  Arith.mk_multq (Mpa.Q.inv q) (mk_inj (Pp.mk_expt (-1)  x))
	else 
	  mk_inj (Pp.mk_expt n (Pp.mk_mult (Arith.mk_num q) x))
    | Add, _ ->                           (* case [(x1 + ... + xk)^n]  *)
	mk_expt_add n xl 
    | _ ->
	assert false

and mk_expt_add n xl =
  if n = 0 then
    Arith.mk_one
  else if n = 1 then 
    Arith.mk_addl xl
  else if n > 1 then
    mk_mult_add (mk_expt_add (n - 1) xl) xl
  else
    mk_inj (Pp.mk_expt n (Arith.mk_addl xl))

	           
let mk_div a b = 
  mk_mult a (mk_expt (-1) b)

let mk_inv a = 
  mk_expt (-1) a

(** Mapping a term transformer [f] over [a]. *)
let rec map f a = 
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num _, [] -> 
	       a
	   | Multq(q), [x] ->
	       let x' = map f x in
		 if x == x' then a else 
		   Arith.mk_multq q x'
	   | Add, [x; y] -> 
	       let x' = map f x and y' = map f y in
		 if x == x' && y == y' then a else 
		   Arith.mk_add x' y'
	   | Add, xl -> 
	       let xl' = Term.mapl (map f) xl in
		 if xl == xl' then a else
		   Arith.mk_addl xl'
	   | _ -> 
	       assert false)
    | App(Pp(op), l) ->
	(match op, l with
	   | Mult, [x; y] -> 
	       let x' = map f x and y' = map f y in
		 if x == x' && y == y' then a else 
		   mk_mult x' y'
	   | Mult, xl -> 
	       let xl' = Term.mapl (map f) xl in
		 if xl == xl' then a else
		   mk_multl xl'
	   | Expt(n), [x] -> 
	       let x' = map f x in
		 if x == x' then a else
		   mk_expt n x'
	   | _ ->
	       assert false)
    | _ ->
	f a










