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

open Term
open Mpa

let rec mk_mult a b =
  if Term.eq a b then mk_expt 2 a
  else if Pprod.is_one a then b
  else if Pprod.is_one b then a
  else 
    try
      let op, xl = Arith.d_interp a in
	mk_mult_arith op xl b
    with
	Not_found -> 
	  (try
	     let op, yl = Arith.d_interp b in
	       mk_mult_arith op yl a
	   with
	       Not_found -> mk_inj (Pprod.mk_mult a b))

and mk_inj pp =
  if Pprod.is_one pp then Arith.mk_one() else pp
	   
and mk_mult_arith op yl b =
  match op, yl with
    | Sym.Num(q), [] -> 
	Arith.mk_multq q b
    | Sym.Multq(q), [x] -> 
	if Pprod.is_one x then
	  Arith.mk_multq q b
	else 
	  Arith.mk_multq q (mk_mult x b)
    | Sym.Add, _ ->
	mk_mult_add b yl
    | _ ->
	assert false

and mk_mult_add a yl =
  Arith.mk_addl (mapl (mk_mult a) yl)

and mk_multl al =
  List.fold_left mk_mult (Arith.mk_one()) al

and mk_expt n a =
  if n = 0 then Arith.mk_one()
  else if n = 1 then a
  else
    try
      let op, xl = Arith.d_interp a in
	mk_expt_arith n op xl
    with
	Not_found -> mk_inj (Pprod.mk_expt n a)

and mk_expt_arith n op xl =
  match op, xl with
    | Sym.Num(q), [] ->                       (* case [q^n] *)
	if n >= 0 then
	  Arith.mk_num (Mpa.Q.expt q n)
	else if n = -1 && not(Q.is_zero q) then
	  Arith.mk_num (Mpa.Q.inv q)
	else 
	  mk_inj (Pprod.mk_expt (-1) (Arith.mk_num q))
    | Sym.Multq(q), [x] ->                    (* [(q * x)^n = q^n * x^n] *)
	if n >= 0 then
	  Arith.mk_multq (Mpa.Q.expt q n) (mk_expt n x)
	else if n = -1 && not(Q.is_zero q) then
	  Arith.mk_multq (Mpa.Q.inv q) (mk_inj (Pprod.mk_expt (-1)  x))
	else 
	  mk_inj (Pprod.mk_expt n (Pprod.mk_mult (Arith.mk_num q) x))
    | Sym.Add, _ ->                           (* case [(x1 + ... + xk)^n]  *)
	mk_expt_add n xl 
    | _ ->
	assert false

and mk_expt_add n xl =
  if n = 0 then
    Arith.mk_one()
  else if n = 1 then 
    Arith.mk_addl xl
  else if n > 1 then
    mk_mult_add (mk_expt_add (n - 1) xl) xl
  else
    mk_inj (Pprod.mk_expt n (Arith.mk_addl xl))

	           
let mk_div a b = 
  mk_mult a (mk_expt (-1) b)

let mk_inv a = 
  mk_expt (-1) a

(** Mapping a term transformer [f] over [a]. *)
let rec map f a = 
  if Arith.is_interp a then
    Arith.map (Pprod.map f) a
  else if Pprod.is_interp a then
    Pprod.map (Arith.map f) a
  else 
    f a

(** Replace [x] by [b] in [a]. *)
let apply (x, b) a = 
  map (fun y -> if Term.eq x y then b else y) a


(** Cross multiplication pair representing equalities, disequalities, etc. *)
let rec crossmultiply (a, b) =
  let lcm_of_denumerators a =  (* least common multiple of denumerators. *)
    Arith.Monomials.fold 
      Arith.Monomials.is_true
      (fun (_, pp) acc ->
	 let qq = Pprod.denumerator pp in
	   if Pprod.is_one qq then acc else 
	     let (_, _, d) = Pprod.lcm (acc, qq) in
	       d)
      a Pprod.mk_one
  in
  let da = lcm_of_denumerators a
  and db = lcm_of_denumerators b in
  let (_, _, d) = Pprod.lcm (da, db) in
    if Pprod.is_one d then (a, b) else
	(mk_mult a d, mk_mult b d)

      
(** Crossmultiply with nonnegative denumerators. *)
let crossmultiply_nonneg a =
  let is_nonneg_denum (x, n) =
    n < 0 && Term.Var.is_slack x 
  in
  let plcm = 
    Arith.Monomials.fold 
      Arith.Monomials.is_true
      (fun (_, pp) acc -> 
	 let qq = Pprod.nonneg_denumerator pp in
	   if Pprod.is_one qq then acc else 
	     let (_, _, d) = Pprod.lcm (acc, qq) in
	       d)
      a Pprod.mk_one
  in
    if Pprod.is_one plcm then a else mk_mult a plcm

