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

open Term
open Mpa

let rec mk_mult a b =
  try
    let op, xl = Arith.d_interp a in
      mk_mult_arith op xl b
  with
      Not_found -> 
	(try
	   let op, yl = Arith.d_interp b in
	     mk_mult_arith op yl a
	 with
	     Not_found -> Pprod.mk_mult a b)
	   
and mk_mult_arith op yl b =
  match op, yl with
    | Sym.Num(q), [] -> 
	Arith.mk_multq q b
    | Sym.Multq(q), [x] -> 
	Arith.mk_multq q (mk_mult x b)
    | Sym.Add, _ ->
	mk_mult_add b yl
    | _ ->
	assert false

and mk_mult_add a yl =
  Arith.mk_addl (mapl (mk_mult a) yl)

and mk_multl al =
  List.fold_left mk_mult (Arith.mk_one()) al

and mk_expt a n =
  assert(n >= 0);
  if n = 0 then Arith.mk_one() else 
    mk_mult a (mk_expt a (n - 1))


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
