
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
open Sym
open Term
(*i*)

let rec mk_unsigned = 
  let unsigned = mk_app (Bvarith(Unsigned)) in
    function
      | App(Bv(Const(b)), []) ->
	  Arith.mk_num (Q.of_int (unsigned_interp_of b))
      | App(Bv(Conc(n, m)), [x; y]) ->
	  let ux = mk_unsigned x 
	  and uy = mk_unsigned y in
	  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
	    Arith.mk_add (Arith.mk_multq two_expt_m ux) uy
      | a -> 
	  unsigned [a]

and unsigned_interp_of b =
  Bitv.fold_right 
    (fun x acc -> 
       if x then 2 * acc + 1 else 2 * acc) 
    b 0

let sigma op l =
  match op, l with
    | Unsigned, [x] -> mk_unsigned x
    | _ -> assert false

let rec map f a =
  match a with
    | App(Bvarith(Unsigned), [x]) ->
	let x' = map f x in
	  if x == x' then a else
	    mk_unsigned x'
    | _ -> 
	f a

let tau _ _ _ = Cnstrnt.mk_nat
