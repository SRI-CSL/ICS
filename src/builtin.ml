
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
open Hashcons
open Term
open Mpa
(*i*)

(*s Unsigned interpretation. *)


let sym_unsigned = Sym.mk_uninterp (Name.of_string "unsigned")


let rec mk_unsigned a =
  let f,l = Term.destruct a in
  match Sym.destruct f, l with
    | Sym.Interp(Sym.Bv(op)), l ->
	(match op, l with
	   | Sym.Const(b), [] -> mk_unsigned_const b
	   | Sym.Sub(n,i,j), [x] -> mk_unsigned_sub n i j x
	   | Sym.Conc(n,m), [x;y] -> mk_unsigned_conc n m x y
	   | _ -> failwith "Bitvector.mk_unsigned: ill-formed expression")
    | _ ->
	Term.mk_app sym_unsigned [a]

and mk_unsigned_const b =
  let n = Bitv.fold_right 
	    (fun x acc -> if x then 2 * acc + 1 else 2 * acc) 
	    b 0
  in
  Arith.mk_num (Q.of_int n)

and mk_unsigned_sub n i j x =
  Term.mk_app sym_unsigned [Bitvector.mk_sub n i j x]

and mk_unsigned_conc n m x y =
  let ux = mk_unsigned x in
  let uy = mk_unsigned y in
  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
  Arith.mk_add (Arith.mk_multq two_expt_m ux) uy
  
(*s Update/Select *)

let sym_update = Sym.mk_uninterp (Name.of_string "update")
let sym_select = Sym.mk_uninterp (Name.of_string "select")

let mk_update a i e =
  Term.mk_app sym_update [a;i;e]

let mk_select a x =
  match Term.destruct a with
    | f, [b;y;e] when Sym.eq f sym_update && Term.eq x y -> e
    | _ -> Term.mk_app sym_select [a;x]
	

(* Sine and Cosine *)

let rec mk_sin a =
  match Arith.d_add a with
    | Some([x;y]) ->
	Arith.mk_add (Arith.mk_mult (mk_sin x) (mk_cos y))
	             (Arith.mk_mult (mk_cos x) (mk_sin y))
    | _ ->
	Term.mk_app Sym.mk_sin [a]

and mk_cos a = 
  Term.mk_app Sym.mk_cos [a]

