
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

let is_builtin a =
  not (is_var a) &&
  Sym.is_builtin (sym_of a)

type tests = {
  is_equal : Term.t -> Term.t -> bool;
  is_diseq : Term.t -> Term.t -> bool;
  cnstrnt : Term.t -> Cnstrnt.t option;
  find : Theories.t -> Term.t -> Term.t option
}

(*s Division. *)

let mk_div tests a b =
  match Arith.d_num b with
    | Some(q) ->
	Arith.mk_multq (Mpa.Q.inv q) a
    | None ->
	Term.mk_app Sym.mk_div [a;b]

(*s Unsigned interpretation. *)

let rec mk_unsigned tests a =
  if is_var a then
    Term.mk_app Sym.mk_unsigned [a]
  else 
    let f,l = Term.destruct a in
    match Sym.destruct f, l with
      | Sym.Interp(Sym.Bv(op)), l ->
	  (match op, l with
	     | Sym.Const(b), [] -> 
		 mk_unsigned_const b
	     | Sym.Sub(n,i,j), [x] -> 
		 mk_unsigned_sub n i j x
	     | Sym.Conc(n,m), [x;y] -> 
		 mk_unsigned_conc tests n m x y
             | Sym.Bitwise(n), [x;y;z] -> 
		 Term.mk_app Sym.mk_unsigned [a]
	     | _ -> assert false)
      | _ ->
	  Term.mk_app Sym.mk_unsigned [a]

and mk_unsigned_const b =
  let n = Bitv.fold_right 
	    (fun x acc -> if x then 2 * acc + 1 else 2 * acc) 
	    b 0
  in
  Arith.mk_num (Q.of_int n)

and mk_unsigned_sub n i j x =
  Term.mk_app Sym.mk_unsigned [Bitvector.mk_sub n i j x]

and mk_unsigned_conc tests n m x y =
  let ux = mk_unsigned tests x in
  let uy = mk_unsigned tests y in
  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
  Arith.mk_add (Arith.mk_multq two_expt_m ux) uy
  

(*s Update/Select *)

let mk_update tests a i e =
  Term.mk_app Sym.mk_update [a;i;e]

let rec mk_select tests a x =
  if is_var a then
    Term.mk_app Sym.mk_select [a;x]
  else 
    match Term.destruct a with
      | f, [b;y;e] 
	  when Sym.eq f Sym.mk_update ->
	    if  tests.is_equal x y then 
	      e
	    else if tests.is_diseq x y then
	      mk_select tests b x
	    else 
	      Term.mk_app Sym.mk_select [a;x]
      | _ -> 
	  Term.mk_app Sym.mk_select [a;x]

(* Sine and Cosine *)

let rec mk_sin tests a =
  match Arith.d_add a with
    | Some([x;y]) ->
	Arith.mk_add (Arith.mk_mult (mk_sin tests x) (mk_cos tests y))
	             (Arith.mk_mult (mk_cos tests x) (mk_sin tests y))
    | _ ->
	Term.mk_app Sym.mk_sin [a]

and mk_cos tests a = 
  Term.mk_app Sym.mk_cos [a]


(*s Floor function. *)

let is_int tests x = 
  match tests.cnstrnt x with
    | Some(c) -> Cnstrnt.dom_of c = Dom.Int
    | None -> false

let mk_floor tests a =
  let ms = Arith.monomials a in
  let (ints,nonints) = List.partition (is_int tests) ms in
  let nonint' = Arith.mk_addl nonints in
  let fl = match Arith.d_num nonint' with
    | Some(q) -> Arith.mk_num (Mpa.Q.of_z (Mpa.Q.floor q))
    | None -> Term.mk_app Sym.mk_floor [nonint'] 
  in
  Arith.mk_addl (fl :: ints)

let mk_ceiling tests a =
  Term.mk_app Sym.mk_ceiling [a]
 

(*s Sigmatizing builtin functions. *)

let sigma tests f l =
  assert(Sym.is_builtin f);
  match Sym.builtin f, l with
    | Sym.Unsigned, [x] -> mk_unsigned tests x
    | Sym.Select, [x;y] -> mk_select tests x y
    | Sym.Update, [x;y;z] -> mk_update tests x y z
    | Sym.Sin, [x] -> mk_sin tests x
    | Sym.Cos, [x] -> mk_cos tests x
    | Sym.Floor, [x] -> mk_floor tests x
    | Sym.Ceiling, [x] -> mk_ceiling tests x
    | Sym.Div, [x;y] -> mk_div tests x y
    | _ -> assert false



