
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


(*s Division. *)

let mk_div s a b =
  let a = Context.find Theories.a s a in
  let b = Context.find Theories.a s b in
  match Arith.d_num b with
    | Some(q) ->
	Arith.mk_multq (Mpa.Q.inv q) a
    | None ->
	Term.mk_app Sym.mk_div [a;b]

(*s Unsigned interpretation. *)

let rec mk_unsigned s a =
  let a = Context.find Theories.bv s a in  (* look for bitvector interpretation. *)
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
		 mk_unsigned_conc s n m x y
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

and mk_unsigned_conc s n m x y =
  let ux = mk_unsigned s x in
  let uy = mk_unsigned s y in
  let two_expt_m = Q.of_z (Z.expt (Z.of_int 2) m) in
  Arith.mk_add (Arith.mk_multq two_expt_m ux) uy
  

(*s Update/Select *)

let mk_update s a i e =
  Term.mk_app Sym.mk_update [a;i;e]

let rec mk_select s a x =
  if is_var a then
    Term.mk_app Sym.mk_select [a;x]
  else 
    match Term.destruct a with
      | f, [b;y;e] 
	  when Sym.eq f Sym.mk_update ->
	    if  Context.is_equal s x y then 
	      e
	    else if Context.is_diseq s x y then
	      mk_select s b x
	    else 
	      Term.mk_app Sym.mk_select [a;x]
      | _ -> 
	  Term.mk_app Sym.mk_select [a;x]

(* Sine and Cosine *)

let rec mk_sin s a =
  let a = Context.find Theories.a s a in
  match Arith.d_add a with
    | Some([x;y]) ->
	Arith.mk_add (Arith.mk_mult (mk_sin s x) (mk_cos s y))
	             (Arith.mk_mult (mk_cos s x) (mk_sin s y))
    | _ ->
	Term.mk_app Sym.mk_sin [a]

and mk_cos s a =   
  let a = Context.find Theories.a s a in
  Term.mk_app Sym.mk_cos [a]


(*s Floor function. *)

let mk_floor s a = 
  let a = Context.find Theories.a s a in
  let ms = Arith.monomials a in
  let (ints,nonints) = List.partition (Context.is_int s) ms in
  let nonint' = Arith.mk_addl nonints in
  let fl = match Arith.d_num nonint' with
    | Some(q) -> Arith.mk_num (Mpa.Q.of_z (Mpa.Q.floor q))
    | None -> Term.mk_app Sym.mk_floor [nonint'] 
  in
  Arith.mk_addl (fl :: ints)

let mk_ceiling s a = 
  let a = Context.find Theories.a s a in
  Term.mk_app Sym.mk_ceiling [a]
 

(*s Sigmatizing builtin functions. *)

let sigma s f l =
  assert(Sym.is_builtin f);
  match Sym.builtin f, l with
    | Sym.Unsigned, [x] -> mk_unsigned s x
    | Sym.Select, [x;y] -> mk_select s x y
    | Sym.Update, [x;y;z] -> mk_update s x y z
    | Sym.Sin, [x] -> mk_sin s x
    | Sym.Cos, [x] -> mk_cos s x
    | Sym.Floor, [x] -> mk_floor s x
    | Sym.Ceiling, [x] -> mk_ceiling s x
    | Sym.Div, [x;y] -> mk_div s x y
    | _ -> assert false



