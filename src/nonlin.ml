
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
open Tools
open Hashcons
open Mpa
open Term
open Sym
(*i*)


(*s Symbols. *)

(*s Recognizers. *)

let is_mult a = Sym.eq Sym.mk_mult (Term.sym_of a)

let is_expt a = Sym.eq Sym.mk_expt (Term.sym_of a)


(*s Destructors. *)

let d_mult a =
  if Sym.eq Sym.mk_mult (Term.sym_of a) then
    Some(Term.args_of a)
  else 
    None

let d_expt a =
  let f,l = Term.destruct a in
  if Sym.eq Sym.mk_expt (Term.sym_of a) then
    (match Term.args_of a with 
       | [x;y] -> Some(x,y)
       | _ -> None)
  else 
    None


(*s Normalize. *)


let expt_of a =
  match d_expt a with
    | Some(n,x) -> (n,x)
    | None -> (Arith.mk_one, a)


(*s Constructors. *)

let rec mk_mult (a,b) =
  if Term.eq a b then 
    mk_expt (Arith.mk_num (Q.of_int 2)) a else
  match d_expt a, d_expt b with
    | Some(n,x), Some(m,y) when Term.eq x y ->
	mk_expt (Arith.mk_add n m) x
    | Some(n,x), _ when Term.eq x b ->
	mk_expt (Arith.mk_incr n) x
    | _, Some(m,y) when Term.eq y a ->
	mk_expt (Arith.mk_incr m) y
    | _ ->
	(match Arith.d_interp a with
	   | Some(f,xl) ->
	       mk_linmult f xl b
	   | None ->
	       (match Arith.d_interp b with
		  | Some(g,yl) ->
		      mk_linmult g yl a
		  | None ->
		      (match d_mult a, d_mult b with
			 | Some(xl), Some(yl) ->
			     of_list (multl xl yl)
			 | Some(xl), None ->
			     of_list (multl xl [b])
			 | None, Some(yl) ->
			     of_list (multl yl [a])
			 | _ ->
			     let (a,b) = Term.order a b in
			     Term.mk_app Sym.mk_mult [a;b])))

and mk_multl al = 
  match al with
    | [] -> Arith.mk_num Q.one
    | [x] -> x
    | x :: xl -> mk_mult (x, mk_multl xl)

and mk_linmult f al b =
  match f, al with
    | Sym.Num(q),[] ->
	Arith.mk_multq q b
    | Sym.Multq(q), [x] ->
	Arith.mk_multq q (mk_mult (x,b))
    | Sym.Add, xl ->
	let yl = List.map (fun x -> mk_mult (b,x)) xl in
	Arith.mk_addl yl
    | _ ->
	failwith "Ill-formed expression"

and multl l1 l2 =             
  match l1, l2 with
    | [], _ -> l2
    | _ , [] -> l1
    | m1 :: l1', m2 :: l2' ->
	let n1,x1 = expt_of m1 in
	let n2,x2 = expt_of m2 in
	let cmp = Term.cmp x1 x2 in
	if cmp = 0 then
	  let n = Arith.mk_add n1 n2 in
	  if Arith.is_zero n then
	    multl l1' l2'
	  else 
	    (mk_expt n x1) :: (multl l1' l2')
	else if cmp < 0 then
	  m1 :: multl l1' l2
	else (* cmp > 0 *)
	  m2 :: multl l1 l2'

and of_list l =
  match l with
    | [] -> Arith.mk_one
    | [x] -> x
    | _ -> Term.mk_app Sym.mk_mult l

and mk_expt n a =
  match Arith.d_num n with
    | Some(q) -> mk_linexpt q a
    | _ -> Term.mk_app Sym.mk_expt [n; a]

and mk_linexpt n a =
   if Q.is_zero n then
     Arith.mk_one
   else if Q.is_one n then
     a
   else 
     (match d_expt a with
	| Some(m, x) when Term.eq x a ->
	    mk_expt (Arith.mk_add (Arith.mk_num n) m) a
	| _ ->
	    (match d_mult a with
	       | Some(xl) ->
		   of_list (List.map (mk_expt (Arith.mk_num n)) xl)
	       | None ->
		   Term.mk_app Sym.mk_expt [Arith.mk_num n; a]))


(*

val mk_sin : t
val mk_cos : t
val mk_unsigned : t
val mk_floor : t
val mk_ceiling : t


let mk_sin = 
  let x = Name.of_string "sin" in
  make(Uninterp(x))

let mk_cos =
  let x = Name.of_string "cos" in
  make(Uninterp(x))

let mk_unsigned =
  make(Uninterp(Name.of_string "unsigned"))

let mk_floor = 
  make(Uninterp(Name.of_string "floor"))

let mk_ceiling = 
  let name = Name.of_string "ceiling" in
  make(Uninterp(name))


*)


