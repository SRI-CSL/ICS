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

open Mpa
open Term
open Arith

(** {6 Inequalities} *)

type t = 
  | True
  | False
  | Less of Term.t * bool * Term.t
  | Greater of Term.t * bool * Term.t

let mk_less (a, alpha, b) =
  match Arith.linearize (Arith.mk_sub a b) with
      | Const(p) ->                   (* p < 0 *)
	  let res = Q.compare p Q.zero in
	    if res < 0 then
	      True
	    else if res > 0 then
	      False
	    else (* [p = r] *)
	      if alpha then True else False
      | Linear(p, q, x, a') ->    (* [p + q*x + a' < 0] *) 
	  assert(not(Q.is_zero q));
	  let b' = mk_addq (Q.minus (Q.div p q))
		     (mk_multq (Q.minus (Q.inv q)) a')
	  in
	    if Q.is_pos q then          (* <=> [x < -p/q - 1/q * a' *)
	      Less(x, alpha, b')
	    else (* [ q < 0] *)  
	      Greater(x, alpha, b')

let mk_greater (a, alpha, b) =              (* [a >(=) b] *)
    match linearize (mk_sub a b) with
      | Const(p) ->                         (* [p >(=) 0] *)
	  let res = Q.compare p Q.zero in
	    if res < 0 then
	      False
	    else if res > 0 then
	      True
	    else (* [p = r] *)
	      if alpha then True else False
      | Linear(p, q, x, a') ->      (* [p + q*x + a' >(=) r] *) 
	  assert(not(Q.is_zero q));
	  let b' =  mk_addq (Q.minus (Q.div p q))
		     (mk_multq (Q.minus (Q.inv q)) a')
	  in
	    if Q.is_pos q then
	      Greater(x, alpha, b')
	    else 
	      Less(x, alpha, b')

let mk_lt a b = mk_less (a, false, b)
let mk_le a b = mk_less (a, true, b)
let mk_gt a b = mk_less (b, false, a)
let mk_ge a b = mk_less (b, true, a)

let mk_negate = function
  | True -> False
  | False -> True
  | Less(x, kind, a) -> Greater(x, not kind, a)
  | Greater(x, kind, a) -> Less(x, not kind, a)

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | False -> Pretty.string fmt "False"
  | Less(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " <= " else " < ");
      Term.pp fmt b
  | Greater(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " >= " else " > ");
      Term.pp fmt b

let solve x = x


(** {6 Term comparison} *)

let is_le a b =
  let (q, ml) = poly_of a 
  and (p, nl) = poly_of b in
    Term.eql ml nl && Q.le q p

let is_lt a b =
  let (q, ml) = poly_of a 
  and (p, nl) = poly_of b in
    Term.eql ml nl && Q.lt q p

let is_less (a, alpha, b) =
  if alpha then le a b else lt a b

let is_greater (a, alpha, b) =
  less (b, alpha, a)

