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
open Sym
open Term
open Arith

let is_less (a, alpha) =
  match a with
    | App(Arith(Num(q)), []) -> 
	if alpha then Q.le q Q.zero else Q.lt q Q.zero
    | _ -> 
	false

let is_greater (a, alpha) =
  match a with
    | App(Arith(Num(q)), []) -> 
	if alpha then Q.ge q Q.zero else Q.gt q Q.zero
    | _ -> 
	false

type t = 
  | True
  | Less of Term.t * bool * Term.t
  | Greater of Term.t * bool * Term.t

let pp fmt = function
  | True -> Pretty.string fmt "True"
  | Less(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " <= " else " < ");
      Term.pp fmt b
  | Greater(a, kind, b) ->
      Term.pp fmt a;
      Pretty.string fmt (if kind then " >= " else " > ");
      Term.pp fmt b

let solve l =
  let (a, alpha, _) = Fact.d_less l in
    match Arith.linearize a with
      | Const(p) ->                   (* p < 0 *)
	  let res = Q.compare p Q.zero in
	    if res < 0 then
	      True
	    else if res > 0 then
	      raise Exc.Inconsistent
	    else (* [p = r] *)
	      if alpha then True else raise Exc.Inconsistent
      | Linear(p, q, x, a') ->    (* [p + q*x + a' < 0] *) 
	  assert(not(Q.is_zero q));
	  let b' = mk_addq (Q.minus (Q.div p q))
		     (mk_multq (Q.minus (Q.inv q)) a')
	  in
	    if Q.is_pos q then          (* <=> [x < -p/q - 1/q * a' *)
	      Less(x, alpha, b')
	    else (* [ q < 0] *)  
	      Greater(x, alpha, b')

let solve =
  Trace.func "foo2" "Ineq.solve" Fact.pp_less pp solve





