
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


(*s Arithmetic type. *)

let arith ctxt a =
  let rec of_term a =
    let f,l = Term.destruct a in
    match Sym.destruct f with
      | Sym.Interp(Sym.Arith(op)) -> 
	  of_linarith op l
      | Sym.Uninterp(Sym.Internal(Sym.Slack(_, c))) ->
	  (try Number.inter (ctxt a) c with Not_found -> c)
      | _ ->
	  ctxt a

  and of_linarith op l =
    match op, l with
      | Sym.Num(q), [] ->  
	  Number.mk_singleton q
      | Sym.Multq(q), [x] ->
	  Number.multq q (of_term x)
      | Sym.Add, _ -> 
	  List.fold_right 
	    (fun x -> Number.add (of_term x)) 
	    l Number.mk_zero
      | _ -> assert false

  and of_nonlin op l =
    Number.mk_real
    (*
    match op, l with
      | Sym.Expt(n), [x] -> 
	  Number.expt n (of_term x)
      | Sym.Mult, _ -> 
	  List.fold_right 
	    (fun x -> Number.mult (of_term x)) 
	    l Number.mk_one
      | _ -> assert false
     *)
  in
  try 
    Some(of_term a) 
  with 
      Not_found -> None
  

(*s Type from static information only. *)

let of_term =  
  let empty _ = raise Not_found in 
  arith empty
