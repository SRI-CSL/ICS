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
 * 
 * Author: Harald Ruess
 *)

open Sym
open Term
open Context
open Mpa
open Three


(** Processing an atom *)

type 'a status = 
  | Valid 
  | Inconsistent
  | Ok of 'a

let pp pp fmt = function
  | Valid -> Format.fprintf fmt ":valid"
  | Inconsistent -> Format.fprintf fmt ":unsat"
  | Ok(x) -> Format.fprintf fmt ":ok "; pp fmt x


let rec atom s =
  Trace.func "shostak" "Process" Atom.pp (pp Context.pp)
    (fun a ->
       try
	 match Can.atom s a with
	   | Atom.True -> 
	       Valid
	   | Atom.False ->
	       Inconsistent
	   | Atom.Equal(e) ->
	       Ok(merge a e s)
	   | Atom.Diseq(d) -> 
	       Ok(diseq a d s)
	   | Atom.In(c) -> 
	       Ok(add a c s)
	 with 
	     Exc.Inconsistent -> Inconsistent)
  
and merge a e =  
  Context.protect
    (fun s  ->
       let s = Context.extend a s in
       let (s', e') = Rule.abstract_equal (s, e) in
       let s'' = Rule.merge e' s' in
	 Rule.close s'')
    
and add a c = 
  Context.protect
    (fun s  ->
       let s = Context.extend a s in
       let (s', c') = Rule.abstract_cnstrnt (s, c) in
       let s'' = Rule.add c' s' in
	 Rule.close s'')

and diseq a d =
  Context.protect
    (fun s  ->  
       let s = Context.extend a s in
       let (s', d') = Rule.abstract_diseq (s, d) in
       let s'' = Rule.diseq d' s' in
	 Rule.close s'')
 
