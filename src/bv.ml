
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
open Term
(*i*)

(*s An tuple context consists of a solution set of equations
 of the form [x = a], where [x] is a variable and [a] is an arithmetic
 term not containing any of the rhs. *)

module BV = Subst.Make(
  struct
    let name = "BV"
    let fold = Bitvector.fold
    let map = Bitvector.map
  end)

type t = BV.t

(*s Empty context. *)

let empty = BV.empty


(*s Accessors. *)

let solution_of = BV.solution


let apply = BV.apply
let find = BV.find
let inv = BV.inv
let mem = BV.mem
let use = BV.use

(*s Add equations between slack variables. *)


let merge e s =
  let (x,y) = Veq.destruct e in
  if BV.occurs s x then
    let (s', es', _) = 
      BV.compose s (Bitvector.solve (BV.norm s x, BV.norm s y))
    in
    (s', Veqs.remove e es')
  else
    (s, Veqs.empty)


(*s Extend. *)

let extend = BV.extend


(*s Pretty-printing. *)

let pp fmt s = 
  if not(s == empty ) then
    begin
      Pretty.string fmt "bv:";
      BV.pp fmt s;
      Pretty.string fmt "\n"
    end

