
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

module T = Subst.Make(
  struct
    let name = "t"
    let fold = Arith.fold
    let map = Arith.map
  end)

type t = T.t

(*s Empty context. *)

let empty = T.empty

(*s Accessors. *)

let solution_of = T.solution


let apply s = T.apply s
let find s = T.find s
let inv s = T.inv s
let mem s = T.mem s
let use s = T.use s


(*s Add equations between slack variables. *)

let rec merge e s =
  let (x,y) = Veq.destruct e in
  if T.occurs s x then           (* 'forget' equalities between internal variables. *)
    let (s', es') = T.compose s (Tuple.solve (T.norm s x, T.norm s y)) in
    (s', Veqs.remove e es')
  else
    (s, Veqs.empty)

(*s Extend. *)

let extend = T.extend

(*s Pretty-printing. *)

let pp fmt s = 
  if not(s == empty ) then
    begin
      Pretty.string fmt "t:";
      T.pp fmt s;
      Pretty.string fmt "\n"
    end


