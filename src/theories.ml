
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


type t = 
  | Uninterp 
  | Interp of Interp.t

let index a =
  assert(not(Term.is_var a));
  match Interp.index (Term.sym_of a) with
    | Some(i) -> Interp(i)
    | None -> Uninterp

let name_of = function
  | Uninterp -> "u"
  | Interp(i) -> Interp.name_of i

let of_name = function
  | "u" -> Uninterp
  | x -> Interp(Interp.of_name x)  (* might raise [Invalid_argument] *)

let a = Interp(Interp.A)
let t = Interp(Interp.T)
let b = Interp(Interp.B)
let bv = Interp(Interp.BV)
let u = Uninterp
