
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
  | A        (*s Arithmetic. *)
  | T        (*s Tuples. *)
  | B        (*s Boolean. *)
  | BV       (*s Bitvectors. *)

let name_of = function
  | A -> "a"
  | T -> "t"
  | B -> "b"
  | BV -> "bv"

let of_name = function
  | "a" -> A
  | "b" -> B
  | "t" -> T
  | "bv" -> BV
  | str -> raise (Invalid_argument (str ^ "not an interpreted theory name."))


let index f =
  match Sym.destruct f with
    | Sym.Interp(op) ->
	Some(match op with
	       | Sym.Arith _ -> A
	       | Sym.Tuple _ -> T
	       | Sym.Boolean _ -> B
	       | Sym.Bv _ -> BV)
    | _ ->
	None
