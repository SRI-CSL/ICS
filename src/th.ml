(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Theory identifiers. *)

type t = 
  | U                               (* Uninterpreted *)
  | LA | P | BV | COP | SET | L     (* Shostak theories *)
  | NL | ARR                        (* Canonizable *)


let rec to_string = function
  | U -> "u"
  | LA -> "la"
  | P -> "p"
  | BV -> "bv"
  | COP -> "cop"
  | SET -> "pset"
  | L -> "l"
  | NL -> "nl"
  | ARR -> "arr"

let pp fmt th =
  Format.fprintf fmt "%s" (to_string th)


let of_string = function
  | "u" -> U
  | "la" -> LA
  | "p" -> P
  | "bv" -> BV
  | "cop" -> COP
  | "nl" -> NL
  | "c" -> L
  | "arr" -> ARR
  | "pset" -> SET
  | str -> raise (Invalid_argument (str ^ ": no such theory"))


let fold f e =
  f U (f LA (f P (f BV (f COP (f NL (f L (f ARR (f SET e))))))))

let iter f =
  f U; f LA; f P; f BV; f COP; f NL; f L; f ARR; f SET

let for_all f = 
  f U && f LA && f P && f BV && f COP && f NL && f L && f ARR && f SET

type th = t

module Map = Map.Make(
  struct
    type t = th
    let compare = Pervasives.compare
  end)
