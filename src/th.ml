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

type t =
  | Shostak of shostak
  | Can of can
  | Uninterpreted

and shostak =  A | P | BV | COP

and can = NL | APP | ARR


(** {6 Names} *)

let a = Shostak(A)
let p = Shostak(P)
let bv = Shostak(BV)
let cop = Shostak(COP)

let nl = Can(NL)
let app = Can(APP)
let arr = Can(ARR)

let u = Uninterpreted

let is_shostak = function
  | Shostak _ -> true
  | _ -> false

let is_can = function
  | Can _ -> true
  | _ -> false

let is_uninterpreted = function
  | Uninterpreted -> true
  | _ -> false

let rec to_string th =
  match th with
    | Shostak(i) -> shostak_to_string i
    | Can(i) -> can_to_string i
    | Uninterpreted -> "u"

and shostak_to_string i =
  match i with
    | A -> "a"
    | P -> "p"
    | BV -> "bv"
    | COP -> "cop"

and can_to_string i =
  match i with
    | NL -> "nl"
    | APP -> "app"
    | ARR -> "arr"


let of_string = function
  | "u" -> u
  | "a" -> a
  | "p" -> p
  | "bv" -> bv
  | "cop" -> cop
  | "nl" -> nl
  | "app" -> app
  | "arr" -> arr
  | str -> raise (Invalid_argument (str ^ ": no such theory"))


let fold f e =
  f u (f a (f p (f bv (f cop (f nl (f app (f arr e)))))))

let iter f =
  f u; f a; f p; f bv; f cop; f nl; f app; f arr

let for_all f =
  f u && f a && f p && f bv && f cop && f nl && f app && f arr

let exists f =
  f u || f a || f p || f bv || f cop || f nl || f app || f arr

let for_all_but i p =
  let p' j = if i <> j then p j else true in
    for_all p'

let exists_but i p =
  let p' j = if i <> j then p j else true in
    exists p'

let of_sym = function
  | Sym.Uninterp _ -> u
  | Sym.Arith _ -> a
  | Sym.Pair _ -> p
  | Sym.Bv _ -> bv
  | Sym.Coproduct _ -> cop
  | Sym.Arrays _ -> arr
  | Sym.Pp _ -> nl
  | Sym.Fun _ -> app


let inj =
  let a = Some(a)
  and p = Some(p)
  and bv = Some(bv)
  and cop = Some(cop)
  and nl = Some(nl)
  and app = Some(app)
  and arr = Some(arr)
  and u = Some(u) in
    function
      | Shostak(i) -> (match i with A -> a | P -> p | BV -> bv | COP -> cop)
      | Can(i) -> (match i with APP -> app | ARR -> arr | NL -> nl)
      | Uninterpreted -> u
    

let pp fmt = function
  | None -> Format.fprintf fmt "v"
  | Some(i) -> Format.fprintf fmt "%s" (to_string i)
