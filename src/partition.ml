
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
 * Author: Harald Ruess, N. Shankar
i*)

(*i*)
open Term
open Three
(*i*)

type t = {
  v: V.t;
  d: D.t;
  vfocus : V.focus;
  dfocus : D.focus
}

let empty = {
 v = V.empty;
 d = D.empty;
 vfocus = V.Focus.empty;
 dfocus = D.Focus.empty
}

let find s = V.find s.v
let deq s = D.deq s.d


let is_equal p x y = 
  let x' = find p x in
  let y' = find p y in
  if Term.eq x' y' then Three.Yes
  else if D.is_diseq p.d x' y' then Three.No
  else Three.X

let is_confluent p =
  V.Focus.is_empty p.vfocus && 
  D.Focus.is_empty p.dfocus


let pp fmt p =
  V.pp fmt p.v;
  D.pp fmt p.d
