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


type t = Extq.t * bool

let make ((a, alpha) as e) =
  e

let destruct e = e

let value e = fst e
let kind e = snd e

let neginf = (Extq.neginf, false)
let posinf = (Extq.posinf, false)

let strict u = (Extq.of_q u, false)
let nonstrict u = (Extq.of_q u, true)

let eq (a,alpha) (b,beta) =
  match Extq.destruct a, Extq.destruct b with
    | Extq.Inject u, Extq.Inject v -> alpha = beta && Mpa.Q.equal u v
    | Extq.Posinf, Extq.Posinf -> true
    | Extq.Neginf, Extq.Neginf -> true
    | _ -> false

let is_q (a,_) =  Extq.is_q a

let is_z (a,_) = Extq.is_z a

let q_of (a,_) = 
  assert(Extq.is_q a);
  match Extq.to_q a with
    | Some(q) -> q
    | _ -> assert false

let z_of (a,_) = 
  assert(Extq.is_z a);
  match Extq.to_z a with
    | Some(q) -> q
    | _ -> assert false

let is_strict (_,alpha) = not alpha

let is_nonstrict (_,alpha) = alpha

