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

type t = 
  | Yes
  | No
  | X

let is_sub a b =
  match a, b with
    | _, X -> true
    | (X | No), Yes -> false
    | Yes, Yes -> true
    | (X | Yes), No -> false
    | No, No -> true

let inter =
  let yes = Some(Yes) in   (* avoid repetitive creation of constants. *)
  let no = Some(No) in
  let x = Some(X) in
  fun a b -> match a, b with
    | No, X -> no
    | Yes, X -> yes
    | X, No -> no
    | X, Yes -> yes
    | No, No -> no
    | X, X -> x
    | Yes,Yes -> yes
    | No, Yes -> None
    | Yes, No -> None

let union a b =
  match a, b with
    | _, X -> X
    | X, _ -> X
    | No, No -> No
    | Yes,Yes -> Yes
    | No, Yes -> X
    | Yes, No -> X

let is_disjoint a b =
  inter a b = None

let pp fmt three =
  let str = match three with
    | Yes -> "Yes"
    | No -> "No"
    | X -> "X"
  in
    Format.fprintf fmt "%s" str
