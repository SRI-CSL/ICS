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

type t = string

let of_string s = s
let to_string s = s

let eq = (=)

let cmp n m =
  Pervasives.compare n m

let pp fmt s =
  Format.fprintf fmt "%s" s

let hash = Hashtbl.hash

type name = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = name
    let compare = Pervasives.compare
  end)

module Map = Map.Make(
  struct
    type t = name
    let compare = Pervasives.compare
  end)

let pp_map p fmt =
  Map.iter
    (fun x y ->
       pp fmt x;
       Format.fprintf fmt " |-> ";
       p fmt y;
       Format.fprintf fmt "\n")
