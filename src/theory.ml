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

type t = 
  | Top
  | U
  | A 
  | P 
  | F

let to_string = function
  | Top -> "top"
  | U -> "u"
  | A -> "a"
  | P -> "p"
  | F -> "f"

let index = function
  | U -> 0
  | A -> 1
  | P -> 2
  | F -> 3 
  | Top -> 4 

let pp fmt t = Format.fprintf fmt "%s@?" (to_string t)

let equal = (==)
  
let compare t1 t2 =
  let i1 = index t1 and i2 = index t2 in
    if i1 == i2 then 0 else 
      if i1 > i2 then 1 else -1

let sub t1 = function
  | Top -> true
  | t2 -> t1 == t2
    
module T = struct 
  type theory = t (* avoid name clash. *)
  type t = theory
  let equal = equal
  let compare = compare
  let hash = index
  let pp = pp
end

module Map = Map.Make(T)
module Set =  Set.Make(T)
module Hash = Hashtbl.Make(T)

let description = function
  | Top -> "The (disjoint) union of the theories U, A, P, F."
  | U -> "Equality theory of uninterpreted functions."
  | A -> "Theory of linear arithmetic."
  | P -> "Equality theory of pairs."
  | F -> "Equality theory of functional arrays."

module type T = sig
  val theory : t
end

module Top: T = struct let theory = Top end
module U: T = struct let theory = U end
module A: T = struct let theory = A end
module P: T = struct let theory = P end
module F: T = struct let theory = F end

