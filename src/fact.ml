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
  | Equal of equal
  | Diseq of diseq
  | Cnstrnt of cnstrnt

and justification =
  | Axiom
  | Rule of string * justification list

and equal = Term.t * Term.t * justification option

and diseq = Term.t * Term.t * justification option

and cnstrnt = Term.t * Interval.t * justification option

and rule = string 

let mk_axiom = 
  Some(Axiom)

let mk_rule str jl =
  try
    let jl' =  List.map 
		(function 
		   | Some(j) -> j 
		   | None -> raise Not_found)
		jl
    in
      Some(Rule(str, jl'))
  with
      Not_found -> None

let mk_equal x y j =
  let (x, y) = Term.orient (x, y) in
    Trace.msg "fact" "Equal" (x, y) Term.pp_equal;
    (x, y, j)

let mk_diseq x y j =
  let (x, y) = Term.orient (x, y) in 
    Trace.msg "fact" "Diseq" (x, y) Term.pp_diseq;
    (x, y, j)

let mk_cnstrnt x c j = 
  Trace.msg "fact" "Cnstrnt" (x, c) (Pretty.infix Term.pp " in " Interval.pp);
  (x, c, j)


let d_equal e = e
let d_diseq d = d
let d_cnstrnt c = c

let of_equal e = Equal(e)
let of_diseq d = Diseq(d)
let of_cnstrnt c = Cnstrnt(c)

let rec pp fmt = function
  | Equal(x, y, _) ->  Pretty.infix Term.pp "=" Term.pp fmt (x, y)
  | Diseq(x, y, _) ->  Pretty.infix Term.pp "<>" Term.pp fmt (x, y)
  | Cnstrnt(x, c, _) -> Pretty.infix Term.pp "in" Interval.pp fmt (x, c)

and pp_equal fmt e = 
  let (x, y, _) = d_equal e in
  Pretty.infix Term.pp "=" Term.pp fmt (x, y)

and pp_diseq fmt d = 
  let (x, y, _) = d_diseq d in
  Pretty.infix Term.pp "<>" Term.pp fmt (x, y)

and pp_cnstrnt fmt c = 
  let (x, i, _) = d_cnstrnt c in
  Pretty.infix Term.pp "in" Interval.pp fmt (x, i)

module Equalset = Set.Make(
  struct
    type t = equal
    let compare e1 e2 =
      let (x1, y1, _) = d_equal e1 in
      let (x2, y2, _) = d_equal e2 in
	if Term.eq x1 x2 && Term.eq y1 y2 then
	  0
	else 
	  Pervasives.compare e1 e2
  end)
