
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

type t = Same | Disjoint | Sub | Super | Overlap

    (*s If [r1] is the best relation in [t] between [s1] and [t1] and
        [r2] is the best relation between [s2] and [t2], then
        [union r1 r2] is the best relation between [union s1 t1] and [union s2 t2].
      *)
      

let union r1 r2 =
  if r1 = r2 then
    r1
  else
  match r1, r2 with
    | Overlap, _ -> Overlap
    | _, Overlap -> Overlap
    | Same, Sub -> Sub
    | Same, Super -> Super
    | Same, _ -> Overlap
    | Disjoint, _ -> Overlap
    | Sub, Same -> Sub
    | Sub, Disjoint -> Overlap
    | Sub, Super -> Overlap
    | Super, Same -> Super
    | Super, Disjoint -> Overlap
    | Super, Sub -> Overlap
    | _ -> assert false
