
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

type t = Term.t * Term.t

let make x y =
  assert(Term.is_var x && Term.is_var y);
  Trace.msg "veq" "" (x, y) Term.pp_equal;
  Term.orient (x,y)

let destruct e = e

let eq (x1,y1) (x2,y2) =
  Term.eq x1 x2 && Term.eq y1 y2

let cmp (x1,y1) (x2,y2) =
  let cmp1 = Term.cmp x1 x2 in
  if cmp1 = 0 then
    Term.cmp y1 y2
  else 
    cmp1

let pp = Pretty.infix Term.pp "=" Term.pp
