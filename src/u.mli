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

(** Congruence closure

  @author Harald Ruess
  @author N. Shankar

  A {i congruence closure} state represents the conjunction of 
  a set of equalities [x = f(x1,...,xn)] with [x], [xi] term variables and [f] 
  an uninterpreted function symbol. This set of equalities is 
  - {i injective} in that [x = a] and [y = a] implies [x = y], and 
  - {i functional} in that [x = a] and [x = b] implies [a = b].
*)


module S: Solution.SET0

(** Congruence closure inference system. *)
module Infsys: (Infsys.EQ with type e = S.t)
