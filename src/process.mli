
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

type 'a status = 
  | Valid 
  | Inconsistent 
  | Satisfiable of 'a


(*s [atom s a] extends the context [s] with an atom [a].
 If [Valid] is returned, then [p] could be deduced from [s].
 If processing yields [Inconsistent], then [p] together with [s]
 is inconsistent. Otherwise, [p] is added to the context [s] and
 an extended context [s'] equivalent to [s] conjoined with [p] is
 returned. *)

val atom : Dp.t -> Atom.t -> Dp.t status

(*s [is_valid s a] iff [s |- a]. *)

val is_valid : Dp.t -> Atom.t -> bool

(*s Processing of a propositional structure. *)

val prop : Dp.t -> Prop.t -> Dp.t status
