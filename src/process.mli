
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


(*s Module [Shostak]: extending a logical context using a version
 of Shostak's algorithm. *)


(*s [process s a] extends the logical context [s] with an atom [a].
 The return value is [Valid] if [a] can be shown to be valid in [s],
 [Inconsistent] if [s] conjoined with [a] can be shown to be 
 inconsistent, and [Satisfiable(s')] otherwise. In the latter case,
 [s'] is a logical state equivalent to [s] conjoined with [a]. *)


type 'a status =
  | Valid
  | Inconsistent 
  | Ok of 'a

val pp : 'a Pretty.printer -> 'a status Pretty.printer


val atom: Context.t -> Atom.t -> Context.t status
