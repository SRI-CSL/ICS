
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

(*s Module [App]: Operations on uninterpreted terms. *)

val sigma : Sym.t -> Term.t list -> Term.t

val lazy_sigma : Term.t -> Sym.t -> Term.t list -> Term.t



(*s [is_uninterp a] holds iff the function symbol of [a] is
 uninterpreted (see module [Sym]). *)

val is_uninterp : Term.t -> bool


(*s For a term [a] such that [is_uninterp a] holds, [d_uninterp a]
 returns the uninterpreted function symbol and the argument list of [a]. *)

val d_uninterp : Term.t -> Sym.t * Term.t list
