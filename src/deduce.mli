
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

(*s Module [Deduce]. Deducing new facts from a variable equality. *)

val deduce : Fact.equal -> Context.t -> Context.t

val of_pprod : Term.t -> Sym.pprod * Term.t list -> Context.t -> Context.t
 
val of_linarith : Term.t -> Sym.arith * Term.t list -> Context.t -> Context.t

val of_bvarith : Term.t -> Sym.bvarith * Term.t -> Context.t -> Context.t
