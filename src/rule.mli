
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


(*s Module [Rule]: encoding of logical rules for manipulating contexts. *)


(*s Merge variable equality. *)

val merge : Fact.equal -> Context.t -> Context.t


(*s Add a constraint. *)

val add : Fact.cnstrnt -> Context.t -> Context.t


(*s Add a disequality. *)

val diseq : Fact.diseq -> Context.t -> Context.t


(*s Tactics. *)

val propagate : Fact.equal -> Context.t -> Context.t

val arith : Fact.equal -> Context.t -> Context.t


(*s Close. *)

val close : Context.t  -> Context.t

val maxclose : int ref

