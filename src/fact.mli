
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


type t

type rule = string

type justification = rule * t list

type equal
type diseq
type cnstrnt

val mk_equal : Term.t -> Term.t -> justification option -> equal
val mk_diseq : Term.t -> Term.t -> justification option -> diseq
val mk_cnstrnt : Term.t -> Cnstrnt.t -> justification option -> cnstrnt

val d_equal : equal -> Term.t * Term.t * justification option
val d_diseq : diseq -> Term.t * Term.t * justification option
val d_cnstrnt : cnstrnt -> Term.t * Cnstrnt.t * justification option

val of_equal : equal -> t
val of_diseq : diseq -> t
val of_cnstrnt : cnstrnt -> t

val pp : t Pretty.printer

val pp_equal : equal Pretty.printer
val pp_diseq : diseq Pretty.printer
val pp_cnstrnt : cnstrnt Pretty.printer
