
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

(*s Module [Builtin]:  Normalizing constructors for some builtin
 functions. *)

val is_builtin : Term.t -> bool

val mk_unsigned : Context.t -> Term.t -> Term.t

val mk_select : Context.t -> Term.t -> Term.t -> Term.t
val mk_update : Context.t -> Term.t -> Term.t -> Term.t -> Term.t

val mk_div : Context.t -> Term.t -> Term.t -> Term.t

val mk_sin : Context.t -> Term.t -> Term.t
val mk_cos : Context.t -> Term.t -> Term.t

val mk_floor : Context.t -> Term.t -> Term.t
val mk_ceiling : Context.t -> Term.t -> Term.t


val sigma : Context.t -> Sym.t -> Term.t list -> Term.t
