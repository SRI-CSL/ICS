
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

type tests = {
  is_equal : Term.t -> Term.t -> bool;
  is_diseq : Term.t -> Term.t -> bool;
  cnstrnt : Term.t -> Cnstrnt.t option;
  find : Theories.t -> Term.t -> Term.t option
}

val mk_unsigned : tests -> Term.t -> Term.t

val mk_select : tests -> Term.t -> Term.t -> Term.t
val mk_update : tests -> Term.t -> Term.t -> Term.t -> Term.t

val mk_div : tests -> Term.t -> Term.t -> Term.t

val mk_sin : tests -> Term.t -> Term.t
val mk_cos : tests -> Term.t -> Term.t

val mk_floor : tests -> Term.t -> Term.t
val mk_ceiling : tests -> Term.t -> Term.t


val sigma : tests -> Sym.t -> Term.t list -> Term.t
