
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

(*s Module [Supinf]: conjunctive constraints including lower 
 and upper bounds. *)


type t = {
  sup : Term.Set.t;
  inf : Term.Set.t;
  diseqs : Term.Set.t;
  cnstrnt : Cnstrnt.t
}

val of_cnstrnt : Cnstrnt.t -> t

val is_empty : t -> bool

val inter : t -> t -> t

val inst : Term.t -> Term.t -> t -> t

val pp : t Pretty.printer
