
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
 * Author: Harald Ruess, N. Shankar
i*)

(*s Module [V]: Equalities and disequalities over variables. *)

type t = {
  v: V.t;
  d : D.t;
  vfocus : V.focus;
  dfocus : D.focus
}

val empty : t

val is_confluent : t -> bool

val find : t -> Term.t -> Term.t
val deq : t -> Term.t -> Term.Set.t

val is_equal : t -> Term.t -> Term.t -> Three.t

val pp : t Pretty.printer
