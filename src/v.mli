
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

(*s Module [V]: operations on variables. *)

(*s [is a] holds iff [a] is considered to be a variable, that is
 if its argument list is empty and if the symbol is either uninterpreted
 or it is an internal label or that of a Boolean constant. *)

val is : Term.t -> bool


(*s Orient variable [(x,y)] such that [y <<< x]. *)

val (<<<): Term.t -> Term.t -> bool

val orient : Term.t * Term.t -> Term.t * Term.t


(*s Check for inconsistency between certain Boolean "variables". *)

val inconsistent : Term.t -> Term.t -> bool


(*s Equalities between variables. *)

type eqs = (Term.t * Term.t) list

val empty : eqs

val add : Term.t -> Term.t -> eqs -> eqs

val union : eqs -> eqs -> eqs

val pp : Format.formatter -> eqs -> unit
