
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

(*s Module [T]: Tuple equations. *)

type t

val solution_of : t -> (Term.t * Term.t) list

val apply : t -> Term.t -> Term.t
val find : t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.Set.t

(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit

(*s Empty state. *)

val empty : t

(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
  It assumes that [a] is not yet in the domain of [s]. 
  Also, [a],[b] must be valid lhs and rhs, respectively. *)

val extend : Term.t -> t -> Term.t * t

(*s [merge (a,b) s] installs an equality [a = b] into [s]. *)

val merge : Veq.t -> t -> (t * Veqs.t)
