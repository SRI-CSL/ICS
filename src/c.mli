
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

(*i*)
open Term
(*i*)

type t

val domains : t -> (Term.t * Cnstrnt.t) list

val pp : t Pretty.printer

val empty : t

val is_empty : t -> bool

val mem : Term.t -> t -> bool

val apply : t -> Term.t -> Cnstrnt.t

val find : t -> Term.t -> Cnstrnt.t

val split : t -> (Term.t * Cnstrnt.t) list

(*s Merging all equalities [x = y] in [veqs] by
 combining their respective constraints. New equalities
 of the form [x = q] where [q] is a rational number might 
 be deduced. *)

val merge: (Term.t * Term.t) list -> t -> t * (Term.t * Term.t) list * Term.Set.t

(*s [add x i s] add a new constraint [x in i] to [s].
  [x] should be a variable. An equality for [x] might
  be generated. *)

val add : Term.t -> Cnstrnt.t -> t -> t * (Term.t * Term.t) option
