
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

(** Array decision procedures

  @author Harald Ruess
  @author N. Shankar
*)

type t

type config = Partition.t * t

val eq : t -> t -> bool

val pp : t Pretty.printer



(** {6 Accessors} *)

val apply : t -> Justification.Eqtrans.t

val find : t -> Justification.Eqtrans.t

val inv : t -> Justification.Eqtrans.t

val dep : t -> Term.t -> Term.Set.t

val is_dependent : t -> Term.t -> bool

val is_independent : t -> Term.t -> bool


val empty : t

val is_empty : t -> bool

val copy : t -> t

val name : config -> Justification.Eqtrans.t


(** {6 Processing} *)

val process_equal : config -> Fact.Equal.t -> unit

val process_diseq : config -> Fact.Diseq.t -> unit

