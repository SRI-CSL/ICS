(*
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
 *)

type t = 
  | True
  | False
  | Less of Term.t * bool * Term.t
  | Greater of Term.t * bool * Term.t

val is_le : Term.t -> Term.t -> bool
  (** [is_le a b] holds iff the nonstrict inequality [a <= b] 
    holds in the rationals. *)

val is_lt : Term.t -> Term.t -> bool
  (** [is_lt a b] holds iff the strict inequality [a < b] 
    holds in the rationals. *)
  
val is_less : Term.t * bool * Term.t -> bool

val is_greater : Term.t * bool * Term.t -> bool

val mk_less : Term.t * bool * Term.t -> t

val mk_greater: Term.t * bool * Term.t -> t

val mk_lt : Term.t -> Term.t -> t
val mk_le : Term.t -> Term.t -> t
val mk_gt : Term.t -> Term.t -> t
val mk_ge : Term.t -> Term.t -> t

val mk_negate : t -> t

val pp : t Pretty.printer


val solve : t -> t
