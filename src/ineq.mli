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

(** {6 Predicates} *)

val is_less : Term.t * bool -> bool
  (** [is_less (a, alpha)] holds iff [a <(=) 0] is derivable in the theory
    of arithmetic. *)

val is_greater : Term.t * bool -> bool
  (** [is_greater (a, alpha)] holds iff [a >(=) 0] is derivable in the theory
    of arithmetic. *)


(** {6 Solving Inequalities} *)

type t =
  | True
  | Less of Term.t * bool * Term.t
  | Greater of Term.t * bool * Term.t

val pp : t Pretty.printer

val solve : Fact.less -> t
