(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)


(** Theory of arithmetic interpretations of bitvectors.
  Currently, only unsigned interpretations are supported.

  @author Harald Ruess
*)

(** {6 Function Symbols} *)

val unsigned : Sym.t

(** {6 Recognizers} *)

val is_interp : Term.t -> bool

(** {6 Constructors} *)

val mk_unsigned : Term.t -> Term.t

(** {6 Canonizer} *)

val sigma : Sym.bvarith -> Term.t list -> Term.t


(** {6 Iterators} *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
