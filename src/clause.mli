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

(** Datatype of clauses

  @author Harald Ruess

*)


type t = disjunction * Jst.t

and disjunction

val unsat : Jst.t -> t

val is_unsat : t -> bool

val of_list : Atom.t list * Jst.t -> t

val singleton : Fact.t -> t

val pp : t Pretty.printer

val eq : t -> t -> bool

val d_singleton: t -> Fact.t
