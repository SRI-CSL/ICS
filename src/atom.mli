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

(** Atomic predicates.

  An atomic predicate is either the 
  - constant [True] or [False] predicate,
  - an equality, 
  - disequality, or 
  - an arithmetic constraint.

  @author Harald Ruess
*)

type t  =
  | True
  | Equal of Fact.equal
  | Diseq of Fact.diseq
  | In of Fact.cnstrnt
  | False


(** {6 Constructors} *)

val mk_true : unit -> t

val mk_false : unit -> t

val mk_equal : Fact.equal -> t

val mk_diseq : Fact.diseq -> t

val mk_in : Fact.cnstrnt -> t


(** {6 Pretty-printing} *)

val pp : t Pretty.printer


(** {6 Set of atoms} *)

module Set : (Set.S with type elt = t)
