
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

(*s The module [Atom] implements constructors for atomic predicates. *)

type t  =
  | True
  | Equal of Fact.equal
  | Diseq of Fact.diseq
  | In of Fact.cnstrnt
  | False


(*s Constructors. *)

val mk_true : unit -> t
val mk_false : unit -> t

val mk_equal : Fact.equal -> t

val mk_diseq : Fact.diseq -> t

val mk_in : Fact.cnstrnt -> t


(*s Pretty-printing. *)

val pp : t Pretty.printer


(*s The less-then constructor [lt (a,b)] builds a constraint corresponding to
  the fact that the difference [sub(a,b)] is negative. Similarly, 
  the less-or-equal constructor [le] associates a non-positive constraint to [sub(a,b)].
  In addition, [sub(a,b)] is normalized so that the coefficient of its least power product, 
  with respect to [<<<], is one. *)

val mk_lt : Term.t -> Term.t -> t
val mk_le : Term.t -> Term.t -> t


(*s Set of atoms. *)

module Set : (Set.S with type elt = t)
