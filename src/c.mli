
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

val cnstrnts : t -> (Cnstrnt.t * Fact.justification option) Var.Map.t

val to_list : t -> (Var.t * Cnstrnt.t) list

val apply : t -> Term.t -> Cnstrnt.t

val to_fact : t -> Term.t -> Fact.cnstrnt

val mem : Term.t -> t -> bool

(*s Empty constraint map. *)

val empty : t 

val eq : t -> t -> bool

(*s Add a new constraint. *)

val add : Fact.cnstrnt -> t -> t

(*s Changed. *)

val changed : Set.t ref

(*s Merge a variable equality [x = y] in the constraint map by
 adding [x in ij] for the canonical variable [x], where [x in i],
 [y in j] are in the constraint map and [ij] is the intersection of
 [i] and [j], and by removing the constraint for [y]. In case, [ij]
 is a singleton constraint with element [q], an equality [x = q] is
 generated. Singleton constraints are always retained in the constraint
 map in order to keep the invariant that the best available constraint
 are always associated with canonical variables. *)

val merge : Fact.equal -> t -> t


(*s Propagate disequalities to the constraint part. The following
 is not complete and should be extended to all finite constraints,
 but the disequality sets might become rather large then. *)

val diseq : Fact.diseq -> t -> t



(*s Split. *)

val split : t -> Atom.Set.t

(*s Pretty-printing. *)

val pp : t Pretty.printer
