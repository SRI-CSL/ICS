
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


(*s Module [Partition]: A partition consists of a set of variable equalities
 [x = y], a set of variable disequalities [x <> y], and a set ofvariable 
 constraints [x in i], where [i] is an arithmetic constraint of type [Cnstrnt.t]. *)


type t = {
  v : V.t;              (* Variable equalities. *)
  d : D.t;              (* Variables disequalities. *)
  c : C.t               (* Constraints. *)
}

(*s The [empty] partition. *)

val empty : t
 
(*s [v s x] returns the canonical representative of the equivalence
 class in the partitioning [s] containing the variable [x]. *)

val v : t -> Term.t -> Term.t


(*s [update_v s v] updates the [v] part of the partitioning [s] if it
 is different from [s.v]. Similarly,  [update_d] and [update_c] update
 the disequlity part and the constraint part, respectively. *)

val update_v : t -> V.t -> t
val update_d : t -> D.t -> t
val update_c : t -> C.t -> t


(*s [cnstrnt s a] computes a constraint for [s] by using variable constraints
 [x in i] in the variable constraint part [c] of the partitioning [s], and by
 recursing over arithmetic terms using abstract interval interpretation as
 defined in the module [Cnstrnt].  If the term is unconstrained, the exception
 [Not_found] is raised. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t


(*s [is_int s a] tests if the constraint [cnstrnt s a] is included in [Cnstrnt.mk_int]. *)

val is_int : t -> Term.t -> bool

(*s [deq s x] returns the set of all variable [y] disequal to [x] as stored
 in the variable disequality part [d] of the partitioning [s].  Disequalities as
 obtained from the constraint part [c] are not necessarily included. *)

val deq : t -> Term.t -> Term.Set.t


(*s Pretty-printing of a partitioning. *)
  
val pp : t Pretty.printer

(*s [is_equal s x y] for variables [x], [y] returns [Three.Yes] if [x] and
 [y] belong to the same equivalence class modulo [s], that is, if [v s x]
 and [v s y] are equal. The result is [Three.No] if [x] is in [deq y],
 [y] is in [deq x], or [x in i] and [y in j] are constraints in [s] and [i],
 [j] are disjoint. Otherwise, [Three.X] is returned. *)

val is_equal : t -> Term.t -> Term.t -> Three.t
 

(* [merge e s] adds a new variable equality [e] of the form [x = y] into
 the partition [s]. If [x] is already equal to [y] modulo [s], then [s]
 is unchanged; if [x] and [y] are disequal, then the exception [Exc.Inconsistent]
 is raised; otherwise, the equality [x = y] is added to [s] to obtain [s'] such
 that [v s' x] is identical to [v s' y]. *)

val merge : Fact.equal -> t -> t

(*s [remove s] removes all internal variables which are not canonical. *)

val removable : t -> Term.Set.t

val restrict : Term.Set.t -> t -> t


(*s [add c s] adds a constraint of the form [x in i] to the constraint part [c]
 of the partition [s]. May raise [Exc.Inconsistent] if the resulting constraint
 for [x] is the empty constraint (see [C.add]). *)

val add : Fact.cnstrnt -> t -> t
  

(*s [diseq d s] adds a disequality of the form [x <> y] to [s]. If [x = y] is
 already known in [s], that is, if [is_equal s x y] yields [Three.Yes], then
 an exception [Exc.Inconsistent] is raised; if [is_equal s x y] equals [Three.No]
 the result is unchanged; otherwise, [x <> y] is added using [D.add]. *)

val diseq : Fact.diseq -> t -> t
 

(*s [eq s t] holds if the respective equality, disequality, and constraint parts
 are identical, that is, stored in the same memory location. *)

val eq : t -> t -> bool


type index = V | D | C

(*s [changed s] returns the changed sets for the equality, the disequality,
 and the constraint part (in this order).  The changed set for equalities
 contains variables [x] for which [v s x] changed since the last [reset].
 Similarly, [x <> y] is in the changed set of the disequality part, if 
 [x <> y] has been added, since the last [reset], and [x] is in the 
 changed set of the constraint part, if a refined constraint [x in i]
 has been added since the last [reset]. *)

val changed : t -> Term.Set.t * Fact.diseq list * Term.Set.t

val changed_v : t -> Term.Set.t
val changed_d : t -> Fact.diseq list
val changed_c : t -> Term.Set.t

(*s Resetting the [changed] indices to empty sets. *)

val reset : index -> t -> t
 
val reset_v : t -> t
val reset_d : t -> t
val reset_c : t -> t



