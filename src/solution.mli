
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

(*s Module [Solution]: abstract datatype for representing
 and manipulating conjunctions of equations [x = a], where
 [x] is a variable and [a] is a non-variable term. As an invariant,
 solution sets [s] are kept in functional form, that is, if [x = a]
 and [x = b] in [s], then [a] is identical with [b]. In addition,
 solution sets are injective, that is, [x = a] and [y = a] are not
 in a solution set for [x <> y]. *)

type t


(*s [to_list s] returns a list of pairs [(x, a)], where [x] is a
 variable and [a] a term for the equalities [x = a] in the 
 solution set [s]. *)

val to_list : t -> (Term.t * Term.t) list


(*s Pretty-printing of solution sets. *)

val pp : Th.t -> t Pretty.printer


(*s [fold f s e] applies [f x a] to all [x = a] in
 the solution set [s] in an unspecified order and
 accumulates the result. *)

val fold : (Term.t -> Term.t * Fact.justification option -> 'a -> 'a) -> t -> 'a -> 'a



(*s [apply s x] returns [b] if [x = b] is in [s], and 
 raises [Not_found] otherwise. *)

val apply : t -> Term.t -> Term.t


(* [find s x] returns [b] if [x = b] is in [s], and [x] otherwise. 
 For non-variable argument [a], [find s a] always returns [a]. *)

val find : t -> Term.t -> Term.t

(*s [justification s x] returns [(b, j)] if [x = b] is in [s] with justification [j].
 Raises [Not_found] if there is no such justification. *)

val justification : t -> Term.t -> Term.t * Fact.justification option

val equality : t -> Term.t -> Fact.equal


(*s [inv s b] returns [x] if [x = b] is in [s]; 
 otherwise [Not_found] is raised. *)

val inv : t -> Term.t -> Term.t


(*s [mem s x] holds iff [x = b] is in [s]. *)

val mem : t -> Term.t -> bool


(*s [occurs s x] holds if either [mem s x] or if [x] is
 a variable in some [b] where [y = b] is in [s]. *)

val occurs : t -> Term.t -> bool


(*s [use s x] returns all [y] such that [y = a] in [s]
 and [x] is a variable in [Term.vars a]. *)

val use : t -> Term.t -> Term.Set.t


(*s The [empty] solution set, which does not contain any equality. *)

val empty : t


(*s [is_empty s] holds iff [s] does not contain any equalities. *)

val is_empty : t -> bool


(*s [eq s t] tests if the solution sets [s] and [t] are identical
 in the sense that the sets of equalities are stored at the same
 memory location. *)

val eq : t -> t -> bool


(*s [restrict s x] removes equalities [x = a] from [s]. *)

val restrict : Th.t -> Term.t -> t -> t 


(*s [union (x, b) s] adds an equality [x = b] to [s], 
 possibly removing an equality [x = b'] in [s]. *)

val union : Th.t -> Fact.equal -> t -> t

(*s [name s a] returns the variable [x] if there is
 an equation [x = a] in [s].  Otherwise, it creates a 
 fresh variiable [x'] and installs a solution [x' = a] in [s]. *)

val name : Th.t -> Term.t * t -> Term.t * t

(*s [fuse norm (p, s) r] propagates the equalities in [r] on 
  the right-hand side of equalities in [s]. The return value [(p', s')] consists 
vi  of an extension of the partition [p] with newly generated variable equalities
  and a modified solution set [s'], which is obtained by transforming every
  [x = b] in [s] to [x = norm(r)(b)].  Here, [norm(r)(b)] replaces occurrences
  of [z] in [b] with [a] if [z = a] is in [r] and normalizes the result according
  to the [norm] argument function. If [norm(r)(b)] reduces
  to a variable, say [y], then the variable equality [x = y] is added to
  the partition [p]. This may trigger an exception [Not_found], if the
  disequality [x <> y] can be deduced from the partition [p].  This equality
  is also propagated in the resulting [s'] in that every occurrence of [x]
  is replaced by [y].  In case [norm(r)(b)] results in a non-variable [b'],
  the equality [x = b'] is added if there is no [y = b'] already in the solution 
  set. If there is such a [y], then the equality [x = y] is added to the 
  partitioning [p] and only one of [x = b'], [y = b'] is retained in the
  resulting solution set. *)

val fuse : Th.t -> Partition.t * t -> Fact.equal list -> Partition.t * t


(*s [compose norm (p,s) r] is a [fuse] step followed by
 extending (and possibly overwriting [x = ...]) 
 the resulting [s'] with all [x = b], for [b]
 a non-variable term, in [sl]. If [b] is a variable, then
 it is added to [v'] and [ch'] is extended accordingly. *)

val compose : Th.t -> Partition.t * t -> Fact.equal list -> Partition.t * t


(*s Every modification or addition of an equality [x = a] to
 a solution set [s]--- using [union], [fuse], or [compose]---has the side-effect 
 of adding [x] to the set of changed variables in [s]. This
 set can be obtained by [changed s]. [reset s] resets the set of
 changed variables in [s] to the empty set. *)
 
module Changed: sig

  type t = Term.Set.t Th.Array.arr

  val reset : unit -> unit
    
  val save : unit -> t
    
  val restore : t -> unit
    
  val stable : unit -> bool

end 
