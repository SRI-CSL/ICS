
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

(*s Module [V]: Representation of equivalence classes of variables. *)

type t

(*s [partition s] returns a partitioning of the set of variables
 in the form of a map with a domain consisting of canonical
 representatives and the corresponding equivalence class in
 the codomain. It does only list non-singleton equivalence classes. *)

val partition : t -> Term.Set.t Term.Map.t

(*s [find s x] returns the canonical representative of [x]
 with respect ot the partitioning of the variables in [s].
 In addition, [find'] performs dynamic path compression as
 a side effect. *)

val find : t -> Term.t -> Term.t

val find' : t -> Term.t -> t * Term.t

(*s [changed s] returns the set of variables [x] with a new
 canonical representative, where 'new' is relative to the last
 [reset s]. *)

val changed : t -> Term.Set.t

val reset : t -> t

val eq : t -> t -> bool

(*s [is_equal s x y] holds if and only if [x] and [y] are
 in the same equivalence class modulo [s]. *)

val is_equal : t -> Term.t -> Term.t -> bool


(*s The empty context. *)

val empty : t


(*s Adding a variable equality [x = y] to a context [s]. *)

val merge : Fact.equal -> t -> t


(*s [restrict x s] removes occurrences of [x] from [s].
 Should only be called for [x] in [removable s]. *)

val restrict : Term.t -> t -> t

val removable : t -> Term.Set.t


(*s Pretty-printing. *)

val pp : t Pretty.printer


(*s Folding over the members of a specific equivalence class. *)

val fold : t -> (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a


(*s Iterate over the extension of an equivalence class. *)

val iter : t -> (Term.t -> unit) -> Term.t -> unit

(*s [exists s p x] holds if [p y] holds for some [y] congruent
 to [x] modulo [s]. *)


val exists : t -> (Term.t -> bool) -> Term.t -> bool

(*s [for_all s p x] holds if [p y] holds for all [y] congruent
 to [x] modulo [s]. *)


val for_all : t -> (Term.t -> bool) -> Term.t -> bool

(*s [choose s p x] chooses a [y] which is congruent to [x] modulo [s]
  which satisfies [p]. If there is no such [y], the exception [Not_found]
  is raised. *)

val choose : t -> (Term.t -> 'a option) -> Term.t -> 'a
