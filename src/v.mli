
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

(*s Module [V]: Equalities and disequalities over variables. *)

type t

(*s [partition s] returns a partitioning of the set of variables
 in the form of a map with a domain consisting of canonical
 representatives and the corresponding equivalence class in
 the codomain. *)

val partition : t -> Term.Set.t Term.Map.t

(*s [diseqs s] lists the set of known disequalities in the
 form of a map with associates with a variable [x]
  the set of all variables known to be disequal to [x]. *)
 
val diseqs : t -> Term.Set.t Term.Map.t


(*s [find s x] returns the canonical representative of [x]
 with respect ot the partitioning of the variables in [s].
 In addition, [find'] performs dynamic path compression as
 a side effect. *)

val find : t -> Term.t -> Term.t

val find' : t -> Term.t -> t * Term.t


(*s Variable equality/disequality modulo [s]. *)

val eq : t -> Term.t -> Term.t -> bool

val deq : t -> Term.t -> Term.t -> bool

(*s [is_equal s x y] is [Yes] if [eq s x y] holds,
 [No] if [x <> y] is a known disequality in [s], and [X]
 for ``don't know'' otherwise. *)

val is_equal : t -> Term.t -> Term.t -> Three.t

(*s The empty context. *)

val empty : t

(*s Adding a variable equality [x = y] to a context [s].
 May throw [Exc.Inconsistent]. *)

val merge : Veq.t -> t -> t

(*s Adding a disequality. May throw [Exc.Inconsistent]. *)

val diseq : Term.t * Term.t -> t -> t
 

(*s Pretty-printing. *)

val pp : t Pretty.printer
