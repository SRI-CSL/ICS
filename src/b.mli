
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

(*s Module [B]: Context of Boolean equalities of the form [x = true]
 or [x = false], where [x] is a variable and [true], [false] are the
 Boolean constants (see module [Boolean]). *)

type t

(*s [solution_of s] yields a list of pairs of the form [(x,true)]
 or [(x,false)], where [x] is a variable. *)

val solution_of : t -> (Term.t * Term.t) list

(*s [find s x]. *)

val apply : t -> Term.t -> Term.t
val find : t -> Term.t -> Term.t
val apply : t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.Set.t



(*s Pretty-printing. *)

val pp : Format.formatter -> t -> unit

(*s The initial logical context includes two entries [t = true] and
 [f = false] for the 'Boolean' variables [t] and [f]. *)

val empty : t

(*s [merge 'x = y' s] propagates an equality ['x = y'] in the
 logical context [s]. It results in a new logical context in which
 every [x] is replaced by [y] and a set of newly deduced equalities.
 The exception [Exc.Inconsistent] is raised if an inconsistency is
 detected. *)

val merge : Veq.t -> t -> (t * Veqs.t)

(*s Extend. *)

val extend : Term.t -> t -> Term.t * t


