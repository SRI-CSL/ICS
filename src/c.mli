
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

(*s Module [C]:  Database for onstraint declarations of the 
 form [x |-> c] where [x] is an uninterpreted term and [c] is
 a numerical constraint. *)
  
type t
  

(*s [empty] is the empty logical context. *)
  
val empty : t


(*s [cnstrnt_of s] returns a finite map of bindings [x |-> c]
 from the representation [s]. *)

val cnstrnt_of : t -> Number.t Term.map


(*s [apply s a] returns [c] if there is a binding [a |-> c];
 otherwise [apply s a] raises [Not_found]. *)

val apply : t -> Term.t -> Number.t


(*s Propagating an equality [a = b] to the constraint
 database. Constraints for [a] are deleted in favor of
 those for [b].  As a side effect, newly generated 
 equalities may be pushed on the pending stack of goals,
 and the exception [Exc.Inconsistent] is raised if the
 interpretation domain of [b] becomes empty. *)


val propagate : Term.t * Term.t -> t -> t


(*s Merging a new constraint [a in c] into the constraint
 database. Raises [Exc.Inconsistent] if the interpretation
 domain of [a] becomes empty, and newly generated equalities
 are pushed on the pending stack of goals. *)

val process: Number.t -> Term.t -> t -> t
