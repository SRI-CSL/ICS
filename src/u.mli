
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


(*s Module [Cc]: Congruence closure data structure based on Shostak's
 online congurence closure algorithm with an open-ended term universe.  
 For example, formulas such as
      \begin{verbatim}
      x = y & f(f(f(x))) = f(x) => f(f(f(f(f(y))))) <> f(f(x))
      \end{verbatim}
  can be decided using this module by processing the equalities
  [x = y] and [f(f(f(x))) = f(x)] to obtain a congruence-closed
  data structure [s]. Now, [can s 'f(f(f(f(f(y)))))'] is equal
  to [can s 'f(x)'], and therefore the above formula does not hold. *)


(*s States [s] of type [t] represent the conjunction of a equalities
 of the form [x = f(x1,...,xn)], where [x], [xi] are variables. *)

type t

val solutions : t -> Solution.t

(*s [find s x] returns [f(x1,...,xn)] if [x = f(x1,...,xn)] is in the
 solution set of [s]; otherwise [x] is returned *)

val find : t -> Term.t -> Term.t

val apply : t -> Term.t -> Term.t

val use : t -> Term.t -> Term.Set.t

(*s [inv s a] returns a variable [x] if [a] is of the
 form [f(x1,...,xn)], there is a  binding [a' |-> y] in [u_of s] 
 and if [v s y] is [x]. If there is no such binding, 
 the exception [Not_found] is raised. *)

val inv : t -> Term.t -> Term.t


(*s Some builtin simplification for ['select(update(a,i,x),j)']. 
 If [i] is equal to [j] in [v], then this term reduces to [x],
 and if [i] and [j] are known to be disequal in [d], then the
 term above simplifies to [select(a,j)]. *)

val sigma : V.t * D.t * t -> Sym.t -> Term.t list -> Term.t


(*s [empty] is the empty logical context. *)
  
val empty : t


(*s [extend a s] for [a] of the form [f(x1,...,xn)], [xi] variables,
 adds an equality [a = v], for [v] a fresh label (see module [Term]).
 It returns [v] together with the extended state. *)

val extend : Term.t -> t -> (Term.t * t)

val close : V.t * D.t * t * Focus.t -> V.t * D.t * t * Focus.t

(*s Splitting. *)

val split : V.t * D.t * t -> Atom.Set.t

(*s Pretty-printing. *)

val pp : t Pretty.printer

(*s Instantiation. *)

val inst : V.t -> t -> t
