
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
 [x = y] over variables and equalities of the form [f(x1,...,xn) = x],
 where [x], [xi] are variables. *)
  
type t


(*s [v_of s] returns a representation of an ordered set of 
 equalities [x1 = y1, ..., xn = yn], where all [xi] is greater 
 than [yi] according to the term ordering [<<<]. *)

val partition : t -> Term.Set.t Term.Map.t

(*s All known disequalities between variables. *)

val diseqs : t -> Term.Set.t Term.Map.t

(*s [find s x] returns [f(x1,...,xn)] if [x = f(x1,...,xn)] is in the
 solution set of [s]; otherwise [x] is returned *)

val find : t -> Term.t -> Term.t


(*s For every term [f(x1,...,xn)] in the domain of [u_of s], 
 [f(x1,...,xn)] is an element of [use_of s] applied to [xi]. 
 In addition, if [x |-> y] is in [v_of s], then [use_of s] 
 applied to [x] is a subset of [use_of s] applied to [y]. *)

val use : t -> Term.t -> Term.Set.t

(*s Return solution set. *)

val solution : t -> (Term.t * Term.t) list


(*s For a variable [x], [v s x] returns the variable [y] if there 
 are bindings [x |-> x1, x2 |-> x3,..., xn |-> y] and no binding
 [y |-> ...] in [v_of s]. Otherwise, if there is no such binding 
 [x |-> ...], [v s x] just returns [x]. *)

val v : t -> Term.t -> Term.t


(*s Two variables [x] and [y] are said to be equivalent w.r.t. to [v_of s]
 if [v s x] is term equal with [v s y]. This equivalence relation partitions
 the set of variables into equivalence classes with [v s x] the canonical 
 representative of the class containing variable [x]. *)

val veq : t -> Term.t -> Term.t -> bool


(* check if two terms are known to be equal *)

val is_equal : t -> Term.t -> Term.t -> Three.t


(*s Accessors. [inv s a] returns a variable [x] if [a] is of the
 form [f(x1,...,xn)], there is a  binding [a' |-> y] in [u_of s] 
 and if [v s y] is [x]. If there does not exist such a binding, 
 the exception [Not_found] is raised. *)

val inv : t -> Term.t -> Term.t


(*s [empty] is the empty logical context. *)
  
val empty : t

(*s New equalities [x = y] between variables are added to
 state [s] with [merge (x,y) s]. This results in an new state
 [s'] and a list of all newly implied variable equalities. *)

val merge : Veq.t -> t -> (t * Veqs.t)


(*s [extend a s] for [a] of the form [f(x1,...,xn)], [xi] variables,
 adds an equality [a = v], for [v] a fresh label (see module [Term]).
 It returns [v] together with the extended state. *)

val extend : Term.t -> t -> (Term.t * t)

(*s Adding a disequality [x <> y] between variables. *)

val diseq : Term.t * Term.t -> t -> (t * Veqs.t)

(*s Instantiate all variables in bindings such as [f(x1,...,xn) |-> y]
 in [u_of s] with their canonical representatives, and delete all bindings 
 [l |-> _] in [v_of s] where [l] is a label term, that is, [l] satisfies
 [Term.is_label]. *)

val compress : t -> t


(*s Pretty-printing. *)

val pp : t Pretty.printer


(*s Splitting. *)

val split : t -> Atom.t list
