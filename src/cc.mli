
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*s Module [Cc]: Congruence closure data structure. This is an implementation
 of Shostak's version of congruence closure algorithm. It is similar to the
 one described by Cyrluk, Lincoln, and Shankar in their CADE'96 paper (Section 3). 
 For example, formulas such as
   \begin{verbatim}
   x = y & f(f(f(x))) = f(x) => f(f(f(f(f(y))))) = f(x)
   \end{verbatim}
 can be decided using this module as follows.
   \begin{verbatim}
   let s = copy(empty()) in
   let _ = process s `x = y' in
   let _ = process s `f(f(f(x))) = f(x)' in
   find s 'f(f(f(f(f(y)))))' === find s 'f(x)'
   \end{verbatim}
 [process] updates the logical context [s] to also include
 its argument equality, and builds up a congruence closure
 datastructure. In effect, [process] partitions the term
 universe of [s], i.e. the set of subterms of equalities in [s]
 into congruence-closed equivalence classes. Now, [find s a]
 returns a canonical representative for the equivalence of [a]. *)


(*s Elements of type [t] represent a logical context of equalities 
  between terms. Hereby, all terms are treated uninterpreted. *)

type t


(*s The [empty] constructor represents the empty context, and
  [copy s] returns a copy of such contexts. Since addition of 
  new equalities using [process] or [propagate] destructively update 
  a context, [copy] can be used to protect against global effects of 
  such updates. Consider, for example, the program fragment
     \begin{verbatim}
       let s' = copy s in process s' (a,b); s'
     \end{verbatim}
  Although the call to process updates [s'], the state [s] is
  left unchanged. Copying is a cheap operation (constant space), 
  since only a shallow copy is made. *)
  
val empty : unit -> t
val copy : t -> t

(*s [subst_of s] returns a substitution, which represents a
  set of equalities equivalent to the context [s]. A binding
  [a |-> b] of this substitution represents an equality [a = b].
  For every such binding, it holds that [a] is lexicographically
  larger than [b] in the sense that e.g. [f(f(f(x))) |-> f(x)],
  [f(x) -> y], or [y |-> x]. *)

val subst_of : t -> Subst.t

(*s [use s a] returns the set of superterms of [a] in the term universe. *)

val use_of : t -> Term.ts Term.Map.t

(*s [cnstrnt_of s] associates with each canonical form a constraint. *)

val cnstrnt_of : t -> Cnstrnt.t Term.Map.t


(*s [diseqs_of s] returns the set of known disequalities for each canonical 
 term [a]; i.e. [a] is known to be disequal from any of these terms. The
 disequality terms are not necessarily in canonical form. *)

val diseqs_of : t -> Term.ts Term.Map.t

(*s The term universe of a state [s] is the set of subterms of
  the equalities in the context [s]. [universe s a] tests if
  [a] is in the term universe of [s]. Notice that the term universe
  is not precomputed, but depends on the set of processed equations in [s]. *)

val universe : t -> Term.t -> bool

(*s [find s a] returns the canonical representative of the
  equivalence class of [a] as determined by the context [s]. 
  For any terms [a] and [b] in the term universe of [s], it 
  holds that [find s a === find s b] iff the equality [a = b]
  is valid in context [s]. Since the term equality [===] is 
  just address comparison, validity of equations is checked
  in constant time. In addition, [find] is idempotent,
  i.e. [find s (find s a) === find s a], and [find s (f(a1,...,an))]        
  equals [find s f(find s a1,...,find s an)]. *)

val find : t -> Term.t -> Term.t

(*s [use s a] returns the set of superterms of [a] in the 
 term universe of s *)

val use : t -> Term.t -> Term.ts

(*s For a term [a = f(a1,...,an)], [sgn s a] returns 
  [f(find s a1,...,find s an)]. Thus, by definition,
  [sgn s a === sgn s b] implies that [a] and [b] are congruent. *)

val sgn : t -> Term.t -> Term.t

(*s [is_diseq s (a,b)] tests if terms [a] and [b] are known to be disequal in state [s]. *)

val is_diseq : t -> Term.t * Term.t -> bool

(*s [cnstrnt s a] returns the constraint for the equivalence class
 of [a]. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t

(*s [diseqs s a] returns the disequality set of a term [a]. The
 terms in this set are not necessarily in canonical form. *)

val diseqs : t -> Term.t -> Term.ts

(*s [extend s a] extends the domain of [s] with [a], if not yet in domain. *)

val extend : t -> Term.t -> unit

(*s [add_eqn s (a,b)] adds a new equality to the context [s] by
 destructively updating [s]. It returns a list of all 
 infered equalities [a = b] in the term universe of the updated context 
 [s'] such that [a = b] does not hold in [s] but in [s']. 
 Processing a new equation is quadratic in the size of the term universe. *)

val add_eqn : t -> Eqn.t -> Eqn.t list

(*s [add_eqns] just iterates [add_eqn] for a list of equalities. *)

val add_eqns : t -> Eqn.t list -> Eqn.t list

(*s [add_cnstrnt s (a,c)] updates the constraint associated with the
 equivalence class of [a] with the intersection of this constraint and [c]. 
 If the intersection of these constraints denotes the empty set, then
 [Exc.Inconsistent] is raised. *)

val add_cnstrnt : t -> Cnstrnt.t * Term.t -> Eqn.t list

val add_cnstrnts : t -> (Cnstrnt.t * Term.t) list -> Eqn.t list


(*s [add_diseq s (a,b)] updates the set of known disequalities of [a] by
 adding [b], and, vice versa, the known disequalities of [b] are extended with [a]. 
 [add_diseqs] just iterates [add_diseq] on a list of disequalities.  *)

val add_diseq : t -> Term.t * Term.t -> unit

val add_diseqs : t -> (Term.t * Term.t) list -> unit

(*s [ext s a] is the set of variable terms known to be equivalent with [a]. *)

val ext : t -> Term.t -> Term.ts
