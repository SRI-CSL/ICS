
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
 *
 * Author: Harald Ruess
 i*)

(*s The module [Tuple] implements constructors for tuples and 
  projections, normalization of tuple terms, a solver for the 
  theory of tuples, and an abstract datatype for logical
  contexts of equalities over tuples. *)

(*s [is_tuple a] holds iff [a] is a projection or a tuple term.
  Terms for which [is_tuple] is false are considered to be uninterpreted. *)

val is_tuple : Term.t -> bool


(*s [iter f a] applies [f] to all top-level uninterpreted subterms of [a]. *)

val iter: (Term.t -> unit) -> Term.t -> unit 


(*s If the argument list [l] is of length [1], then
  this term is returned. Otherwise, [tuple l] constructs the 
  corresponding tuple term. *)

val tuple : Term.t list -> Term.t

   
(*s [proj i n a] is the constructor for the family of [i]-th projections
  from [n]-tuples, where [i] is any integer value between [0] and [n-1].
  This constructor simplifies [proj i (tuple \list{a_0;...;a_n-1})] to [a_i]. *)
 
val proj : int -> int -> Term.t -> Term.t


(*s [sigma op l] applies the function symbol [op] from the tuple theory to
  the list [l] of terms. For the function symbol [Proj(i,n)] and the list [a],
  it simply applies the constructor [proj i n a], and for [Tuple] and it 
  applies [tuple l]. All other inputs result in a run-time error. *)

val sigma : Term.tuple -> Term.t list -> Term.t
  

(*s Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in sigmatized.  Thus, [norm] can be thought of as the the 
  composition of applying substitution [s] to [a] followed by sigmatizing each 
  interpreted subterm by the function [sigma] above. *)

val norm: Subst.t -> Term.t -> Term.t


(*s [solve (a,b)] returns a solved form for the equation [a = b]. 
  If this equation is inconsistent, then the exception [Exc.Inconsistent]
  is raised.  Otherwise, the solved form [(x1,e1),...,(xn,en)] is returned,
  where [xi] are uninterpreted in the tuple theory and the [ei] are sigmatized.
  The [ei] may also contain fresh variables.  The solver for equations over tuples 
  and projections works by repetitively applying
      \begin{tabular}{lcl}
      [(tuple \list{$a_0$,\ldots,$a_n$},~tuple \list{$a_0$,\ldots,$a_n$})]
          & = & [\list{($a_0$,$b_0$),\ldots,($a_n$,$b_n$)}] \\
      [(proj i n a, b)] & = & [(a, tuple \list{$c_0$,\ldots,c_{i-1},b,...c_{n-1}})]
      \end{tabular}
      where [$c_i$] are fresh, and [b] at [i]-th position. *)

val solve : Eqn.t -> Eqn.t list
 

(*s The type [t] comprises a set of tuple equalities of the form [x = a],
  where [x] is an uninterpreted term that is not a fresh variable, and [a]
  is a tuple term. [empty] represents the empty context, an equalities over
  tuple terms are added to an arithmetic context by [extend] or [process], 
  and uninterpreted equalities are propagated  by [propagate].  Since [extend], [process], 
  and [propagate] destructively update the argument constraints, [copy] must be used 
  before calling these functions in order to save a certain state.  [Copying] is 
  inexpensive, since only a shallow copy is created. *)


type t

val empty: unit -> t
val copy: t -> t


(*s [subst_of s] returns a substitution with bindings of the form [x |-> a],
 where [x] is a power product (but not a fresh variable), and [a] is a
 tupleterm or a fresh variable.  Substitution represents an
 tuple context by interpreting bindings as equalities [x = a]. *)

val subst_of : t -> Subst.t


(* [use_of s a] returns the set of right-hand side terms of s, in which
 [a] occurs as a subterm. *)

val use_of : t -> Term.ts Term.Map.t


(*s [apply s a] returns [b] if there is a binding [a |-> b] in [s], and raises
 [Not_found] otherwise. [find s a] is like [apply] but returns [a] if there
 is no binding with domain [a]. [inv s b] returns [a] if there is a binding
 [a |-> b], and [b] otherwise. [use s a] returns the set of rhs in [s] which
 contain [a] as a subterm. *)


val apply: t -> Term.t -> Term.t
val find: t -> Term.t -> Term.t
val inv : t -> Term.t -> Term.t
val use : t -> Term.t -> Term.ts



(*s [extend s (a,b)] installs a new binding [a |-> b] into [s]. 
 It assumes that [a] is not yet in the domain of [s]. 
 Also, [a],[b] must be valid lhs and rhs, respectively. *)

val extend : t -> Eqn.t -> unit

(*s [process ctxt s (a,b)] installs an equality [a = b], where at least one of [a],[b] is
 a tuple term, into the tuple context [s]. Abstractly, the manipulations on [s] can be 
 described by [s o solve(norm s a, norm s b)], where the composition [o] operator 
 includes new bindings [x |-> e] for each equality in the solved form 
 [solve(norm s a, norm s b)], and all rhs of [s] are normalized, using [norm] above, 
 with respect to the this solved form.  In addition, if the rhs of a newly introduced 
 binding reduces to an uninterpreted term (excluding fresh variables), then the 
 corresponding binding is removed and returned as a newly infered equality between 
 uninterpreted terms. *)


val process: t -> Eqn.t -> Eqn.t list



(*s [propagate ctxt s (a,b)] works similar to [process] above, but it 
 assumes that both [a] and [b] are uninterpreted (and not fresh either). *)

val propagate: t -> Eqn.t list -> Eqn.t list

