
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

(*s The module [Bool] implements constructors, recognizers, and destructors
  for for propositional connectives and for first-order quantifications. In
  addition, there is a solver for equalities over propositional connectives. *)

val is_bool : Term.t -> bool

    (*s The true [tt()] and false [ff()] constants. [is_tt a]
     tests if the argument [a] is the true constant; similarly for [is_ff]. *)

val tt : unit -> Term.t
val ff : unit -> Term.t
   
val is_tt : Term.t -> bool
val is_ff : Term.t -> bool

    
    (*s Constructor for equalities with simplifications
          \begin{tabular}{lcll}
          [equal a a] & = & [tt()] & \\
          [equal c d] & = & [ff()] & for different constants [c], [d]. \\
          [equal a b] & = & [iff a b] & if one of [a],[b] is an [ite] structure.
          \end{tabular}
      [iff] is the constructor for propositional equivalence below.
      [is_equal a] holds iff the node of [a] is of the form [Equal(x,y)];
      for these terms [d_equal a] is defined and returns the pair [(x,y)]
      consisting of the lhs and the rhs. *)   

val equal : Term.t * Term.t -> Term.t
val is_equal : Term.t -> bool
val d_equal : Term.t -> Term.t * Term.t


    (*s The [diseq a b] reduces to [neg(equal a b)], where [neg] is
      the constructor for boolean negation. The recognizer [is_diseq a]
      holds iff the node of [a] is of the form [Ite(Equal(x,y),ff(),tt())];
      in these cases, the destructor [d_diseq a] returns [(x,y)]. *)

val diseq : Term.t -> Term.t -> Term.t
val is_diseq : Term.t -> bool
val d_diseq : Term.t -> Term.t * Term.t  

    
    (*s Representations of propositional connectives are built using
      the [ite a b c] constructor. This constructor, together with [tt()]
      and [ff()] builds up ordered binary decision diagrams with nonboolean
      terms (that is, terms not of the form [tt()], [ff()], or [Ite(_)])
      in the conditional position.  Viewed as a directed acyclic graph,
      the resulting [Ite] structures are reduced in the sense that there are
      no subgraphs of the form [Ite(a,b,b)], and every subgraph occurs at
      most once. Conditionals along each part are (strictly) ordered by
      the [Term.cmp] function.  When conditionals are considered to be
      variables or uninterpreted terms, the resulting [Ite] structure is
      canonical in the sense that two such structures are equivalent (in the
      theory of booleans) iff they are syntactically equal.

      In addition, the [ite] constructor performs a number of semantic
      simplifications based on the fact that [ite a b c] is equivalent with
      [disj (conj a b) (conj (neg a) b)].  First, one tries to simplify [conj a b]
      and [conj (neg a) b]. For example, [conj (lt x 2) (le x 3)] is reduced to
      [lt x 2]. If both simplifications are successful, then a simplified disjunction
      is built. For example, [disj (equal x 2) (diseq 2 x)] is reduced to [tt()].
      For simplification, the obvious facts about equality, disequality, a
      and constraints are used.

      The recognizer [is_ite a] holds iff a is of the form [Ite(x,y,z)]; in these cases,
      the destructor [d_ite a] returns the triple [(x,y,z)].
    *)
      
val ite : Term.t * Term.t * Term.t -> Term.t
val is_ite : Term.t -> bool
val d_ite : Term.t -> Term.t * Term.t * Term.t

val cond : Term.t * Term.t * Term.t -> Term.t

    (*s Constructors for negation [neg], conjunction [conj], disjunction [disj],
        exclusive or [xor], implication [imp], and equivalence [iff]
        are derived from [tt], [ff], and [ite].
         \begin{tabular}{lcl}
            [neg a] & = & [ite a ff() tt()] \\
         [conj a b] & = & [ite a b ff()] \\
         [disj a b] & = & [ite a tt() b] \\
          [xor a b] & = & [ite a (neg b) b] \\
          [imp a b] & = & [ite a b tt()] \\
          [iff a b] & = & [ite a b (neg b)] 
         \end{tabular}
      Destructors such as [d_conj a] hold iff if a is of the form [Ite(a,b,ff())]
      of [conj]; in these cases the recognizer [d_conj a] yields [(a,b)].  For the
      simplifications of [ite] it is not the case that [d_conj a] holds for every
      term [a] built using [conj]. The destructors and recognizers for the other connectives
      are defined similarly.

      Notice that for all propositional terms exactly one of [is_tt()], [is_ff()],
      or [is_ite()] holds. In particular, [is_ite a] also holds for terms for which
      [is_conj a] holds, since conjunction is just a derived notion.

      The constructors [conjl] and [disjl] are just shorthand for iterative binary
      conjunction and disjunction, respectively.
      *)
    
val neg : Term.t -> Term.t
val conj : Term.t -> Term.t -> Term.t
val disj : Term.t -> Term.t -> Term.t
val xor : Term.t -> Term.t -> Term.t
val imp : Term.t -> Term.t -> Term.t
val iff : Term.t -> Term.t -> Term.t

val is_neg : Term.t -> bool
val is_conj : Term.t -> bool
val is_disj : Term.t -> bool
val is_xor : Term.t -> bool
val is_imp : Term.t -> bool
val is_iff : Term.t -> bool


val d_neg : Term.t -> Term.t
val d_conj : Term.t -> Term.t * Term.t
val d_disj : Term.t -> Term.t * Term.t
val d_xor : Term.t -> Term.t * Term.t
val d_imp : Term.t -> Term.t * Term.t
val d_iff : Term.t -> Term.t * Term.t


val conjl : Term.t list -> Term.t
val disjl : Term.t list -> Term.t

    
    (*s Constructors for universal and existential quantification.
      Currently, no simplifications are perfored. *)

val forall : Term.variable list -> Term.t -> Term.t
val exists : Term.variable list -> Term.t -> Term.t

    
    (* Given an equation [(a,b)], where at least one of [a] and [b] is an [Ite]-structure,
       [solve (a,b)] either fails, in which case [a = b] is unsatisfiable in the theory of
       booleans, or it returns a list of equations [\list{(x1,e1);...(xn,en)}]
       such that [xi] is not a boolean term, all the [xi] are not equal, none of the [xi] occurs
       in any of the terms [ej], and, viewed as a conjunction, the result is equivalent
       (in the theory of booleans) with the equation to be solved. The terms [ei] may contain
       fresh variables. *)
       
val solve : Term.t -> Term.eqn list option

    (*s Lifting conditionals. *)

val unary_lift_ite : (Term.t -> Term.t) -> Term.t -> Term.t
val binary_lift_ite : (Term.t * Term.t -> Term.t) -> Term.t * Term.t -> Term.t
val ternary_lift_ite : (Term.t * Term.t * Term.t -> Term.t) -> Term.t * Term.t * Term.t -> Term.t
val nary_lift_ite : (Term.t list -> Term.t) -> Term.t list -> Term.t
    











