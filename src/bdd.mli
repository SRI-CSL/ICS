
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

(*s Module [Bdd]: Ordered binary decision diagrams with arbitrary atoms. *)

(*s This module implements ordered binary decision diagrams (BDDs). 
  All operations over BDDs  are purely applicative (no side-effects).
  The BDD operations are polynomial in the size of their arguments. *)


(*i*)
open Hashcons
(*i*)

  (*s The input signature of the functor [Bdd.Make].
     [bdd] is the type of the [low], [high], and [ite] constructors,
     which are required to satisfy the usual equalities from Boolean
     algebra. These constructors are tagged in order to allow for
     families of constructors; for example, the tag for fixed-sized bit vectors
     is the width of the bitvector to be processed.
   
     The type [bdd] is required to be hashed as can be seen from
     the definition of this type. [is_low], [is_high], and [is_ite]
     are recognizers for the respective constructors. [destructure_ite a]
     yields the arguments [(x,y,z)] if a is of the form [ite x y z],
     and [None] otherwise. [fresh t] just returns a fresh constant with tag [t]. *)

module type ITE = sig
  type bdd_node
  type bdd = bdd_node hashed
  type tag
  val compare : bdd -> bdd -> int
  val high : tag -> bdd
  val low : tag -> bdd
  val ite : tag -> bdd -> bdd -> bdd -> bdd
  val is_high : bdd -> bool
  val is_low : bdd -> bool
  val is_ite : bdd -> bool
  val destructure_ite : bdd -> (bdd * bdd * bdd) option
  val fresh : tag -> bdd
end

module Make(Ite : ITE) : sig
  open Ite

    (*s Given three BDDs [b1], [b2], and [b3], [build b1 b2 b3]
      builds a BDD which is equivalent to [ite b1 b2 b3]. That is,
      the result is either [low], [high], or an [ite] structure
      which does only contain atoms in conditional positions.
      Viewed as a directed acyclic graph,
      the resulting [ite] structure is reduced in the sense that there are
      no subgraphs of the form [ite a b b], and every subgraph occurs at
      most once. Conditionals along each part are (strictly) ordered by
      the [Term.cmp] function. Viewing atoms to be uninterpreted, the resulting
      [ite] structure is canonical in the sense that two such structures are
      equivalent (in the theory of booleans) iff they are syntactically equal.

      Notice that, unlike other BDD packages, we do not employ dynamic variable
      reordering in order to preserve canonicity of ICS terms. *)
      
  val build : tag -> bdd * bdd * bdd -> bdd

      (*s Constructors for negation [neg], conjunction [conj], disjunction [disj],
        exclusive or [xor], implication [imp], and equivalence [iff]
        are derived from [tt], [ff], and [ite].\\
        \begin{tabular}{lcl}
        [neg a] & = & [ite a ff() tt()] \\
        [conj a b] & = & [ite a b ff()] \\
         [disj a b] & = & [ite a tt() b] \\
        [xor a b] & = & [ite a (neg b) b] \\
        [imp a b] & = & [ite a b tt()] \\
        [iff a b] & = & [ite a b (neg b)]  \\
         \end{tabular} *)
           
  val neg : tag -> bdd -> bdd
  val conj : tag -> bdd -> bdd -> bdd
  val disj : tag -> bdd -> bdd -> bdd
  val xor : tag -> bdd -> bdd -> bdd
  val imp : tag -> bdd -> bdd -> bdd
  val iff : tag -> bdd -> bdd -> bdd

      (*s Recognizers such as [is_conj a] hold iff if a is of the form [ite a b low].
	in these cases the recognizer [d_conj a] yields [(a,b)].  For the
	simplifications of [ite] it is not the case that [d_conj a] holds for every
	term [a] built using [conj]. The destructors and recognizers for the other connectives
	are defined similarly. *)
      
  val is_neg : bdd -> bool          
  val is_conj : bdd -> bool
  val is_disj : bdd -> bool
  val is_xor : bdd -> bool  
  val is_imp : bdd -> bool
  val is_iff : bdd -> bool

  val d_neg : bdd -> bdd  
  val d_conj : bdd -> bdd * bdd
  val d_disj : bdd ->bdd * bdd
  val d_xor : bdd -> bdd * bdd
  val d_imp : bdd -> bdd * bdd
  val d_iff : bdd -> bdd * bdd

           
      (* Given a BDD [b]
       [solve b] either fails, in which case [b] is unsatisfiable in the given theory,
       or it returns a list of equations [\list{(x1,e1);...(xn,en)}]
       such that [xi] is an atom, all the [xi] are not equal, none of the [xi] occurs
       in any of the terms [ej], and, viewed as a conjunction of equivalences, the result
       is equivalent (in the given theory) with [b]. The terms [ei] may contain
       fresh variables.

       Solving is based on the equivalence\\
	 \begin{tabular}{lcl}
	 [ite(x,p,n)] & = & [conj (disj p n) (exists delta. x = (conj p (imp n delta)))] \\
	 \end{tabular}
       [solve1] applies this equation only once for the toplevel atom, while [solve]
       applies this equivalence repetitively for all atoms. *)

  val solve1 : tag -> bdd -> (bdd * bdd) list option     
  val solve : tag -> bdd -> (bdd * bdd) list  option        (* Solving bdds *)
 
end














