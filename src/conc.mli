
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

(*s Module [Conc]: Constructors for building up concatenation normal
  forms of bitvector and for solving equations over such normal forms. *)
 

(*s The input signature of the functor [Conc.Make].
  [t] is the type of variables, which are required, as indicated
  by its definition, to be hash-consed. [fresh] is supposed to
  generate fresh variables. *)

module type Var = sig
  type tnode
  type t = tnode Hashcons.hashed
  val fresh : unit -> t
end

module Make(X : Var) : sig

   (*s 
     Fixed-sized bitvectors have a nonnegative length [n].
     Bits are either low [zero] or high [one], and the
     positions of individual bits of a bitvector of length [n]
     are numbered [0] through [n-1] in a left-to-right reading.

     Concatenation normal forms are representations for bitvectors
     of the form [b1 ++ ... ++ bn] where [++] denotes
     concatenation and the [bi] are basic bitvectors.
     Basic bitvectors are
     \begin{enumerate}
     \item constant bitvectors [const(c)],
     \item extractions [sub((x,n),i,j)] of the [i]-th through the [j]-th bit
     of a term [x], when [0 <= i <= j < n] or
     \item bitwise operations [ite(b1,b2,b3)], where [b1] is either a
           constant with not all bits zeros or ones, or an extraction;
     [b2] and [b3] are arbitrary basic bitvectors.
     \end{enumerate} *)

  type t

    
    (*s Homomorphism on concatenation normal forms. For basic bitvectors,
      this homomorphism is defined as\\
      \begin{tabular}{lcl}
      [hom fc fe fbw const(c)] & = & [fc c] \\
      [hom fc fe fbw sub(x,n,i,j)] & = & [fe x n i j] \\
      [hom fc fe fbw ite(n,x,y,z)] & = & [fbw n x y z] \\
      \end{tabular}
      and is homomorphically extended to concatenations. *)

    
  val hom : (Bitv.t -> 'a)
               -> (X.t -> int -> int -> int -> 'a)
                  -> (int -> 'a -> 'a -> 'a -> 'a) -> t -> 'a list

		      
    (*s The length of a basic bitvector is just the length of the constant,
      the length of an extraction [sub(_,_,i,j)] is [j-i+1], and the length
      of a bitwise operation [ite(x,y,z)] is the length of, say, [x] and is
      only defined if the lengths of the arguments are all equal. *)
		      
  val length : t -> int


     (*s [const c] is the constructor for building constant bitvectors.
       [zero n] is just defined to be the constant zero bitvector of length [n],
       [one n] is the constant one bitvector of length [n], and [eps] is the
       constant bitvector of length [0]. *)
	
  val eps : t
  val zero : int -> t
  val one : int -> t
  val const : Bitv.t -> t


     (*s [sub b i j] is the extraction of the [i]-th through the [j]-th bit.
      This operation is only defined if [0 <= i <= j < n], where [n] is the
      length of [b]. The resulting bitvector is of length [j - i + 1]. *)

  val sub : t -> int -> int -> t

     (*s [inj n x] injects a variable [x] into a bitvector of length [n]. *)
  
  val inj : int -> X.t -> t

     (*s [b1 ++ b2] concatenates two bitvectors. The width of the
       resulting bitvector is the sum of the width of [b1] and [b2].
       [++] are built up in a right-associative way, argument
       bitvectors of length [0] are ignored, constant argument
       bitvectors are combined into the corresponding concatenated
       constant bitvector, and concatentations of extractions such
       as [sub x i j ++ sub x (j+1) k] are simplified to [sub x i k].
       [conc] iterates the binary concatenation operator on a list of
       bitvector arguments. *)
    
  val (++) : t -> t -> t 
  val conc : t list -> t

     (*s Constructor [ite b1 b2 b3] for bitwise operations on bitvectors.
       The result is equivalent, in the theory of bitvectors, with
       [bw_disj (bw_conj b1 b2) (bw_conj (bw_neg b1) b3)].  Moreover,
       [ite] builds canonical terms by building up binary decision
       diagram-like data structures (see module [Bdd]) with only basic
       bitvectors as arguments, since concatenation distributes over
       the bitwise operation [ite]. *)
  
  val ite : t -> t -> t -> t

      
     (*s Derived constructors, recognizers, and destructors for bitwise
       negation, conjunction, disjunction, exclusive or, implication,
       and equivalence . *)

  val bw_neg : t -> t
  val bw_conj :  t -> t -> t
  val bw_disj : t -> t -> t
  val bw_xor :  t -> t -> t
  val bw_imp : t -> t -> t
  val bw_iff : t -> t -> t

  val is_bw_neg : t -> bool
  val is_bw_conj : t -> bool
  val is_bw_disj : t -> bool
  val is_bw_xor : t -> bool
  val is_bw_imp : t -> bool
  val is_bw_iff : t -> bool

  val d_bw_neg : t -> t
  val d_bw_conj : t -> (t * t)
  val d_bw_disj : t -> (t * t)
  val d_bw_xor : t -> (t * t)
  val d_bw_imp : t -> (t * t)
  val d_bw_iff : t -> (t * t)

    (*s  [solve b] either fails, in which case [b] is unsatisfiable in the given
      theory, or it returns a list of equations [\list{(x1,e1);...(xn,en)}]
      such that [xi] is a non bitvector term, all the [xi] are pairwise disjoint,
      none of the [xi] occurs in any of the terms [ej], and, viewed as a conjunction of
      equivalences, the result is equivalent (in the given theory) with [b]. The terms
      [ei] may contain fresh variables.

      Solving is based on the equivalence\\
      \begin{tabular}{lcl}
      [ite(x,p,n)] & = & [bw_conj (bw_disj p n) (exists delta. x = (bw_conj p (bw_imp n delta)))] \\
      \end{tabular} *)
  

  val solve : t -> t -> (X.t * t) list

end







