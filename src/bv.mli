
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

(*s Module [Bv]: Constructors, recognizers, and accessors for
 bitvector terms, and a solver for equations over bitvectors.
 Bitvectors of width [n], where [n] is a natural
 number, are arrays of bits with positions [0] through [n-1] 
 in increasing order from left-to-right. *)


(*s [is_interp a] holds iff the top-level function symbol of [a]
 is interpreted in the theory of bitvectors (see also module [Sym]. *)

val is_interp : Term.t -> bool


(*s Computes the width of a bitvector term. *)

val width : Term.t -> int option


(*s [iter f a] applies [f] for all toplevel subterms of [a] 
 which are not interpreted in the theory of bitvectors. *)

val iter : (Term.t -> unit) -> Term.t -> unit
  

(*s [mk_const c] is the constructor for building constant bitvectors,
  in which all bits are known to be either [0] or [1]. 
  [mk_zero n] is just defined to be the constant zero bitvector of length [n],
  [mk_one n] is the constant one bitvector of length [n], and
  [mk_eps] is the constant bitvector of length [0]. *)
    
val mk_eps : Term.t
val mk_one : int -> Term.t
val mk_zero : int -> Term.t
val mk_const : Bitv.t -> Term.t
    

(*s [is_zero a] holds iff all  bits in [a] are [0]. *)

val is_zero : Term.t -> bool

(*s [is_one a] holds iff all bits in [a] are [1]. *)

val is_one : Term.t -> bool  

(*s [mk_conc n m b1 b2] concatenates a bitvector [b1] of width [n]
  and a bitvector [b2] of width [m]. 
  [mk_conc] terms are built up in a right-associative way, argument
  bitvectors of length [0] are ignored, constant argument
  bitvectors are combined into the corresponding concatenated
  constant bitvector, and concatentations of extractions such as
  [mk_sub x i j] and [mk_sub x (j+1) k] are merged to [mk_sub x i k]. *)
    
val mk_conc : int -> int -> Term.t -> Term.t -> Term.t


(*s [mk_sub n i j x] returns the representation of the bitvector
 for the contiguous extraction of the [j-i+1] bits [i] through [j]
 in the bitvector [x] of length [n]. It is assumed that
 [0 <= i,j < n].  If [j < i] then the empty bitvector [mk_eps]
 is returned, and if [i = j] then the result is a bitvector of 
 width [1].  Simplifications include extraction of bitvector
 terms, [mk_sub] distributes over concatenation and bitwise
 operators, and successive extractions are merged. *)

val mk_sub : int -> int -> int -> Term.t -> Term.t


(*s [mk_bitwise n a b c] builds bitvector BDDs of 
 width [n]. Bitvector BDDs are a canonical form for bitwise 
 conditionals of width [n], and they are
 similar to a binary decision diagram (BDD) in that
 [mk_bitwise n a b c] reduces to [c] when [is_zero a]
 holds, it reduces to [b] when [is_one a] holds, and
 to [b] if [b] is syntactically equal to [c].  Furthermore
 the conditional structure is reduced in that there are
 no identical substructures, and the conditional parts
 are ordered from top to bottom (this ordering is
 fixed but unspecified). Since bitwise operations
 distribute over concatenation and extraction, all
 terms in a bitvector BDD are either bitvector constants,
 uninterpreted terms, or a (single) extraction from an
 uninterpreted term. *)
 
val mk_bitwise : int -> Term.t -> Term.t -> Term.t -> Term.t


(*s Derived bitwise operators for bitwise conjunction ([mk_bwconj]), 
 disjunction ([mk_bwconj]), negation ([mk_bwconj]), implication ([mk_bwconj]), 
 and equivalence ([mk_bwconj]). *)

val mk_bwconj : int -> Term.t -> Term.t -> Term.t
val mk_bwdisj : int -> Term.t -> Term.t -> Term.t
val mk_bwneg : int -> Term.t -> Term.t
val mk_bwimp : int -> Term.t -> Term.t -> Term.t
val mk_bwiff : int -> Term.t -> Term.t -> Term.t

(*s Given a bitvector symbol [f] (see module [Sym]) and a list
 [l] of arguments, [sigma f l] returns a concatenation normal form (CNF)
 of the application ['f(l)'].  A CNF is either a simple bitvector or a 
 right-associative concatenation of simple bitvectors, and a simple
 bitvector is either an uninterpreted term, a bitvector constant,
 an extraction, or a bitvector BDD (see above).  All the
 simplifications for [mk_sub], [mk_conc], [mk_bitwise] are applied. *)

val sigma : Sym.bv -> Term.t list -> Term.t


(*s [norm f a] applies [f] to all top-level uninterpreted
  subterms of [a], and rebuilds the interpreted parts in order.
  It can be thought of as replacing every toplevel uninterpreted
  [a] with ['f(a)'] if [Not_found] is not raised by applying [a],
  and with [a] otherwise, followed by a sigmatization
  of all interpreted parts using [mk_sigma]. *)

val norm: (Term.t -> Term.t) -> Term.t -> Term.t
   
(*s [solve b] either fails, in which case [b] is unsatisfiable in the 
  given bitvector theory or it returns a list of equations 
  [\list{(x1,e1);...(xn,en)}] such that [xi] is a non bitvector term, 
  all the [xi] are pairwise disjoint, none of the [xi] occurs in any of 
  the terms [ej], and, viewed as a conjunction of equivalences, the result
  is equivalent (in the theory of bitvectors) with [b]. The terms [ei] 
  may contain fresh bitvector constants. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list


(*s [is_fresh a] checks if term [a] has been generated by the 
 bitvector solver. *)

val is_fresh : Term.t -> bool





