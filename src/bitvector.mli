(*
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
 *)

(** Theory of bitvectors.

  @author Harald Ruess

  Bitvectors of width [n], where [n] is a natural
  number, are arrays of bits with positions [0] through [n-1] 
  in increasing order from left-to-right.

  Bitvector terms are built up from bitvector constants, concatenation
  of two bitvectors, extraction of a contiguous subrange from a bitvector,
  and logical bitwise operations. 
*)


val bitv2nat : Bitv.t -> int
  (** Unsigned interpretation of a bitvector constant. *)

val nat2bitv : int -> int -> Bitv.t
  (** [nat2bitv n i] returns the a bitvector of length [n]
    representing the unsigned interpretation of the nonnegative 
    integer [i]. It is assumed that [n] is large enough for 
    representing [i]. *)
    

(** {6 Width of a bitvector} *)

val width : Term.t -> int option
  (** Computes the width of a bitvector term, and returns [None] if
    the argument is not an application of a bitvector symbol *)


(** {6 Destructors} *)

val d_const : Term.t -> Bitv.t
  (** Accessor for bitvector constants. *)


(** {6 Constructors} *)

val mk_const : Bitv.t -> Term.t
  (** [mk_const c] is the constructor for building constant bitvectors,
    in which all bits are known to be either [0] or [1]. *)
    
val mk_eps : Term.t
 (** [mk_eps] is the constant bitvector of length [0]. *)

val mk_one : int -> Term.t
  (** [mk_one n] is the constant one bitvector of length [n] *)

val mk_zero : int -> Term.t
  (** [mk_zero n] is just defined to be the constant zero bitvector of length [n] *)

val mk_conc : int -> int -> Term.t -> Term.t -> Term.t
  (** [mk_conc n m b1 b2] concatenates a bitvector [b1] of width [n]
    and a bitvector [b2] of width [m]. 
    [mk_conc] terms are built up in a right-associative way, argument
    bitvectors of length [0] are ignored, constant argument
    bitvectors are combined into the corresponding concatenated
    constant bitvector, and concatentations of extractions such as
    [mk_sub x i j] and [mk_sub x (j+1) k] are merged to [mk_sub x i k]. *)

val mk_sub : int -> int -> int -> Term.t -> Term.t
  (** [mk_sub n i j x] returns the representation of the bitvector
    for the contiguous extraction of the [j-i+1] bits [i] through [j]
    in the bitvector [x] of length [n]. It is assumed that
    [0 <= i,j < n].  If [j < i] then the empty bitvector [mk_eps]
    is returned, and if [i = j] then the result is a bitvector of 
    width [1].  Simplifications include extraction of bitvector
    terms, [mk_sub] distributes over concatenation and bitwise
    operators, and successive extractions are merged. *)

(** {6 Recognizers} *)

val is_interp : Term.t -> bool
  (** [is_interp a] holds iff the top-level function symbol of [a]
    is interpreted in the theory of bitvectors (see also module [Sym]. *)

val is_const : Bitv.t -> Term.t -> bool
  (** [is_const a] holds iff [a] is a bitvector constant. *)

val is_zero : Term.t -> bool
  (** [is_zero a] holds iff all  bits in [a] are [0]. *)

val is_one : Term.t -> bool  
  (** [is_one a] holds iff all bits in [a] are [1]. *)

    

(** {6 Canonizer} *)

val sigma : Sym.bv -> Term.t list -> Term.t
  (** Given a bitvector symbol [f] (see module [Sym]) and a list
    [l] of arguments, [sigma f l] returns a concatenation normal form (CNF)
    of the application ['f(l)'].  A CNF is either a simple bitvector or a 
    right-associative concatenation of simple bitvectors, and a simple
    bitvector is either an uninterpreted term, a bitvector constant,
    an extraction, or a bitvector BDD (see above).  All the
    simplifications for [mk_sub], [mk_conc], [mk_bitwise] are applied. *)


(** {6 Iterators} *)

val iter : (Term.t -> unit) -> Term.t -> unit
  (** [iter f a] applies [f] for all toplevel subterms of [a] 
    which are not interpreted in the theory of bitvectors. *)
  
val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** [fold f a e] applies [f] at uninterpreted positions of [a] and 
    accumulates the results starting with [e]. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] applies [f] to all top-level uninterpreted
    subterms of [a], and rebuilds the interpreted parts in order.
    It can be thought of as replacing every toplevel uninterpreted
    [a] with ['f(a)'] if [Not_found] is not raised by applying [a],
    and with [a] otherwise, followed by a sigmatization
    of all interpreted parts using [mk_sigma]. *)

val apply : Term.Equal.t -> Term.t -> Term.t


(** {6 Solver} *)

val solve : Term.t * Term.t ->  (Term.t * Term.t) list
  (** [solve b] either fails, in which case [b] is unsatisfiable in the 
    given bitvector theory or it returns a list of equations 
    [[(x1,e1);...(xn,en)]] such that [xi] is a non bitvector term, 
    all the [xi] are pairwise disjoint, none of the [xi] occurs in any of 
    the terms [ej], and, viewed as a conjunction of equivalences, the result
    is equivalent (in the theory of bitvectors) with [b]. The terms [ei] 
    may contain fresh bitvector constants. *)


