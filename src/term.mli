
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

(*s Terms are the basic data structures of ICS. *)

(*i*)
open Hashcons
open Mpa
(*i*)

(*s Terms.  A term is either a variable [Var(s)], where the name [s] is a string, an
 application [App(f,l)] of a `function symbol' to a list of arguments, an update
 expression [Update(a,i,v)], or a term interpreted in one of the theories of linear
 arithmetic [Arith], propositional logic, [Prop], propositional sets [Set], tuples
 [Tuple], or bitvectors [Bv]. By definition, all entitities of type [t] are hash-consed.
 Therefore, equality tests between terms can be done in constant time using the equality
 [===] from the module [Hashcons].

 Arithmetic terms are either numerals of the form [Num(q)], n-ary addition [Add(l)],
 linear multiplication [Multq(q,a)], nonlinear multiplication [Mult(l)], or division.
 Arithmetic terms built up solely from [Num], [Add], and [Multq] are considered to
 be interpreted, since [Mult] and [Div] are considered to be uninterpreted, in general.
 However, certain simplification rules for these uninterpreted function symbols are
 built-in.

 Propositional terms are either [False], [True], or conditionals [Ite(a,b,c)].
 Other propositional connectives can be encoded using these constructor.

 A tuple term is either a tuple [Tup(l)] or the [i]-th projection [Proj(i,n,_)]
 from an [n]-tuple.
	
 Set of terms are implemented using Patricia trees.  Operations
 on these sets are described below in the submodule [Set]. *)


type term = 
  | App of Sym.t * t list

and t = term hashed

and set = term Ptset.t

and 'a map = (term,'a) Ptmap.t


(*s Constructing and destructing terms *)

val make : Sym.t * t list -> t

val destruct : t -> Sym.t * t list

val sym_of : t -> Sym.t
val args_of : t -> t list

(*s Equality of terms. *)

val eq : t -> t -> bool

(*s Fast comparison is done in constant time, but is session-dependent,
  since it uses physical addresses. In constrast, [cmp] 
  is session-independent but requires linear time. *)

val fast_cmp : t -> t -> int

val cmp : t -> t -> int

val (<<<): t -> t -> bool

val order : t -> t -> t * t


(*s Test if term is a constant. *)

val is_const : t -> bool

val is_interp_const : t -> bool
 
(*s Test if [a] and [b] are known to be disequal. *)

val is_diseq : t  -> t -> bool

(*s [is_suberm a b] tests if [a] occurs in [b], interpreted or not. *)

val is_subterm : t -> t -> bool

(*s Fold operator on ts. *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a


(*s Iteration operator on terms. *)

val iter : (t -> unit) -> t -> unit

(*s Predicate holds for all subterms. *)

val for_all : (t -> bool) -> t -> bool

    
(*s Mapping over list of terms. Avoids unnecessary consing. *)

val mapl : (t -> t) -> t list -> t list

(*s Homomorphism [hom a op f (b1,b2,...)] on terms. 
 [f] is applied to arguments [bi], if [bi] equals [f(bi)] 
 for all [i], then the original term [a] is returned, otherwise 
 a new term is constructed using [op]. *)

val hom1 : t -> (t -> t) -> (t -> t) -> t -> t
val hom2 : t -> (t * t -> t) -> (t -> t) -> t * t -> t
val hom3 : t -> (t * t * t -> t) -> (t -> t) -> t * t * t -> t
val homl : t -> (t list -> t) -> (t -> t) -> t list -> t


(*s Association lists for terms. *)

val assq : t -> (t * 'a) list -> 'a

(*s Get theory of top-level function symbol. *)

val theory_of : t -> Sym.classify

(*s Printer. *)

val pp : Format.formatter -> t -> unit

val ppeqn : Format.formatter -> t * t -> unit
