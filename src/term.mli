
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
open Bitv
(*i*)

      (*s Terms.  A term is either a variable [Var(s)], where the name [s] is a string, an
	application [App(f,l)] of a `function symbol' to a list of arguments, an update
	expression [Update(a,i,v)], or a term interpreted in one of the theories of linear
	arithmetic [Arith], propositional logic, [Prop], propositional sets [Set], tuples
	[Tuple], or bitvectors [Bv]. By definition, all entitities of type [t] are hash-consed.
	Therefore, equality tests between terms can be done in constant time using the equality
	[===] from the module [Hashcons].
      *)

type variable = string

type tag = int

type t = tnode hashed

and tnode =
  | Var of variable
  | App of t * t list
  | Update of t * t * t
  | Cond of t * t * t
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Bv of bv

      (*s Arithmetic terms are either numerals of the form [Num(q)], n-ary addition [Add(l)],
	linear multiplication [Multq(q,a)], nonlinear multiplication [Mult(l)], or division.
	Arithmetic terms built up solely from [Num], [Add], and [Multq] are considered to
	be interpreted, since [Mult] and [Div] are considered to be uninterpreted, in general.
	However, certain simplification rules for these uninterpreted function symbols are
	built-in. *)
    
and arith =
  | Num of Mpa.Q.t
  | Add of t list
  | Multq of Mpa.Q.t * t
  | Mult of t list
  | Div of t * t

      (*s Propositional terms are either [False], [True], or conditionals [Ite(a,b,c)].
	Other propositional connectives can be encoded using these constructors.  Constructors
	for universal and existential are included to allow for extensions dealing with
	first-order extensions. However, the core algorithms of ICS do not support
	first-order quantification.
      *)
     
and prop =
  | True
  | False
  | Equal of t * t  
  | Ite of t * t * t
  | Forall of variable list * t
  | Exists of variable list * t

      (*s Propositional sets are build from the empty set [Empty], the full set
	  [Full], the finite set constructor [Finite s], denoting the extension of
	  the set of terms [s], the constraint set [Cnstrnt(c)], which denotes
	  all terms satisfying the constraint [c], and the set connective [SetIte(a,b,c)],
	  which can be thought of [(a inter b) union ((compl a) inter b)], where
	  [inter] denotes intersection of two sets, [union] denotes set union, and [compl]
	  set complementation.  Set constructors are parameterized by a [tag] in order
	  to distinguish between, for example, between sets of various types.
      *)

and set =
  | Empty of tag
  | Full of tag
  | Finite of terms
  | Cnstrnt of Interval.t
  | SetIte of tag * t * t * t

      (*s A tuple term is either a tuple [Tup(l)] or the [i]-th projection [Proj(i,n,_)]
	  from an [n]-tuple.
	*)
      
and tuple =
  | Tup of t list
  | Proj of int * int * t

      (*s A fixed-sized bitvector consists of pair [(n,a)], where [a] is a term and
	[n] is the length of [a]. The bits of a bitvectors of length [n] are addressed
	by [0] through [n-1] from left-to-right.  Bitvector terms are  either constants
	[Const(b)], concatentations [Conc(l)] of fixed-sized bitvectors, extractions
	[Extr(a,i,j)] of the [i]-th through the [j]-th bit from [a], or bitwise operations
	constructed using the bitwise conditional [BvIte(a,b,c)].  In addition, [BvToNat(a)]
	represents the function for computing the unsigned interpretation of bitvectors.
      *)

and bv =
  | Const of Bitv.t
  | Conc of fixed list
  | Extr of fixed * int * int
  | BvIte of fixed * fixed * fixed
  | BvToNat of t
 
and fixed = int * t

      (*s Set of terms are implemented using Patricia trees.  Operations
	on these sets are described below in the submodule [Set]. *)

and terms = tnode Ptset.t

	      (*s A term equation [a = b] is just represented by a pair [(a,b)]. *)

type eqn = t * t

	     (*s The injection [hc] hash-conses term nodes. Thus, terms [a] and [b]
	       are considered to be equal iff they are identical.  In addition to
	       syntactical equality, sets [Finite(s1)] and [Finite(s2)] are also
	       considered to be equal whenever [s1] and [s2] contain the same elements.
	     *)

val hc : tnode -> t

    (*s Fast comparison is done in constant time, but is session-dependent,
      since it uses physical addresses. In constrast, [cmp] is session-independent
      but requires linear time.
    *)

val fast_cmp : t -> t -> int
val cmp : t -> t -> int

    (*s The terms [Bool(True)], [Bool(False)], [Arith(Num(q))], for a rational [q],
      [Set(Full(_))], [Set(Empty(_))], [Bv(Const(_))] are considered to be constant terms.
      They are all having different interpretations.
    *)

val is_const : t -> bool

    (*s Variables [Var(s)], uninterpreted function application [App _], function update [Update _],
        nonlinear multiplication [Arith(Mult _)], division [Arith(Div _)], the unsigned interpretation
        [Bv(BvToNat _)], and the set constructors [Set(Finite _)], and [Set(Cnstrnt _)] are
        considered to be uninterpreted, and all other terms are interpreted. *)

val is_uninterpreted : t -> bool

    (*s [occurs_interpreted a b] tests if term [a] occurs interpreted in [b]; in
      particular, this test fails if [a] is a subterm of an uninterpreted term. *)
    
val occurs_interpreted : t -> t -> bool

    (*s [is_ground a] holds for all terms which do not contain
        a variable term [Var(s)] or a function application [App(f,l)]. *)

val is_ground : t -> bool 
    
    (*s [cache n f] memoizes a function [f] which takes terms as arguments using
        an initial hashtable of size [n]. Similarly, [cache2] caches functions
        with [t * t] as domain, and [cachel] caches functions with a list of terms
        as domains. *)
		  
val cache : int -> (t -> 'a) -> (t -> 'a)
val cache2 : int -> (t*t -> 'a) -> (t*t -> 'a)    
val cachel : int -> (t list -> 'a) -> (t list -> 'a)

    (*s Set of terms. *)

type term = t
    
module Set : sig
	     (*s The empty set. *)
  val empty : terms
    
    (*s [mem a s] tests whether [a] belongs to the set [s]. *)
    
  val mem : t -> terms -> bool

      (*s [add a s] returns a set containing all elements of [s],
        plus [a]. If [a] was already in [s], [s] is returned unchanged. *)    
  val add : t -> terms -> terms
      
      (*s [singleton a] returns the one-element set containing only [a]. *)  
  val singleton: t -> terms
      
       (*s [sub s1 s2] tests whether the set [s1] is a subset of the set [s2]. *)
  val sub : terms -> terms -> bool
      
      (*s Test whether a set is empty or not. *)
  val is_empty : terms -> bool
      
      (*s [remove a s] returns a set containing all elements of [s],
         except [a]. If [a] was not in [s], [s] is returned unchanged. *)
  val remove : t -> terms -> terms
      
      (* Union and intersection. *)
  val union : terms -> terms -> terms
  val inter : terms -> terms -> terms
      
      (* [iter f s] applies [f] in turn to all elements of [s].
         The order in which the elements of [s] are presented to [f]
         is unspecified. *)
  val iter : (t -> unit) -> terms -> unit
  val iter2 : (t -> t -> unit) -> terms -> unit
      
      (*s [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
        where [x1 ... xN] are the elements of [s].
        The order in which elements of [s] are presented to [f] is
        unspecified. *)
  val fold : (t -> 'a -> 'a) -> terms -> 'a -> 'a
      
      (*s [map f s] constructs a set consisting of all terms [f(a)] for [a] in [s]. *)
  val map : (t -> t) -> terms -> terms
      
      (*s [exists p s] checks if at least one element of the set satisfies the predicate [p]. *)
  val exists : (t -> bool) -> terms -> bool
      
      (*s [for_all p s] checks if all elements of the set satisfy the predicate [p]. *)  
  val for_all : (t -> bool) -> terms -> bool
      
      (* [filter p s] returns the set of all elements in [s] that satisfy predicate [p]. *) 
  val filter : (t -> bool) -> terms -> terms
      
      (*s [to_list s] enumerate the elements of s in a list. The order of the elements
	  in this list is unspecified. *)
  val to_list : terms -> term list
      
      (*s Return one element of the given set, or raise exception [Not_found] if
        the set is empty. Which element is chosen is unspecified,
        but equal elements will be chosen for equal sets. *)
      
  val choose : (term -> bool) -> terms -> term
      (*s [destructure s] returns an element term [a] of a nonempty set [s] of terms
	  together with the set in which [a] is removed from [s]. The exception [Not_found]
	  is reaised if the argument set is empty. *)
      
  val destructure : terms -> term * terms
end

    (*s Finite maps with terms as domain. *)

module Map : sig
  type 'a t
    
      (*s The empty map. *)
  val empty : 'a t

      (*s Check whether the argument map is undefined everywhere. *)
  val is_empty : 'a t -> bool
      
      (*s [add x y m] returns a map containing the same bindings as
        [m], plus a binding of [x] to [y]. If [x] was already bound
        in [m], its previous binding disappears. *)
  val add : term -> 'a -> 'a t -> 'a t

      (*s [find x m] returns the current binding of [x] in [m],
        or raises [Not_found] if no such binding exists. *)
  val find : term -> 'a t -> 'a

      (*s [remove x m] returns a map containing the same bindings as
        [m], except for [x] which is unbound in the returned map. *)
  val remove : term -> 'a t -> 'a t

      (*s [mem x m] returns [true] if [m] contains a binding for [m],
        and [false] otherwise. *)
  val mem :  term -> 'a t -> bool

      (*s [iter f m] applies [f] to all bindings in map [m].
        [f] receives the key as first argument, and the associated value
        as second argument. The order in which the bindings are passed to
        [f] is unspecified. Only current bindings are presented to [f]:
        bindings hidden by more recent bindings are not passed to [f]. *)
  val iter : (term -> 'a -> unit) -> 'a t -> unit

      (*s [map f m] returns a map with same domain as [m], where the
        associated value [a] of all bindings of [m] has been
        replaced by the result of the application of [f] to [a].
        The order in which the associated values are passed to [f]
        is unspecified. *) 
  val map : ('a -> 'b) -> 'a t -> 'b t

      (*s [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
        where [k1 ... kN] are the keys of all bindings in [m],
        and [d1 ... dN] are the associated data.
        The order in which the bindings are presented to [f] is
        unspecified. *)
  val fold : (term -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

      (*s [choose p m] chooses an association [(x,y)] in [m]
	such that [p x y] holds. If the domain of [m] is empty,
	the exception [Not_found] is raised. *)
  val choose : (term -> term -> bool) -> term t -> term * term

      (*s [to_list m] enumerates the associations [(x,y)] for all [x]
	in the domain of [m] such that [y] equals [find x m]. The
	order of bindings in the result is undefined. *)
  val to_list : 'a t -> (term * 'a) list

end

  (*s Some constructors and recognizers for constants. *)

val tt : unit -> t
val ff : unit -> t
    
val is_tt : t -> bool
val is_ff : t -> bool

   (*s Constructing conditional terms. *)
    
val ite : t -> t -> t -> t

  (*s Fold operator on terms. *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

  (*s Iteration operator on terms. *)

val iter : (t -> unit) -> t -> unit

    
(*s Mapping over list of terms. Avoids unnecessary consing. *)

val mapl : (t -> t) -> t list -> t list


(*s Homomorphism [hom a op f (b1,b2,...)] on terms. [f] is applied to arguments [bi],
    if [bi] equals [f(bi)] for all [i], then the original term [a]
    is returned, otherwise a new term is constructed using [op]. *)

val hom1 : t -> (t -> t) -> (t -> t) -> t -> t
val hom2 : t -> (t * t -> t) -> (t -> t) -> t * t -> t
val hom3 : t -> (t * t * t -> t) -> (t -> t) -> t * t * t -> t
val homl : t -> (t list -> t) -> (t -> t) -> t list -> t
val homs : t -> (terms -> t) -> (t -> t) -> terms -> t    

    

