
(*i*)
open Mpa
open Hashcons
open Bitv
(*i*)

(*s Types of terms.  *)

type cnstrnt = Int | Real | Pos | Neg | Nonneg | Nonpos

type variable = string

type tag = int

type term =
    term_node hashed

and term_node =
  | Var of variable
  | App of term * term list
  | Update of term * term * term
  | Equal of term * term
  | Cnstrnt of cnstrnt * term
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Bv of bv
      
and arith =
  | Num of Q.t
  | Times of term list
  | Plus of term list
    
and prop =
  | True
  | False
  | Ite of term * term * term
  | Forall of variable list * term
  | Exists of variable list * term

and set =
  | Empty of tag
  | Full of tag
  | SetIte of tag * term * term * term
      
and tuple =
  | Tup of term list
  | Proj of int * int * term

and bv =
  | Const of Bitv.t
  | Conc of fixed list
  | Extr of fixed * int * int
  | BvIte of fixed * fixed * fixed

and fixed = int * term
 
(*s Constructors. The constructors of the above types should not be used
    directly, and the following functions have to be used instead.
    There are two reasons: first, the following functions perform hash-consing,
    sharing the common subterms whenever it is possible; secondly, they
    prevent the construction of ill-formed terms like 
    [(NotArith (Arith a))] or [(Arith (NotArith a))]. *)

val hc : term_node -> term

(*s Equality and comparison of terms.  Due to internal hash-consing,
    equality is always done in constant time $O(1)$. Comparison
    functions are split into two categories: fast ones using internal
    tagging with unique integers, and structural ones. The former are
    session-dependent and the latter are not. *)

val fast_cmp : term -> term -> int
val cmp : term -> term -> int

val is_const : term -> bool

val is_uninterpreted : term -> bool
    
(*s Caches for functions over terms. *)
		  
val cache : int -> (term -> 'a) -> (term -> 'a)
val cache2 : int -> (term*term -> 'a) -> (term*term -> 'a)    
val cachel : int -> (term list -> 'a) -> (term list -> 'a)    
