
(*i*)
open Mpa
open Hashcons
open Bitv
(*i*)

(*s Types of terms. \label{typeterms}
    There is one type for each theory
    ([arith], [tuple], etc.). It facilitates the definition of solvers
    and canonizers by allowing pattern-matching on a type containing only
    the constructors corresponding to the given theory. 
    Any other kind of term (i.e. uninterpreted or from another theory) is
    handled through a particular constructor ([NotArith], [NotTuple], etc.)
    Then the type of terms ([term]) assembles the different theories,
    and introduce uninterpreted terms (constructors [Var] and [App]). *)

type constraints = Int | All
  
type variable = string * constraints

type tag = int

type term_node =
  | Var of variable
  | App of term * term list
  | Update of term * term * term
  | Equal of term * term
  | Bv of bv
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Mem of term * term
  | Integer of term
        
and term = term_node hashed

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

val var : variable -> term
val app : term -> term list -> term
val update : term -> term -> term -> term
val equal : term -> term -> term
val mem : term -> term -> term
val integer: term -> term

val ptrue  : unit -> term
val pfalse : unit -> term
val ite    : term -> term -> term -> term
val forall : variable list -> term -> term
val exists : variable list -> term -> term

val empty  : tag -> term
val full   : tag -> term
val setite : tag -> term -> term -> term -> term

val tuple: term list -> term
val proj : int -> int -> term -> term

val num  : Q.t -> term
val mult : term list -> term
val add  : term list -> term

val const : Bitv.t -> term
val fixed : int -> term -> fixed
val conc  : fixed list -> term
val extr  : fixed -> int -> int -> term
val bvite : fixed -> fixed -> fixed -> term

(*s Fresh variables. *)
  
val fresh : string -> term list -> constraints -> term
val new_var : string -> constraints -> term

(*s Equality and comparison of terms.  Due to internal hash-consing,
    equality is always done in constant time $O(1)$. Comparison
    functions are split into two categories: fast ones using internal
    tagging with unique integers, and structural ones. The former are
    session-dependent and the latter are not. *)

val eq_term : term -> term -> bool
val fast_compare_term : term -> term -> int
val compare_term : term -> term -> int

    
val is_var : term -> bool
val is_num : term -> bool
val is_const : term -> bool
val is_bv : term -> bool
val is_arith : term -> bool
val is_atomic : term -> bool

val is_uninterpreted : term -> bool

val num_of : term -> Q.t option
val val_of : term -> Q.t

val width_of : term -> int

(*s Caches for functions over terms. *)
 
		  
val cache : int -> (term -> 'a) -> (term -> 'a)
val cache2 : int -> (term*term -> 'a) -> (term*term -> 'a)    
val cachel : int -> (term list -> 'a) -> (term list -> 'a)    
    
    
(*s The following exceptions are raised by any solver which detects an 
    inconsistent system or an obvious validity. *)
    
exception Inconsistent of string
exception Valid





