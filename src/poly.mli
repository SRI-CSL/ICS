
(*i*)
open Mpa
open Term
open Hashcons
(*i*)

(*s A {\em power product} is a list of terms which are ordered from
    left-to-right according to the session-independent [Term.cmp]
    comparison on terms; e.g. the power product $x^2*y~3$ is represented
    by the list {\tt [x;x;y;y;y]}\@. Since power products are hash-consed,
    they can be compared for equality using the identity [==].

    A polynomial can be viewed as a a map from power products to rational coefficients.
  
    A monomial is a pair consisting of a rational coefficient and a power product.
  *)

type pproduct = (term list) hashed
type monomial = Q.t * pproduct

type t


(*s Tests if a term [x] occurs in one of the power products of the polynomial *)

val occurs : term -> t -> bool

(* Constructors and recognizers for the [zero] and [one] polynomial,
   for constant polynomials, and for polynomials consisting only of a
   singleton monomial. *)
  
val zero : t
val one : t
val num : Q.t -> t
val monomial : Q.t * term list -> t

val is_zero : t -> bool
val is_num : t -> bool

val num_of : t -> Q.t

(*s [exists f p] checks if at least one monomial of the polynomial satisfies
    the predicate [f], whereas [for_all f p] checks if every monomial of
    the polynomial satisfies the predicate [f]. *)
 
val exists : (monomial -> bool) -> t -> bool
   
val for_all : (monomial -> bool) -> t -> bool
         
(*s Arithmetic operations on polynomials *)

val neg : t -> t
val add2 : t -> t -> t
val add : t list -> t
val sub : t -> t -> t
val mult2 : t -> t -> t
val mult : t list -> t

(*s Destructure a polynomial into the constant coefficient and the
    remaining polynomial *)

val destructure: t -> t * Q.t

(*s Choose the smallest monomial *)

val smallest : t -> monomial * t

(*s Choose a monomial satisfying some predicate from a polynomial.
    If there is such a monomial [m] then return [Some(m)] and the
    other monomials. Otherwise return [None]
  *)

val choose : (monomial -> bool) -> t -> monomial option * t

(*s Iterators on polynomials *)

val fold : (monomial -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (monomial -> unit) -> t -> unit
val map : (monomial -> monomial) -> t -> t    

val mapq : (Q.t -> Q.t) -> t -> t
 

(* Partition a polynomial into a polynomial consisting of
   monomials which satisfy some predicate [p] and the polynomial
   formed from the remaining monomials. *)

val partition : (monomial -> bool) -> t -> t * t

(*s Compute a polynomial by applying a function [f] to all
    coefficients of the argument polynomial.
  *)
 
val multq : Q.t -> t -> t
val addq : Q.t -> t -> t
val divq : Q.t -> t -> t

(*s Represent a polynomial as a list of (ordered) monomials. *)
    
val to_list : t -> monomial list

(*s Greatest common divisor (gcd) and least common multiple
    of a polynomial's (rational) coefficients.
 *)

val gcd : t -> Z.t
val lcm : t -> Z.t



(*s Check if a polynomial is diophantine; i.e. all coefficients are integers
    and all terms of the power product are interpreted over the integers, too. *)

val is_diophantine: (term -> bool) -> t -> bool

(*s Solving polynomial equations over the rationals and the
    integers
 *)

val qsolve : term option -> t -> (pproduct * t) list

val zsolve : (Q.t list -> Q.t -> (Q.t * term list) option)
                -> (t -> (pproduct * term) list)

(*s Pretty-printing a polynomial to standard output *)

val pp : t -> unit






