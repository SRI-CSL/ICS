
(*i*)
open Mpa
open Term
open Hashcons
(*i*)

type monomial = Q.t * ((term list) hashed)

val to_poly : term -> Poly.t
val of_poly : Poly.t -> term

val occurs: term -> term -> bool

(*s Smart constructors *)

val num  : Q.t -> term
val zero : term
val one  : term
val add  : term list -> term
val add2 : term * term -> term
val sub  : term * term -> term
val incr : term -> term
val neg  : term -> term
val mult : term list -> term
val mult2: term * term -> term

val divq : Q.t -> term  -> term
val multq : Q.t -> term -> term

val map : (monomial -> monomial) -> term -> term

val le : term -> term -> bool
val lt : term -> term -> bool

    
(*s Canonizer and solver for arithmetic. \label{arith} *)

val solve : term option -> term * term -> (term * term) list




