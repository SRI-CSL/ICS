
(*i*)
open Mpa
open Term
open Hashcons
(*i*)

type monomial = Q.t * ((term list) hashed)

val to_poly : term -> Poly.t
val of_poly : Poly.t -> term

val integer: term -> term

val is_integer: term -> bool

val occurs: term -> term -> bool

(*s Smart constructors *)

val num  : Q.t -> term
val zero : unit -> term
val one  : unit -> term
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

(*s Abstract interpretation *)

type sign = Nonpos | Neg | Zero | Pos | Nonneg | T

val sign: term -> sign
    
val is_nonneg : term -> bool
val is_pos : term -> bool
val is_neg : term -> bool
val is_nonpos : term -> bool

val inconsistent: term -> term -> bool
