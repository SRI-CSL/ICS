
(*i*)
open Mpa
open Term
(*i*)

val lt : term * term -> term
val le : term * term -> term

val solve : term * term -> (term * term) list
