
(*i*)
open Term
(*i*)

val app: term -> term list -> term
val update : term -> term -> term -> term

(*s Solver for arrays. \label{arrays} *)

val solve : term * term -> (term * term) list
