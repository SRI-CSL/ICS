
(*i*)
open Term
(*i*)

val equal : term -> term -> term
val diseq : term -> term -> term

val is_disequality : term -> bool

val destructure_disequality : term -> (term * term) option

val solve : term * term -> (term * term) list
