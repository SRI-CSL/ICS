
(*i*)
open Term
(*i*)

val fold: (term -> 'a -> 'a) -> term -> 'a -> 'a
val size : term -> int
val is_pure : term -> bool
val map : (term -> term) -> term -> term
val replace : term -> term -> term -> term
