
(*i*)
open Term
(*i*)

val eq  : term * term -> term
val deq : term * term -> term
val le  : term * term -> term
val lt  : term * term -> term
    
val pos : term -> term
val neg : term -> term
val nonneg : term -> term
val nonpos : term -> term
    
val int : term -> term

val solve : term * term -> (term * term) list
