
(*i*)
open Term
(*i*)

val occurs : term -> term -> bool

val tt : term
val ff : term
  
val neg : term -> term
val conj : term -> term -> term
val disj : term -> term -> term
val xor : term -> term -> term
val imp : term -> term -> term
val iff : term -> term -> term
val ite : term -> term -> term -> term

val forall : variable list -> term -> term
val exists : variable list -> term -> term

val solve : term * term -> (term * term) list
val solve_deq : term * term -> (term * term) list

val infer: term * term -> (term * term) list

