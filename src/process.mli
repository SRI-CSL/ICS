
(*i*)
open Term
(*i*)

val norm : State.t -> term -> term

val process : term option -> State.t -> term * term -> State.t
