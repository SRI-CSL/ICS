
(*i*)
open Term
(*i*)

type typ = F | Int | Real | Nonint | Nonreal | NonintReal | T

val typ : State.t -> term -> typ

val inconsistent : State.t -> term * term -> bool
    
val infer : State.t -> term * term -> (term * term) list
