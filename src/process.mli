
(*i*)
open Term
(*i*)

val norm : State.t -> term -> term

type result =
  | Valid
  | Inconsistent
  | Consistent of State.t

val term : term option -> State.t -> term -> result

val equality : term option -> State.t -> term * term -> State.t
