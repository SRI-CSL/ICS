
(*i*)
open Term
(*i*)

type result =
  | Valid
  | Inconsistent
  | Consistent of State.t

val process: term option -> State.t -> term -> result
