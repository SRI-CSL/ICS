
(*i*)
open Term
(*i*)

val norm : Congstate.t -> term -> term

type result =
  | Valid
  | Inconsistent
  | Consistent of Congstate.t

val term : term option -> Congstate.t -> term -> result

val equality : term option -> Congstate.t -> term * term -> Congstate.t
