
(*i*)
open Term
(*i*)

val occurs: term -> term -> bool

(*s Smart constructors *)

val tuple : term list -> term
val proj : int -> int -> term -> term

(*s Solver for equation on tuples. \label{tuple} *)

val solve : term * term -> (term * term) list
