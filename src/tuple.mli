
(*s Smart constructors *)

val tuple : Term.t list -> Term.t
val proj : int -> int -> Term.t -> Term.t

(*s Solver for equation on tuples. \label{tuple} *)

val solve : Term.eqn -> Term.eqn list option
