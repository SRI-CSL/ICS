
(*s Equality theories. *)


type t = 
  | Uninterp
  | Interp of interp

and interp = A | T | B | E | BV | NLA

val a : t
val t : t
val b : t
val e : t
val bv : t
val nla : t
val u : t

val of_term : Term.t -> t

val of_eqn : Term.t * Term.t -> t

val name_of : t -> string

(*s May raise [Invalid_argument] exception. *)

val of_name : string -> t
