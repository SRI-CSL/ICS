
(*i*)
open Term
(*i*)

(*s Pretty-printing. *)

val term : term -> unit
val bv : bv -> unit
val eqn : term * term -> unit

val list : ('a -> unit) -> 'a list -> unit
val list_sep : (unit -> unit) -> ('a -> unit) -> 'a list -> unit

val tset : Tset.t -> unit
val tmap : ('a -> unit) -> 'a Tmap.t -> unit
