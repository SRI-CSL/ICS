

(*s Datatype for power products. *)


type t

val of_term : Term.t -> t
val to_term : t -> Term.t

val partition: t -> (t * t) option       (*s partition in head and tail. *)

val remove : Term.t -> t -> t

val inter : t * t -> t
val diff : t * t -> t

val divides : t * t -> bool

val cancel : t * t -> t * t

val cp : t * t -> (t * t) option

val mult : t * t -> t
