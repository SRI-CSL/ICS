

type t = Term.t * Term.t

val mem : t -> t list -> bool

val cons : t -> t list -> t list

val (@@) : t list -> t list -> t list

val remove : t -> t list -> t list
