
(*i*)
open Term
(*i*)
  
type t

val empty : t
val copy : t -> t
val apply : t -> term -> term
val find  : t -> term -> term
val use : t -> term -> Tset.t
val mem : t -> term -> bool
val update : t -> term -> term -> unit
val subterm_close : t -> term -> unit
val subterm_close1 : t -> term -> unit
val pp_find : t -> unit
val pp_use : t -> unit
val pp_universe : t -> unit
