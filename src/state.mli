
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

val  is_int : t -> term -> bool
val  is_real : t -> term -> bool
val  is_pos : t -> term -> bool
val  is_neg : t -> term -> bool
val  is_nonneg : t -> term -> bool
val  is_nonpos : t -> term -> bool
