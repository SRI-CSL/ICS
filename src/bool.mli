
val tt : unit -> Term.t
val ff : unit -> Term.t

val equal : Term.t -> Term.t -> Term.t
val diseq : Term.t -> Term.t -> Term.t

  
val neg : Term.t -> Term.t
val conj : Term.t -> Term.t -> Term.t
val disj : Term.t -> Term.t -> Term.t
val xor : Term.t -> Term.t -> Term.t
val imp : Term.t -> Term.t -> Term.t
val iff : Term.t -> Term.t -> Term.t
val ite : Term.t -> Term.t -> Term.t -> Term.t

val conjl : Term.t list -> Term.t
val disjl : Term.t list -> Term.t

val forall : Term.variable list -> Term.t -> Term.t
val exists : Term.variable list -> Term.t -> Term.t

val is_tt : Term.t -> bool
val is_ff : Term.t -> bool

val is_equal : Term.t -> bool
val is_diseq : Term.t -> bool

val is_neg : Term.t -> bool
val is_conj : Term.t -> bool
val is_disj : Term.t -> bool
val is_xor : Term.t -> bool
val is_imp : Term.t -> bool
val is_iff : Term.t -> bool
val is_ite : Term.t -> bool

val d_diseq : Term.t -> Term.t * Term.t
val d_equal : Term.t -> Term.t * Term.t

val d_neg : Term.t -> Term.t
val d_conj : Term.t -> Term.t * Term.t
val d_disj : Term.t -> Term.t * Term.t
val d_xor : Term.t -> Term.t * Term.t
val d_imp : Term.t -> Term.t * Term.t
val d_iff : Term.t -> Term.t * Term.t
val d_ite : Term.t -> Term.t * Term.t * Term.t

val solve : Term.eqn -> Term.eqn list option
