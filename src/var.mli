
val var : Term.variable -> Term.t
  
val is_var : Term.t -> bool

val create : Term.variable -> Term.t

val fresh : Term.variable  -> Term.t list -> Term.t
    
