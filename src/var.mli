(*i*)
open Term
(*i*)

val var : variable -> term
  
val is_var : term -> bool

val create : variable -> term

val fresh : variable  -> term list -> term
    
