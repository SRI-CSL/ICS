
(*i*)
open Term
(*i*)

val var : variable -> term
  
val fresh : string -> term list -> constraints -> term
    
val new_var : string -> constraints -> term
    
val is_var : term -> bool
