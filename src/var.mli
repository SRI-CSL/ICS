(*i*)
open Term
(*i*)

type sgn = Pos | Neg | Nonneg | Nonpos

val var : (variable * sgn option) -> term
    
val intvar : (variable * sgn option) -> term
  
val is_var : term -> bool

val sgn : term -> sgn
    
val is_integer : term -> bool

val create : string * sgn option -> term

val fresh : string -> term list -> sgn option -> term
    
