
(*i*)
open Term
(*i*)

val occurs: term -> term -> bool
    
val empty : tag -> term
val full : tag -> term
    
val mem : term -> term -> term
    
val ite : tag -> term -> term -> term -> term

val inter : tag -> term -> term -> term
val union : tag -> term -> term -> term
val compl : tag -> term -> term
val diff : tag -> term -> term -> term
val sym_diff : tag -> term -> term -> term

val solve : tag -> term * term -> (term * term) list
val solve_deq : tag -> term * term -> (term * term) list
