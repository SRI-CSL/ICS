
(*i*)
open Term
(*i*)

(*s Abstract interpretation *)

type sign = F | Nonpos | Neg | Zero | Pos | Nonneg | T

val inf: sign * sign -> sign

val sign: term -> sign
    
val is_nonneg : term -> bool
val is_pos : term -> bool
val is_neg : term -> bool
val is_nonpos : term -> bool
