
(*i*)
open Term
(*i*)

type signum = F | Nonpos | Neg | Zero | Pos | Nonneg | T

(*s Abstract interpretation *)

val inf: signum * signum -> signum

val sign: State.t -> term -> signum
    
val is_nonneg : State.t -> term -> bool
val is_pos : State.t -> term -> bool
val is_neg : State.t -> term -> bool
val is_nonpos : State.t -> term -> bool

val inconsistent : State.t -> term * term -> bool

val infer : State.t -> term * term -> (term * term) list
