
(*i*)
open Mpa
(*i*)

(*s Type [t] represents a union of real intervals. *)

type domain = Int | Real | NonintReal

type t

val empty : t
val full : t
   
val oo : domain -> Q.t -> Q.t -> t
val oc : domain -> Q.t -> Q.t -> t
val co : domain -> Q.t -> Q.t -> t
val cc : domain -> Q.t -> Q.t -> t

val lt : domain -> Q.t -> t
val le : domain -> Q.t -> t
val gt : domain -> Q.t -> t
val ge : domain -> Q.t -> t

val int : t
val real : t
val nonint : t

val singleton : Q.t -> t
val diseq : Q.t -> t

    (*s Recognizers. *)
    
val is_empty : t -> bool
val is_full : t -> bool
val is_int : t -> bool
val is_real : t -> bool
val is_singleton : t -> bool

    (*s Get the value of a singleton constraint. Throws [Invalid_argument] if
        argument constraint is not a singleton. *)

val value_of : t -> Q.t

    (* Membership test, subsumption, and equality. *)

val mem : Q.t -> t -> bool
val eq : t -> t -> bool

    (*s Pretty printing constraints. *)

val pp : Format.formatter -> t -> unit

    (*s Comparison of constraints. *)

val cmp : t -> t -> Binrel.t

    (*s Set operations on constraints. *)

val inter : t -> t -> t
val union : t -> t -> t
val compl : t -> t
    
    (*s Abstract interpretation of addition and multiplication. *)

val add : t -> t -> t
val mult : t -> t -> t




