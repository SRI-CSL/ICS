
type t = Term.cnstrnt

type domain = Interval.domain

val make : t -> Term.t

val empty : t
val full : t

val zero : t
val one : t
val singleton: Mpa.Q.t -> t

val is_singleton : t -> bool
    
val value_of : t -> Mpa.Q.t

val oo : domain -> Mpa.Q.t ->  Mpa.Q.t -> t
val oc : domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
val cc : domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
val co : domain ->  Mpa.Q.t ->  Mpa.Q.t -> t
    
val int : t                          (* integer constraint *)
val real : t                         (* real constraint *)
val nonintreal : t                   (* non-integer real constraint *)
  
val diseq : Mpa.Q.t -> t             (* disequality constraint *)
    
val lt : domain -> Mpa.Q.t -> t
val le : domain -> Mpa.Q.t -> t
val gt : domain -> Mpa.Q.t -> t
val ge : domain -> Mpa.Q.t -> t

val is_empty : t -> bool
val is_full : t -> bool

val mem : Term.t -> t -> bool

val inter : t -> t -> t
val union : t -> t -> t
val compl : t -> t
val ite : t -> t -> t -> t
  
    (*s [is_real c]  holds iff the kind part of [c] is [Int] or Real] or if
        the sign part of is a genuine constraint (i.e. other than [Top] and [Bot]).
        The other recognizers are defined similarly. *)

val is_int : t -> bool
val is_real : t -> bool
    
    (*s Comparison of constraints. *)

val cmp: t -> t -> Binrel.t
    
    (*s Abstract interpretation of an arithmetic term with [Cnstrnts.t] as
      abstract domain. [cnstrnt f a] first checks if the context [f]
      contains a declaration or not not. In the first case, this
      constraint is returned. Otherwise, when [f] throws the exception
      [Not_found] then it computes a most refined constrained by traversing
      arithmetic terms
    *)

val cnstrnt : (Term.t -> t) -> Term.t -> t

    (*s Application of a constraints to a term. *)

val app : t -> Term.t -> Term.t

    








