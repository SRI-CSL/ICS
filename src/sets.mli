   
val empty : Term.tag -> Term.t
val full : Term.tag -> Term.t

    (*s Finite set and arithmetic set constraints. *)
    
val finite : Term.tnode Ptset.t -> Term.t       
val cnstrnt : Interval.t -> Term.t

    
    (*s Set inclusion and set equality. *)
    
val sub : Term.tag -> Term.t -> Term.t -> Term.t
val equal : Term.tag -> Term.t -> Term.t -> Term.t

    
    (*s Set connective, including recognizers and destructors. *)
      
val ite : Term.tag -> Term.t * Term.t * Term.t -> Term.t

val inter : Term.tag -> Term.t -> Term.t -> Term.t
val union : Term.tag -> Term.t -> Term.t -> Term.t
val compl : Term.tag -> Term.t -> Term.t
val diff : Term.tag -> Term.t -> Term.t -> Term.t
val sym_diff : Term.tag -> Term.t -> Term.t -> Term.t
val equal : Term.tag -> Term.t -> Term.t -> Term.t

val is_compl : Term.t -> bool
val is_inter : Term.t -> bool
val is_union : Term.t -> bool
val is_sym_diff : Term.t -> bool
val is_sub : Term.t -> bool
val is_equal : Term.t -> bool

val d_compl : Term.t -> Term.t
val d_inter : Term.t -> Term.t * Term.t
val d_union : Term.t -> Term.t * Term.t
val d_sym_diff : Term.t -> Term.t * Term.t
val d_sub : Term.t -> Term.t * Term.t
val d_equal: Term.t -> Term.t * Term.t

    
    (*s Solving equalities over terms built up from set connectors. *)

val solve : Term.tag -> Term.eqn -> Term.eqn list










