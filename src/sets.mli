   
val empty : Term.tag -> Term.t
val full : Term.tag -> Term.t

    (*s Finite set and arithmetic set constraints. *)
    
val finite : Term.tnode Ptset.t -> Term.t       
val cnstrnt : Interval.t -> Term.t

    
    (*s Set inclusion and set equality. *)
    
val sub : Term.tag -> Term.t -> Term.t -> Term.t
val equal : Term.tag -> Term.t -> Term.t -> Term.t

    
    (*s Set connective, including recognizers and destructors.
      [ite(x,p,n)] can be thought of being defined as
      [union (inter x p) (inter (compl x) n)], where [union],[inter],
      and [compl] are just set union, set intersection, and set complement,
      respectively. [diff s1 s2] is the set difference, and [sym_diff]
      is the symmetric set difference operator.

      [sub $s_1$ $s_2$] denotes the set [union (compl $s_1$) $s_2$],
      and [equal $s_1$ $s_2$] denotes [inter (sub $s_1$ $s_2$) (sub $s_2$ $s_1$)]. *)
    
      
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

    
    (*s Solving equalities over terms built up from set connectors.
      The solver satisfies the following recursive definition.
         \begin{tabular}{lcl}
         [solve(setite(x,p,n) = full)]
            & = & [x = p inter (compl c union delta)] \\
            & and & [solve(p union n = full)] \\
         \end{tabular}
      Given an equation [$s_1$ = $s_2$] between sets, solve first
      computes the set [s] which equals [equal $s_1$ $s_2$], and
      then solves [solve(s,full)]. *)

val solve : Term.tag -> Term.eqn -> Term.eqn list










