 
(*s A state consists of four components.
    \begin{description}
    \item{[find]}: returns find for any variable/uninterp term (vterm). It
                 can be thought of a finite set of equations [a = b], where
                 [a] is a vterm.
    \item[use]: maps vterms to rhs terms in find that contain them interpreted.
    \item[ext]: maps rhs to the set of lhs with rhs as find.
    \item[uninterp]: maps uninterp function symbols to
                     uninterpreted terms in the domain.
    \end{description}
    In the [empty] context, [find] is the identity function on terms.
    [copy] is a only a shallow copy of a state. Given a state [s]
    and a term equality [(a,b)], [update s (a,b)] sets the [find] of
    [a] to [b], and updates the [use], [ext], and [uninterp] indices
    correspondingly. 
 *)

type t

val to_subst : t -> Subst.t

val empty : t
val copy : t -> t
val apply : t -> Term.t -> Term.t
    
val find  : t -> Term.t -> Term.t
val ext : t -> Term.t -> Term.terms
val use : t -> Term.t -> Term.terms
val uninterp : t -> Funsym.t -> Term.terms
val cnstrnt: t -> Term.t -> Cnstrnt.t
    
val mem : t -> Term.t -> bool

val ctxt_of : t -> Term.eqn list
val find_of : t -> Subst.t
val ext_of : t -> Term.terms Term.Map.t
val cnstrnt_of : t -> Cnstrnt.t Term.Map.t
val use_of : t -> Term.terms Term.Map.t
val uninterp_of : t -> Term.terms Funsym.Map.t
    
val add_ctxt : t -> Term.eqn -> unit
val add_eqn : t -> Cnstrnt.t -> Term.eqn -> unit
val add_cnstrnt : t -> Cnstrnt.t -> Term.t -> unit
    
val pp_find : Format.formatter -> t -> unit
val pp_use : Format.formatter -> t -> unit
val pp_uninterp : Format.formatter -> t -> unit




