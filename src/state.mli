
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*s Module [State]: Datastructure for logical database including
  various indeces for performance improvements. *)

(*s A state consists of the following components.
    \begin{description}
    \item{[ctxt]}: The logical context, not necessarily in canonized form.
    \item{[find]}:
         returns find for any variable/uninterp term (vterm). It
         can be thought of a finite set of equations [a = b], where
         [a] is a vterm.
    \item{[use]}:
         maps vterms to rhs terms in find that contain them interpreted.
    \item{[ext]}:
         maps rhs to the set of lhs with rhs as find.
    \item{[uninterp]}:
         maps uninterp function symbols to uninterpreted terms in the domain.
    \end{description}
    In the [empty] context, [find] is the identity function on terms.
    
 *)

type t

  (*s The initial state. *)
val empty : t

  (*s Shallow copying of a state. This function should be called
    before destructively updating a state in order to create a new state. *)
  
val copy : t -> t

    (*s [apply s a] returns [b] if there is a binding [a |-> b] in the find
      structure of [s]; otherwise [Not_found] is raised. *)
    
val apply : t -> Term.t -> Term.t

    (*s [find s a] returns [b] if there is a binding [a |-> b] in the find
      structure of [s]; otherwise [a] is returned *)
    
val find  : t -> Term.t -> Term.t

    (*s Given b term [b] with [b === find s b], [ext s b] yields the set
      of terms [a] such that [a |-> b] is in the [find] of [s]. *)
    
val ext : t -> Term.t -> Term.terms

     (*s Given an uninterpreted term [a], [use s a] yields the set of
       terms [b] in the codomain of the find of [s], such that [a] occurs
       interpreted in [b]. *)
    
val use : t -> Term.t -> Term.terms

     (*s [uninterp s f] yields the set of terms [a] in the domain of the find of [s],
       such that the toplevel function symbol of [a] is [f]. *)
       
val uninterp : t -> Funsym.t -> Term.terms

    (*s Given an arbitrary term [a], [cnstrnt s a] yields the currently best
      known constraint of the [find] of [a]. *)
       
val cnstrnt: t -> Term.t -> Interval.t

(*s Test is term is an integer. *)

val is_int : t -> Term.t -> bool

    (*s Computing a substitution from the [find] structure of a state.
      This translation does not consider the constraint part of a state. *)
  
val to_subst : t -> Subst.t

    (*s [mem s a] tests if term [a] is in the domain of the [find] structure of state [s]. *)
   
val mem : t -> Term.t -> bool

    (*s Accessors for the various parts of a state. *)

val ctxt_of : t -> Term.eqn list
val find_of : t -> Subst.t
val ext_of : t -> Term.terms Term.Map.t
val cnstrnt_of : t -> Interval.t Term.Map.t
val use_of : t -> Term.terms Term.Map.t
val uninterp_of : t -> Term.terms Funsym.Map.t

    (*s [add_ctxt s (a,b)] adds the [(a,b)] equality to the context of the
      state [s]. This procedure destructively updates [s]. *)
    
val add_ctxt : t -> Term.eqn -> unit

    (*s Given a state [s] and a term equality [(a,b)], [add_eqn s c (a,b)] sets
      the [find] of [a] to [b], and updates the [use], [ext], and [uninterp] indices
      correspondingly. It also refines the constraint information of new finds using
      the constraint [c]. [add_eqn] destructively updates [s]. *)
    
val add_eqn : t -> Interval.t -> Term.eqn -> unit

    (*s [add_cnstrnt s c a] refines the cnstrnt of the find of [a].
      [add_cnstrnt] destructively updates [s]. *)
    
val add_cnstrnt : t -> Interval.t -> Term.t -> unit









