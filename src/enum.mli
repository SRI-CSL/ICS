
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
 *
 * Author: Harald Ruess
 i*)

(*s The module [Enum] implements constructors for tuples and 
  projections, normalization of enumerations, a solver for this
  theory. *)

(*s [is_interpreted a] holds iff [a] is an enumeration type
  Terms for which [is_tuple] is false are considered to be uninterpreted. *)

val is_interpreted : Term.t -> bool

(*s Constructing enumeration types. *)

val mk_enum : Name.Set.t -> Name.t -> Term.t

(*s Destructure an enumeration. *)

val d_enum : Term.t -> (Name.Set.t * Name.t) option

(*s [sigma op l] applies the function symbol [op] from the enumeration theory to
  the list [l] of terms. *)

val sigma : Sym.enum -> Term.t list -> Term.t

val tau : Sym.enum -> Type.t list -> Type.t


(*s [type_of f a] computes the best type of [a] in context [f]. *)

val type_of : (Term.t -> Type.t) -> Term.t -> Type.t

(*s Given a substitution [s] and a term [a], [norm s a] replaces uninterpreted
  occurrences [x] in [a] with term [y], if there is a binding [x |-> y] in [s].
  The resulting term is in sigmatized.  Thus, [norm] can be thought of as the the 
  composition of applying substitution [s] to [a] followed by sigmatizing each 
  interpreted subterm by the function [sigma] above. *)

val norm: Term.t Term.map -> Term.t -> Term.t

(*s [solve (a,b)] in the theory of tuples is either [(a,b)] or
    [Exc.Inconsistent]. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list
