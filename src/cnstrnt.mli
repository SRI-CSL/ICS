
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

(*s Module [Cnstrnt]: extends the [Interval] module with functions
  specific to terms. *)

type t = Interval.t

  (*s Abstract interpretation of an arithmetic term with [Cnstrnts.t] as
    abstract domain. [cnstrnt f a] first checks if the context [f]
    contains a declaration or not not. In the first case, this
    constraint is returned. Otherwise, when [f] throws the exception
    [Not_found] then it computes a most refined constrained by traversing
    arithmetic terms *)

val cnstrnt : (Term.t -> Interval.t) -> Term.t -> t

  (*s Application of a constraints to a term. The following simplifications
    are used.\\
    \begin{tabular}{lcll}
    [app c a] & = & ff() & if [is_empty c] \\
    [app c a] & = & ff() & if [is_ground c] and not [mem a c] \\
    [app c a] & = & tt() & if [is_full c] \\
    [app c a] & = & tt() & if [mem a c] \\
    \end{tabular} *)

type status = Yes | No | X
   
val mem : Term.t -> t -> status

    
    (*s Apply a constraint. *)

val app : t -> Term.t -> Term.t
