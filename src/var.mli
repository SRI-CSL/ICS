
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

(*s Module [Var] includes several constructors and recognizers
  dealing with variables. *)


  (*s Equality for variable names. These are hash-consed, and
    therefore this equality test is in constant time. *)

val eq : Term.variable -> Term.variable -> bool

    (*s [var x] constructs a variable of name [x], and [is_var a]
      tests if the argument term is indeed a variable. In this case,
      [d_var a] yields the name of the argument variable. *)

val var : Term.variable -> Term.t  
val is_var : Term.t -> bool
val d_var : Term.t -> Term.variable

    (*s [fresh x] creates a fresh constants. These are 0-ary uninterpreted
      symbols. They are given named like \_c123 in order to prevent clashes
      with the user's symbols. Create uses global counters for creating fresh
      names; these are reinitialized using the [reset] command of ICS. *)

val fresh : Term.variable -> Term.t option -> Term.t

val is_fresh : Term.t -> bool

    (* Return, if possible a term equivalent to the argument, which is supposed
       to be a fresh variable. The result does not contain this argument variable. *)

val equiv : Term.t -> Term.t option

    (*s Set of fresh variables occuring in a term. *)
    
val fresh_of : Term.t -> Term.terms 
    