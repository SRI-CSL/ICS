
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

(*s The module [Tuple] implements constructors for tuples and projections,
  and a solver for the tuple theory. *)


     (*s If the argument list [l] is of length [1], then
      this term is returned. Otherwise, 
      [tuple l] constructs the corresponding tuple term. *)

val tuple : Term.t list -> Term.t

    
    (*s [proj i n a] is the constructor for the family of [i]-th projections
        from [n]-tuples, where [i] is any integer value between [0] and [n-1].
        This constructor uses the simplification.\\
           \begin{tabular}{lcl}
           [proj i (tuple \list{$a_0$;\ldots,$a_{n-1}$})] & = & [$a_i$]
           \end{tabular} *)
 
val proj : int -> int -> Term.t -> Term.t
  

    (*s The solver for equations over tuples and projections works by repetitively
      applying the following reductions on equations.\\
      \begin{tabular}{lcl}
      [(tuple \list{$a_0$,\ldots,$a_n$},~tuple \list{$a_0$,\ldots,$a_n$})]
          & = & [\list{($a_0$,$b_0$),\ldots,($a_n$,$b_n$)}] \\
      [(proj i n a, b)] & = & [(a, tuple \list{$c_0$,\ldots,c_{i-1},b,...c_{n-1}})]
      \end{tabular}
      where [$c_i$] are fresh, and [b] at [i]-th position.
  *)

val solve : Term.eqn -> Term.eqn list option
 
