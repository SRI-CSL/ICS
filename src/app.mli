
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

(*s The module [App] implements various constructors for function application. *)

   (*s [app f l] constructs an application of [f] to a list
     of arguments [l]. This constructor uncurries applications and
     if lifts Boolean conditionals.\\
     \begin{tabular}{lcl}
     [app (app b m) l] & = & [app b [m @ l]] \\
     [app (Ite(x,y,z)) l] & = & [Bool.ite (app a \list{x}) (app a \list{y}) (app a \list{z})]
     \end{tabular}\\
     The function symbol [f] may be an arbitrary term,
     and depending on this term, several simplifications are performed.
     Function update is simplified using the rules\\
     \begin{tabular}{lcll}
     [app (update b j v) j] & = & [v] & \\
     [app (update b j v) i] & = & [app b i] & if [i <> j] holds \\
     [app (update b j v) i] & = & [ite (equal i j) v (app b l)] \\
     \end{tabular}
     and membership tests are reduced as follows.\\
     \begin{tabular}{lcll}
     [app Empty l] & = & ff() & \\
     [app True l] & = & tt() & \\
     [app Cnstrnt(c) l] & = & [Cnstrnt.app c l] & \\
     [app Finite(s) l] & = & finite s l & \\
     [app (SetIte(s1,s2,s3)) l] & = & [ite (app a \list{s1}) (app a \list{s2}) (app a \list{s3})]
     \end{tabular} *)

val app: Term.t -> Term.t list -> Term.t

    (*s [update a i u] is a constructor for updating function [a] at position [i] with
      value [u]. It employs the simplification\\
        \begin{tabular}{lcl}
        [update (update b j v) j u] & = & update b i u \\
        \end{tabular} *)
	 
val update : Term.t * Term.t * Term.t -> Term.t

    (*s [finite s a] builds the set membership test [app a (Finite s)] with the
      additional simplifications that
      \begin{tabular}{lcll}
     [app Finite(s) l] & = & tt() & if [l] is a member in [s] \\
     [app Finite(s) l] & = & ff() & if [l] is not in [s] and [l] is known to be disjoint form [s] \\
     \end{tabular} *) 

val finite : Term.terms -> Term.t -> Term.t




