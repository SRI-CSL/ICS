
(*i
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
 i*)

(*s The module [Tuple] implements constructors for tuples and 
  projections, normalization of tuple terms, a solver for the 
  theory of tuples, and an abstract datatype for logical
  contexts of equalities over tuples. *)

(*s [is_tuple a] holds iff [a] is a projection or a tuple term.
  Terms for which [is_tuple] is false are considered to be uninterpreted. *)

val is_tuple : Term.t -> bool


(*s [iter f a] applies [f] to all top-level uninterpreted subterms of [a]. *)

val iter: (Term.t -> unit) -> Term.t -> unit 


(*s [fold f a e] applies [f] at uninterpreted positions of [a] and 
 accumulates the results starting with [e]. *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a

(*s [norm f a] applies [f] to all top-level uninterpreted subterms of [a],
 and rebuilds the interpreted parts in order. *)

val norm: (Term.t -> Term.t) -> Term.t -> Term.t


(*s If the argument list [l] is of length [1], then
  this term is returned. Otherwise, [tuple l] constructs the 
  corresponding tuple term. *)

val mk_tuple : Term.t list -> Term.t
   
(*s [proj i n a] is the constructor for the family of [i]-th projections
  from [n]-tuples, where [i] is any integer value between [0] and [n-1].
  This constructor simplifies [proj i (tuple \list{a_0;...;a_n-1})] to [a_i]. *)
 
val mk_proj : int -> int -> Term.t -> Term.t

(*s [sigma op l] applies the function symbol [op] from the tuple theory to
  the list [l] of terms. For the function symbol [Proj(i,n)] and the list [a],
  it simply applies the constructor [proj i n a], and for [Tuple] and it 
  applies [tuple l]. All other inputs result in a run-time error. *)

val sigma : Sym.tuple -> Term.t list -> Term.t


(*s [solve (a,b)] returns a solved form for the equation [a = b]. 
  If this equation is inconsistent, then the exception [Exc.Inconsistent]
  is raised.  Otherwise, the solved form [(x1,e1),...,(xn,en)] is returned,
  where [xi] are uninterpreted in the tuple theory and the [ei] are sigmatized.
  The [ei] may also contain fresh variables.  The solver for equations over tuples 
  and projections works by repetitively applying
      \begin{tabular}{lcl}
      [(tuple \list{$a_0$,\ldots,$a_n$},~tuple \list{$a_0$,\ldots,$a_n$})]
          & = & [\list{($a_0$,$b_0$),\ldots,($a_n$,$b_n$)}] \\
      [(proj i n a, b)] & = & [(a, tuple \list{$c_0$,\ldots,c_{i-1},b,...c_{n-1}})]
      \end{tabular}
      where [$c_i$] are fresh, and [b] at [i]-th position. *)

val solve : Term.t * Term.t -> (Term.t * Term.t) list
 
(*s Test if variables has been introduced by tuple solver. *)

val is_fresh : Term.t -> bool
