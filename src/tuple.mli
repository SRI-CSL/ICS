(*
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
 *)

(** Theory of tuples.

  @author Harald Ruess

  The signature of this theory consists of the [n]-ary function symbol
  [product] for constructing tuples and of the family of unary 
  function symbols [proj i n)], for integers [0 <= i < n], for projecting 
  the [i]-th component (starting with [0] and addressing in increasing 
  order from left to right).

  The theory of tuples is given in terms of the equality theory
  - [(proj i n)(product(a0,...,an-1)) = ai]
  - If [ai = bi] for [i=0,...,n-1], then
         [product(a0,...,an-1) = product(b0,...,bn-1)]
*)

(** {6 Function symbols} *)

val product : Sym.t
val proj : int -> int -> Sym.t


(** {6 Constructors} *)

val mk_tuple : Term.t list -> Term.t
  (** If the argument list [l] is of length [1], then
    this term is returned. Otherwise, [tuple l] constructs
    the corresponding tuple term. *)
   
val mk_proj : int -> int -> Term.t -> Term.t
  (** [mk_proj i n a] is the constructor for the family of [i]-th projections
    from [n]-tuples, where [i] is any integer value between [0] and [n-1].
    This constructor simplifies [proj i n (tuple [a0;...;an-1])] to [ai]. *)


(** {6 Recognizers} *)

val is_interp : Term.t -> bool
  (** [is_interp a] holds iff [a] is an application of a term [b]
    to a projection symbol [proj _ _], or [a] is an application of
    the symbol [product] to a list of terms. Terms for 
    which [is_interp] is [false] are considered to be {i uninterpreted}
    in the theory of tuples, and are usually treated as they were
    variables by the functions in this module *)


(** {6 Iterators} *)

val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** If [x1, ..., xn] are the variables and uninterpreted terms of [a],
    then [fold f a e] is  [f (... (f (f e x1) x2) ...) xn]. *)
  

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** - [map f (mk_tuple al)] equals [mk_tuple (List.map f al)]
    - [map f (mk_proj i n a)] equal [mk_proj i n (map f a)]
    - Otherwise, [map f x] equals [f x] *)


(** {6 Canonization} *)

val sigma : Sym.product -> Term.t list -> Term.t
  (** [sigma op l] applies the function symbol [op] from the tuple theory to
    the list [l] of terms. For the function symbol [Proj(i,n)] and the list [a],
    it simply applies the constructor [proj i n a], and for [Tuple] and it 
    applies [tuple l]. All other inputs result in a run-time error. *)


(** {6 Solver} *)

val solve : Fact.equal -> Fact.equal list
  (** [solve e] raises the exception {!Exc.Inconsistent} if 
    the equality [e] is inconsistent in the theory of tuples.
    Otherwise, it returns a list of solved equalities such that the
    conjunction of these equalities is equivalent to [e] in the
    theory of tuples. The solved equalities are of the form [x = a]
    where [x] is a variable in [e], and none of the lhs variables [x]
    in the solved form occurs in any of the rhs [a]. In addition, 
    every rhs [a] is canonical and may contain fresh variables not
    in [e].  These variables are generated using {!Var.mk_fresh} and
    thus there is a possible side-effect on the counter {!Var.k}. *)

