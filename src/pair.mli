(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Theory of tuples.

  @author Harald Ruess

  The signature of the theory of pairs consists of
  - [cons], the constructor of arity two.
  - [car] of arity one, projection on first component.
  - [cdr] of arity one, projection to second component.

  The theory is axiomatized by
  - [car(cons(s, t)) = s]
  - [cdr(cons(s, t)) = t]
  - [cons(s, t) = cons(u, v)] implies [s = u] and [t = v]
*)

(** {6 Function symbols} *)

val cons : Sym.t
val car : Sym.t
val cdr : Sym.t


(** {6 Constructors} *)

val mk_cons : Term.t -> Term.t -> Term.t

val mk_car : Term.t -> Term.t
 
val mk_cdr : Term.t -> Term.t

val mk_tuple : Term.t list -> Term.t

val mk_proj : int -> Term.t -> Term.t



(** {6 Recognizers} *)

val is_interp : Term.t -> bool
 
  

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** - [map f (mk_tuple al)] equals [mk_tuple (List.map f al)]
    - [map f (mk_proj i n a)] equal [mk_proj i n (map f a)]
    - Otherwise, [map f x] equals [f x] *)


(** {6 Canonization} *)

val sigma : Sym.pair -> Term.t list -> Term.t
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

