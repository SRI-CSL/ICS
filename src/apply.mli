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

(** Theory of lambda calculus.

  @author Harald Ruess

  Terms in this theories are build from the symbols 
  - [$] for function application (written in infix notation below),
  - [lam] for functional abstraction, and
  - [cases] for definition by cases.
  
  {i deBruijn} indices are used to in represent bound variables (see module {!Var}).  
  All terms not of the form [a $ b], [lam(a)], [cases(x, y, a, b)] are considered to be
  uninterpreted.

  Equality theory
  - [lam(x) $ y) = x[!0/y]]
  - [cases(x, x, a, b) = a
  - [cases(x, y, Z $ y, Z $ x) = Z $ x
  - [cases(x, y, y, x) = x]
  - [cases(x, y, a, a) = a]

  In these rules, equality means ``reduces to''.  Only strongly-normalizing 
  terms are considered here. Church-Rosser fails, so normal forms are only
  unique for terms without any [cases].
*)

val d_interp : Term.t -> Sym.cl * Term.t list

val mk_apply : Term.t -> Term.t -> Term.t
  (** [mk_apply c a al] builds an application with range type [c] by
    applying term [a] to an argument term [b].  In addition, it 
    performs beta-reduction. *)

val mk_s : unit -> Term.t
val mk_k : unit -> Term.t
val mk_i : unit -> Term.t
val mk_c : unit -> Term.t

val abstract : Name.t -> Term.t -> Term.t
  (** [abstract x a] lamgda-abstracts [a]. All free occurrences in [a] of 
    free variables  of the form [!0] are bound by this abstraction. *)

val sigma : Sym.cl -> Term.t list -> Term.t
  (** Depending on the function symbol [f], [sigma f al] uses the constructors
    {!Apply.mk_apply} and {!Apply.mk_abs} to compute the normal-form of applying [f] to the 
    arguments [al]. *)


val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] applies the term transformer [f] to each uninterpreted 
    subterm of [a] and rebuilds the term [a] by using the simplifying constructors
    [mk_apply] and [mk_abs]. *)

val apply : (Term.t * Term.t) -> Term.t -> Term.t

val disapply : Term.t * Term.t -> Term.t -> Term.t

val solve : Term.Equal.t -> Term.Equal.t list
  (** Work in progress. Not yet used in an essential way. *)
