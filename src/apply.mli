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

(** Theory of function application and abstraction.

  @author Harald Ruess

  Terms in this theories are build from the symbols [apply(r)] and [abs] 
  for function application and abstraction, respectively.  The [r] argument 
  in [apply(r)] is used to infer a constraint [c] for application terms 
  if [r] is of the form [Some(c)]. deBruijn indices are used to in represent 
  bound variables (see module {!Var}).  All other terms are considered to 
  be uninterpreted.

  Equality theory
  - [apply(abs(x), y) = x[!0/y]]

  Only strongly-normalizing terms are considered here, since, otherwise,
  beta-reduction may lead to nontermination in building function applications.
*)


type norm = Sym.t -> Term.t list -> Term.t

(** {6 Constructors} *)

val mk_apply : norm -> Dom.t option -> Term.t -> Term.t -> Term.t
  (** [mk_apply norm c a al] builds an application with range type [c] by
    applying term [a] to an argument term [b].  In addition, it performs
    beta-reduction. The [norm] argument is used to further simplify beta redeces. *)

val mk_abs : Term.t -> Term.t
  (** [mk_abs a] lambda-abstracts [a]. All free occurrences in [a] of 
    free variables  of the form [!0] are bound by this abstraction. *)


(** {6 Canonizer} *)

val sigma : norm -> Sym.apply -> Term.t list -> Term.t
  (** Depending on the function symbol [f], [sigma f al] uses the constructors
    {!Apply.mk_apply} and {!Apply.mk_abs} to compute  the normal-form of applying [f] to the 
    arguments [al]. *)


(** {6 Iterators} *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] applies the term transformer [f] to each uninterpreted 
    subterm of [a] and rebuilds the term [a] by using the simplifying constructors
    [mk_apply] and [mk_abs]. *)

val apply : Term.Equal.t -> Term.t -> Term.t
