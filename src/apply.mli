
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
 * 
 * Author: Harald Ruess
 *)

(** Module [Apply]: 

  Theory of function application and abstraction. Terms in this theories are
  build from the symbols [apply(r)] and [abs] for function application and
  abstraction, respectively.  The [r] argument in [apply(r)] is used to infer
  a constraint [c] for application terms if [r] is of the form [Some(c)]. 
  deBruijn indices are used to in represent bound variables (see module {!Var}).
  All other terms are considered to be uninterpreted.

  Only strongly-normalizing terms are considered here, since, otherwise,
  beta-reduction may lead to nontermination in building function applications.
*)


(** {Signature} *)

val apply : Cnstrnt.t option -> Sym.t
(** Family of function symbols for representing function application. *)

val abs : Sym.t
(** Function symbol for representing functional abstraction. *)


(** {Constructors} *)


val mk_apply : (Sym.t -> Term.t list -> Term.t)
                   -> Cnstrnt.t option -> Term.t -> Term.t list -> Term.t
(** [mk_apply sigma c a al] builds an application with range type [c] by
 applying term [a] to a list of argument terms [al].  In addition, it performs
 beta-reduction. The [sigma] argument is used to further simplify beta redeces. *)

val mk_abs : Term.t -> Term.t
(** [mk_abs a] lambda-abstracts [a]. All free occurrences in [a] of free variables 
 of the form [!0] are bound by this abstraction. *)


(** {Operators} *)

val sigma : Sym.apply -> Term.t list -> Term.t
(** Depending on the function symbol [f], [sigma f al] uses the constructors
 {!mk_apply} and {!mk_abs} to compute  the normal-form of applying [f] to the 
 arguments [al]. *)

val tau : (Term.t -> Cnstrnt.t) -> Sym.apply -> Term.t list -> Cnstrnt.t
(** [tau c f al] computes the constraint [i] if [f] is of the form [apply(Some(i))]
 and [al] is a unary list. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
(** [map f a] applies the term transformer [f] to each uninterpreted 
  subterm of [a] and rebuilds the term [a] by using the simplifying constructors
  [mk_apply] and [mk_abs]. *)
  
