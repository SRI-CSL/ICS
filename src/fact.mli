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

(** A {b fact} is either 
  - an equality [a = b] between terms [a] and [b], 
  - a disequality [a <> b] between terms [a], [b], or 
  - a membership constraint of the form [a in c], where [a] is a term and [c]
    is a constraint of type {!Cnstrnt.t}.
  In addition, every fact includes an optional {b justification} in terms
  of facts sufficient to prove the fact at hand.

  @author Harald Ruess
*)


(** {6 Facts} *)

type equal
  (** The type of equality facts. *)

type diseq
  (** The type of disequality facts. *)

type cnstrnt
  (** The type of cnstrnt constraints. *)


(** {6 Injections} *)

type t
  (** Elements of this type are composed of equality facts,
    disequality facts, and constraint facts. *)
  
val of_equal : equal -> t
val of_diseq : diseq -> t
val of_cnstrnt : cnstrnt -> t


(** {6 Justifications} *)

type rule = string
    (** Currently only a string, but will eventually include a complete
      rule calculus. *)

type justification =
  | Axiom
  | Rule of rule * justification list
      (** A justification is either 
	- [Axiom] which labels a fact to be an {i axiom}, or
	- [Rule(rl, jl)] which asserts a succedent fact to be derivable
          by applying rule [rl] with antecedents [jl]. *)

val mk_axiom : justification option
  (** Creating an optional axiom justification. *)

val mk_rule : rule -> justification option list -> justification option
  (** Given a rule [rl], and a list of justifications, which are all of
    the form [Some(jk)] for [k] between [1] and [n], 
    return [Some(rl, [j1;...;jn])]; otherwise return [None]. *)


(** {6 Constructors} *)

val mk_equal : Term.t -> Term.t -> justification option -> equal
  (** [mk_equal a b j] constructs a fact for the equality [a = b]
    with justification [j]. *)

val mk_diseq : Term.t -> Term.t -> justification option -> diseq
  (** [mk_diseq a b j] constructs a fact for the disequality [a <> b]
   with justification [j]. *)


val mk_cnstrnt : Term.t -> Interval.t -> justification option -> cnstrnt
  (** [mk_cnstrnt a c j] constructs a fact for the membership [a in c]
    with justification [j]. *)



(** {6 Accessors} *)

val d_equal : equal -> Term.t * Term.t * justification option
  (** [d_equal e] deconstructs equality fact [e] into [(a, b, j)]
    if [e] had been constructed using [mk_equal a b j]. *)

val d_diseq : diseq -> Term.t * Term.t * justification option
  (** [d_diseq e] deconstructs disequality fact [d] into [(a, b, j)]
    if [d] had been constructed using [mk_diseq a b j]. *)

val d_cnstrnt : cnstrnt -> Term.t * Interval.t * justification option
  (** [d_cnstrnt c] deconstructs constraint fact [c] into [(a, c, j)]
    if [c] had been constructed using [mk_cnstrnt a c j]. *)


(** {6 Pretty-printing} *)

val pp : t Pretty.printer

val pp_equal : equal Pretty.printer
val pp_diseq : diseq Pretty.printer
val pp_cnstrnt : cnstrnt Pretty.printer


(** {6 Sets} *)

module Equalset : (Set.S with type elt = equal)
  (** Set of equality facts. *)
