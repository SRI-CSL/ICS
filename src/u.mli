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

(** Congruence closure

  @author Harald Ruess
  @author N. Shankar

  A {i congruence closure} state represents the conjunction of 
  a set of equalities [x = f(x1,...,xn)] with [x], [xi] term variables and [f] 
  an uninterpreted function symbol. This set of equalities is 
  - {i injective} in that [x = a] and [y = a] implies [x = y], and 
  - {i functional} in that [x = a] and [x = b] implies [a = b].
*)

type t

val eq : t -> t -> bool
  (** [eq s1 s2] holds iff [s1] and [s2] are identical. Failure
    of this test does not necessarily imply that the states are
    not logically equivalent. *)


val pp : t Pretty.printer
  (** Pretty-printing a congruence-closure state. *)

val empty : t
  (** The empty congruence closure state. *)

val is_empty : t -> bool

val is_flat : Term.t -> bool
  (** [is_flat a] holds iff [a] is of the form [f(x1,...,xn)]
    with [f] an uninterpreted function symbol (see {!Sym.Uninterp})
    and all [xi] are term variables. *)


(** {6 Accessors} *)

val apply : t -> Term.t -> Term.t * Jst.t
  (** [apply s x] yields [(b, rho)] with [rho |- x = b]
    and [b] a {i flat} term, if [x = b] is in [s]. Otherwise,
    [Not_found] is raised. *)

val find : t -> Term.t -> Term.t * Jst.t
  (** [find s a] yields [(b, rho)] with [rho |- a = b]
    and [b] a {i flat} term, if [a = b] is in [s]. In
    particular, [a] is a variable. Otherwise, [(a, rho)] is
    returned with [rho |- a = a]. *)

val inv : t -> Term.t -> Term.t * Jst.t

val dep : t -> Term.t -> Term.Var.Set.t

val is_dependent : t -> Term.t -> bool
  (** [is_dependent s x] iff there is an [a] such that [x = a] in [s]. *)

val is_independent : t -> Term.t -> bool
  (** [is_independent s y] iff there are [x], [a] such that [x = a] in [s]
    and [y] is an argument variable of the flat term [a]. *)


(** {6 Updates} *)

val copy : t -> t

val name : Partition.t * t -> Jst.Eqtrans.t

val merge : Partition.t * t -> Fact.Equal.t -> unit
