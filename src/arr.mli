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

(** Functional arrays decision procedure

  @author Harald Ruess
  @author N. Shankar

  A context consists of equalities of the form [x = b] with [x] a
  variable and [b] a flat array term with variables as arguments:
  - [create(a)] for creating a constant array with 'elements' [a]
  - [a[i:=x]] for updating array [a] at position [i] with [x]
  - [a[j]] for selection the value of array [a] at position [j]

  The extensional array theory {!Th.arr} is defined in module {!Funarr}.
  
  Right-hand sides of context equalities [x = a] are kept in 
  canonical form.  That is, if the variable equality [y = z]
  has been merged using {!Arr.merge}, then the noncanonical [y]
  is not appearing on any right-hand side.

  Forward chaining is used to keep configurations {i confluent}.
  - (1) [u = a[i:=x]] ==> [x = u[i]],
  - (2) [i<>j], [u = a[i:=x]] ==> [u[j] = a[j]],
  - (3) [i<>j], [v = a[i:=x][j:=y]] ==> [v = a[j:=y][i:=x]],
  - (4) [u = a[i:=y][i:=x]] ==> [u = a[i:=x]],
  - (5) [u = create(a)[j]] ==> [u = a]

  Here, [i<>j] are the known disequalities in a variable partition
  (see {!Partition.t}).
*)

type t
  (** Representing sets of equalities of the form [x = a]
    with [a] restricted to {i flat} array terms, that is [a]
    is an term application of the form [f(x1,...,xn)] with [f]
    a function symbol in the theory {!Th.arr} of arrays and
    all arguments are variables. *)

val eq : t -> t -> bool
  (** [eq s1 s2] succeeds if [s1] and [s2] are identical. If
    [eq s1 s2] holds, then [s1] and [s2] are logically equivalent. *)

val pp : t Pretty.printer
  (** Pretty-printing an array equality set} *)

val empty : t
  (** The empty array equality set. *)

val is_empty : t -> bool
  (** [is_empty s] succeeds iff [s] represents the empty equality set. *)

val fold : (Term.t -> Term.t * Jst.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s e] applies [f x (a, rho)] for each [x = a] with justification
    [rho] in [s] and accumulates the result starting with [e]. The order of
    application is unspecified. *)

val apply : t -> Jst.Eqtrans.t
  (** [apply s x] returns [a] if [x = a] is in [s]; otherwise
    [Not_found] is raised. *)

val find : t -> Jst.Eqtrans.t
  (** [find s x] returns [a] if [x = a] is in [s], and [x] otherwise. *)

val inv : t -> Jst.Eqtrans.t
  (** [inv s a] returns [x] if [x = a] is in [s]; otherwise
    [Not_found] is raised. *)

val dep : t -> Term.t -> Term.Var.Set.t
  (** [dep s y] returns the set of [x] such that [x = a] in [s]
    and [y] occurs in [a]. *)
  
val is_dependent : t -> Term.t -> bool
  (** [is_dependent s x] holds iff there is an [a] such that [x = a] in [s]. *)
  
val is_independent : t -> Term.t -> bool
  (** [is_independent s y] holds iff [y] occurs in some [a] 
    such that [x = a] in [s]. *)

type config = Partition.t * t
    (** A {i configuration} consists of a pair [(p, s)] with
      [p] a partitioning and [s] an array equality set. *)

val replace : config -> Jst.Eqtrans.t
  (** [replace (p, s) a] returns a flat array term if there is
    a [y] with [x] equal [y] modulo [p] such that [y = a] in [s]. *)

val abstract : config -> Jst.Eqtrans.t
  (** [absrtract c a] returns a canonical variable which
    is equivalent in a possibly extended configuration [c]. *)

val copy : t -> t
  (** The update functions {!Arr.can}, {!Arr.merge},
    and {!Arr.dismerge} {b destructively} update equality
    sets. The function [copy s] can be used to protect state [s]
    against these updates. *)

val can : config -> Jst.Eqtrans.t
  (** For a pure array term [a], [can (p, s) a] returns a canonical 
    term [b] together with a justification of [a = b]. *)

val name : config -> Jst.Eqtrans.t
  (** For a {!Th.arr}-pure term [a], [name (p, s) a] returns
    a variable [y] equal to [a] in the (destructively) updated 
    configuration [(p, s)]. *)


val merge : config -> Fact.Equal.t -> unit
  (** [merge (p, s) e] conjoins an array solution
    set [s] with an equality [e] over {i flat} array terms.
    If [e] conjoined with [s] and [p] is {i inconsistent},
    then {!Jst.Inconsistent} is raised.  Besides 
    {i destructively} updating [s], all generated variable 
    equalities and disequalities are propagated into the 
    partitioning [p].  Notice, however, that not all variable
    equalities implied by [e] conjoined with
    [p] and [s] are generated. Also, a full case split over
    the variable pairs generated by {!Arr.split} is needed
    in general to detect every inconsistency. *)

val process_equal : config -> Fact.Equal.t -> unit
  (** For a pure array equality [e] of the form [a = b], 
    [process c e] merges the variable equality [x = y] using
    {!Arr.merge}, where [x] ([y]) is the canonical form of [a] ([b])
    as obtained by canonization using {!Arr.can}. *)

val process_diseq : config -> Fact.Diseq.t -> unit

val dismerge : config -> Fact.Diseq.t -> unit
  (** [dismerge (p, s) d] closes the array context [s] under
    applications of rules [(2)], [(3)] with the disequality [d]. *)

val split : config -> Term.t * Term.t
  (** [split (p, s)] generates a case splits necessary
    to make the procedure complete for the theory of power products
    The result is a pair of equalities of the form [(x, 0); (y, 1)]
    for representing the disjunction [x = 0] OR [y = 1]. *)






