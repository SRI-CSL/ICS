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

(** Slack equalities

  @author Harald Ruess
  @author N. Shankar

  A {b slack equality} set of equalities of the form [k = a].
*)

type t
  (** Abstract datatype for representing slack equalities *)


(** {6 Accessors} *)

val apply : t -> Term.t -> Term.t
  (** [apply s x] returns [b] if [x = b] is in [s], and 
    raises [Not_found] otherwise. *)

val find : t -> Term.t -> Term.t
  (** [find s x] returns [b] if [x = b] is in [s], and [x] otherwise. 
    For non-variable argument [a], [find s a] always returns [a]. *)

val to_list : t -> (Term.t * Term.t) list
  (** [to_list s] returns a list of pairs [(x, a)], where [x] is a
    variable and [a] a term for the equalities [x = a] in the 
    solution set [s]. *)

val pp : t Pretty.printer
  (** Pretty-printing of theory-specific solution set. *)


(** {6 Iterators} *)

val fold : (Term.t -> Term.t * Fact.justification option -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s e] applies [f x a] to all [x = a] in
    the solution set [s] in an unspecified order and
    accumulates the result. *)


(** {6 Predicates} *)

val mem : t -> Term.t -> bool
  (** [mem s x] holds iff [x = _] is in [s]. *)

val is_empty : t -> bool
  (** [is_empty s] holds iff [s] does not contain any equalities. *)

val eq : t -> t -> bool
  (** [eq s t] tests if the solution sets [s] and [t] are identical
    in the sense that the sets of equalities are stored at the same
    memory location. *)
  

(** {6 Manipulating solution sets} *)

val empty : t
  (** The [empty] solution set, which does not contain any equality. *)

val restrict : Term.t -> t -> t 
  (** [restrict s x] removes equalities [x = a] from [s]. *)

val update : Fact.equal -> t -> t
  (** [update e s] adds an equality [e] of the form [x = b] to [s], 
    possibly overwriting an equality [x = b'] in [s]. *)
