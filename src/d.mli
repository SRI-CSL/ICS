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

(** Context for handling disequalities

  @author Harald Ruess
*)
  

type t
  (** Elements of type {!D.t} represent sets of variable disequalities. *)

val pp : t Pretty.printer
  (** Pretty-printing. *)

val eq : t -> t -> bool
  (** [eq s t] holds iff [s] and [t] are physically equal.
    If [eq s t] equals [false], then it is not necessarily
    true that [s] and [t] are not logically equivalent. *)


(** {6 Accessors} *)


val deq_of : t -> Term.Set.t Term.Map.t
  (** Return disequalities as bindings of the form [x |-> {y1,...,yn}].
    The interpretation of such a binding is the conjunction 
    [x <> y1 & ... & x <> yn] of all known disequalities for [x]. The
    bindings returned by [deq] are closed in that forall [x], [y] 
    such that [x |-> {...,y,...} ] then also [y |-> {....,x,....}] *)

val d : t -> Term.t -> (Term.t * Fact.justification option) list
  (** [disequalities s x] returns the maximal set of  disequalites [di] 
    of the form [x <> y] such that [x <> y] is represented in [s]. *)


(** {6 Recognizers} *)

val is_diseq: t -> Term.t -> Term.t -> bool
  (** Check if two terms are known to be disequal. *)


(** {6 Constructors} *)

val empty : t
  (** The empty disequality context. *)
 
val merge : Fact.equal -> t -> Term.Set.t * t
  (** [merge e s] propagates an equality [e] of the form [x = y]
    into the disequality context by computing a new disequality
    context which is equal to [s] except that every [x] has been
    replaced by [y]. Raises {!Exc.Inconsistent} if [x <> y] is
    already in [s]. *)

val add : Fact.diseq -> t -> Term.Set.t * t
  (** [add d s] adds a disequality [d] of the form
    [x <> y] to the disequality context [s]. As a side
    effect, both [x] and [y] are added to the set {!D.changed}. *)
