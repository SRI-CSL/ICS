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


(** Logical rules for manipulating contexts

  @author Harald Ruess
*)

type rule = Context.t -> Context.t

type 'a transform = Context.t * 'a -> Context.t * 'a

val merge : Fact.equal -> rule
  (** Merge variable equality. *)

val add : Fact.cnstrnt -> rule
  (** Add a constraint. *)

val diseq : Fact.diseq -> rule
  (** Add a disequality. *)

(** {6 Abstraction} *)

val abstract_equal : Fact.equal transform

val abstract_diseq : Fact.diseq transform

val abstract_cnstrnt : Fact.cnstrnt transform


(** {6 Tactics} *)

val propagate: Fact.equal -> rule
  (** [propagate e (p, u)] possibly deduces new equalities from the variable
    equality [e] of the form [x = y].  If both [z1 = select(u, j1)]
    and [z2 = update(a2,i2,k2)] are in the solution set [u], and if
    and [u = z2], [x = i2], [y = j1] are known to be true in the
    partitioning [p], then the equality [z1 = k2] is added to [p]. *)

val arrays_equal : Fact.equal -> Context.t -> Context.t
  (** [diseq d (p, u)] propagates a disequality [d] of the form [i <> j]. 
    If [z1 = select(upd, j')], [z2 = update(a,i',x)] are equalities in [u] and
    if [i = i'], [j = j'], [upd = z2] are equalities in the partitioning [p], then
    the variable equality [z1 = z3] is added to [p]. Either [z3 = select(a,j)] is
    already in [u] or [z3] is generated and the equality [z3 = select(a,j)] is
    added to [u]. *)

val arrays_diseq : Fact.diseq -> Context.t -> Context.t

val fme : bool ref

(* ... *)


(** {6 Confluence} *)

val close : rule

val maxclose : int ref




