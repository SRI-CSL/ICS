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

(** Solution sets.

  @author Harald Ruess
  @author N. Shankar

  A {b solution set} for theory [th] is a set of equalities of the 
  form [x = a], where [x] is term variable and [a] is a [th]-pure
  term application. For each such equality, a {i justification} [rho] 
  of type {!Jst.t} is maintained.

  As an invariant, solution sets [s] are kept in {i functional} form, 
  that is, if [x = a] and [x = b] in [s], then [a] is identical with [b]. 
  In addition, solution sets are {i injective}, that is, [x = a] and [y = a] 
  are not in a solution set for [x <> y].

  An equality set might have additional {i indices}. These are
  sets of dependent variables [x = a] such that [a] satisfies
  a specified constraint.

  A {i constant index} is special in that disequalities [x <> y]
  are generated for [x = c], [y = d] and [c], [d] two disequal
  constants.
*)


(** Abstract interface of an equality set. *)
module type SET = sig
  type t
  val theories : Th.t list
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val find : Th.t -> Partition.t * t -> Jst.Eqtrans.t
  val inv : Th.t -> Partition.t * t -> Jst.Eqtrans.t 
  val mem : Term.t -> t -> bool 
  val fold : (Fact.Equal.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (Fact.Equal.t -> unit) -> t -> unit
end

module Make(T: Solution.TH): SET
  (** Construct an equality set from a theory specification. *)

module Cross(Left: SET) (Right: SET): (SET  with type t = Left.t * Right.t)
  (** Cross product of two equality sets. *)


