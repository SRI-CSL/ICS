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

(** Inference system for canonizable, ground confluent theories.

  @author Harald Ruess

  A {i canonizable and ground confluent} theory [th] is specified 
  by means of a
  - replacement [map f a] for replacing uninterpreted 
  subterms of [a] with [f a] and canonizing the result,
  - a theory-specific canonizer,
  - and a [deduce] function for making equality sets ground confluent.
*)

module type T = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val sigma : Sym.t -> Term.t list -> Term.t
end

module type DEDUCE = sig
  type t
  val of_equal : Fact.Equal.t -> Partition.t * t -> Fact.Equal.t list
  val of_var_equal : Fact.Equal.t -> Partition.t * t -> Fact.Equal.t list
  val of_var_diseq : Fact.Diseq.t -> Partition.t * t -> Fact.Equal.t list
  val disjunction : Partition.t * t -> Fact.t list
end

module Trace(D: DEDUCE): (DEDUCE with type t = D.t)

module type EQS = sig
  module S : Solution.SET0
  type t = S.t
  val find : Partition.t * t -> Jst.Eqtrans.t
  val inv : Partition.t * t -> Jst.Eqtrans.t
  val can : Partition.t * t -> Jst.Eqtrans.t
end 

(** Constructing an inference system from a canonizable
  and ground confluent theory. *)
module E(Can: T): EQS

module Make(Can: T)
           (Deduce: (DEDUCE with type t = E(Can).t))
: 
(Infsys.IS with type e = E(Can).t)
