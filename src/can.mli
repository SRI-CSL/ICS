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
*)

type config = Partition.t * Solution.Set.t 

(** A {i canonizable and ground confluent} theory [th] is specified 
  by means of a
  - replacement [map f a] for replacing uninterpreted 
  subterms of [a] with [f a] and canonizing the result,
  - a theory-specific canonizer;
  - chainings functions [of_var_equal], [of_var_diseq], [of_var_diseq] 
  for keeping equality sets ground confluent by triggering inference rules
  on pure equalities, variable equalities, and variable disequalities, respectively; and
  - a branching function [disjunction]. *)
module type T = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val sigma : Sym.t -> Term.t list -> Term.t
  val of_equal : Fact.Equal.t -> config -> Fact.Equal.t list
  val of_var_equal : Fact.Equal.t -> config -> Fact.Equal.t list
  val of_var_diseq : Fact.Diseq.t -> config -> Fact.Equal.t list
  val disjunction : config -> Clause.t
end


module Make(Can: T): (Infsys.EQ with type e = Solution.Set.t)
  (** Constructing an inference system from the specification [Can]
    of a canonizable and ground confluent theory. *)


(** Operations for canonizable theories. *)
module type OPS = sig
  val is_flat : Term.t -> bool
  val is_pure : Term.t -> bool
  val find : Partition.t * Solution.Set.t -> Jst.Eqtrans.t
  val inv : Partition.t * Solution.Set.t -> Jst.Eqtrans.t
end 

module Ops(Can: T): OPS
