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

(** Inference system for Shostak theories.

  @author Harald Ruess
*)


(** A Shostak theory [th] is specified by means of a
  - replacement [map f a] for replacing uninterpreted 
  subterms of [a] with [f a] and canonizing the result, and
  - a {i solver} [solve]. 
  - an extended {i canonizer} [map f a] for replacing uninterpreted
  positions [x] of [a] with [f b] followed by canonization in the
  given theory, and
  - a {i branching} function [disjunction]. If [disjunction] always returns [Not_found],
  then [th] is also said to be a {i convex} Shostak theory. *)
module type T = sig
  val th : Th.t
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
  val solve : Fact.Equal.t -> Fact.Equal.t list
  val disjunction : Fact.Equal.t -> Clause.t
end


module Make(Sh: T): (Infsys.EQ with type e = Solution.Set.t)
  (** Constructing an inference system from a Shostak theory [Th].
    For a description of the correctness statement of these inference
    rules, see module {!Infsys}.

    As an invariant, equality sets for representing contexts
    of Shostak theories are {i ordered} equalities of the 
    form [x = a] with [x] a variable and [a] a nonvariable, 
    [Sh.th]-pure term.  In addition, these equality sets are in
    - {i solved form}, that is, there are no [x = a], [y = b]
    with [x] a variable in [b].
    - {i canonical}, that is, if [x = y] has been propagated
    using the [propagate] rule, then the {i noncanonical} [x] does 
    not appear in the equality set.  Also, right-hand sides are
    always kept in canonical form w.r.t to the given theory canonizer [map].

    In case of {i convex} Shostak theories, [disjunction] always fails,
    and there is no branching. 

    In case of {i incomplete} Shostak theories, [solve] might raise
    {!Exc.Incomplete} on an equality [a = b].  Now, [a], [b] are
    named apart and the corresponding variable equality is merged.
    Notice, that incomplete solvers might lead to an incompleteness in
    the inference procedure.
 *)
