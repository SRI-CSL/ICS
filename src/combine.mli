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

(** Combined inference system

  @author Harald Ruess
  @author N. Shankar 
*)

(** Inference system for the union of the theories
  - {!Th.u} of uninterpreted functions,
  - {!Th.la} of linear arithmetic,
  - {!Th.nl} of nonlinear multiplication,
  - {!Th.p} of products,
  - {!Th.cop} of coproducts,
  - {!Th.app} of combinatory logic,
  - {!Th.arr} of functional arrays,
  - {!Th.set} of propositional sets,
  - {!Th.bv} of bitvectors. *)


val sigma : Sym.t -> Term.t list -> Term.t
  (** Theory-specific canonizers. *)


val solve : Th.t -> Term.Equal.t -> Term.Subst.t
  (** Theory-specific solvers. *)


(** Combination of individual equality sets. *)
module E : sig

  type t
    (** Combined equality set. *)

    (** Projections to individual equality sets. *)
  val u_of : t -> U.S.t
  val la_of : t -> La.S.t
  val nl_of : t -> Solution.Set.t
  val p_of : t -> Solution.Set.t
  val cop_of : t -> Solution.Set.t
  val cl_of : t -> Solution.Set.t
  val arr_of : t -> Solution.Set.t
  val set_of : t -> Solution.Set.t

  val empty: t 
    (** The empty equality configuration. *)

  val copy : t -> t
 
  val is_empty: t -> bool
    (** Succeeds if all individual equality sets are empty. *)

  val eq : t -> t -> bool
    (** Identity test for two equality sets. Failure does
      imply that the argument equality sets are not logically equivalent. *)

  val pp : t Pretty.printer
    (** Pretty-printing of combined equality set. *)

  val pp_i : Th.t -> t Pretty.printer
    (** Pretty-printing of an individual equality set. *)

  val find : t * Partition.t -> Th.t -> Jst.Eqtrans.t
    (** [find s th x] is [a] if [x = a] is in the solution set for theory [th]
      in [s]; otherwise, the result is just [x]. *)

  val inv : t * Partition.t -> Jst.Eqtrans.t
    (** [inv s a] is [x] if there is [x = a] in the solution set for
      theory [th]; otherwise [Not_found] is raised. *)

  val dep : Th.t -> t -> Term.t -> Term.Var.Set.t
    (** [dep i s x] returns a set [ys] of variables such that
      [y] in [ys] iff there is an [x = a] in [s] with [y] a subterm of [a]. *)

  val diff : t -> t -> t
    (** [diff s1 s2] returns the equality set [s] such that an equality [e]
      is in [s] iff if it is in [s1] but not in [s2]. *)

end 


type t = E.t Infsys.Config.t
    (** Configurations [(g, e, p)] for the combined inference system consist
      - of a global input [g],
      - a combined euqality set [e], and
      - a variable partition [p]. *)


val process : Fact.t -> E.t * Partition.t -> E.t * Partition.t
  (** Given a starting configuration [({fct}, e, p)], process applies
    all rules of the combined inference system (except branching rules).
    The source and target configuration of [process] are {i equivalent},
    although the target configuration might contain internally generated
    variables not present in the source configuration. *)

val is_sat :  E.t * Partition.t -> (E.t * Partition.t) option
  (** [is_sat [c]] applies applicable {i branching rules} until 
    it finds a satisfiable configuration [d] (with empty input) for 
    which no branching rule is applicable; in this case [Some(d)] is returned.
    Otherwise, all branching states are unsatisfiable, and [None] is returned. *)


val dom : E.t * Partition.t -> Term.t -> Dom.t * Jst.t
(** [dom p a] returns a domain [d] for a term [a] together with
  a justification [rho] such that [rho |- a in d].  This function
  is extended to arithmetic constraints using an abstract domain
  interpretation. Raises [Not_found] if no domain constraint is found. *)

val can : E.t * Partition.t -> Jst.Eqtrans.t
  (** Given an equality set [e] and a partition [p] as obtained
    from processing, [can (e, p) a] returns a term [b] equivalent
    to [a] in [(e, p)] together with a justification [rho].  If
    no further branching is applicable on [(e, p)], then [can (e, p)]
    is a canonizer in the sense that [can (e, p) a] is syntactically
    equal to [can (e, p) b] iff the equality [a = b] follows from [(e, p)]
    in the supported union of theories. *)

val simplify : E.t * Partition.t -> Atom.t -> Atom.t * Jst.t
  (** Simplification of atoms in a context [(e, p)] by canonizing
    component terms using {!Combine.can}.  Simplification is
    incomplete in the sense that there are 
    - valid atoms [a] in [(e, p)] which do not reduce to {!Atom.mk_true},
    - unsatisfiable atoms [a] in [(e, p)] which do not reduce to {!Atom.mk_false}. *)


val cheap : bool ref
  (** If [cheap] is set, {!Combine.simplify} only performs cheap 
    simplifications for disequalities and inequalities. Setting [cheap]
    to [false] make {!Combine.simplify} more complete. *)

val gc : t -> unit
  (** [gc c] garbage collects internal variables in configuration [c]
    as introduced by {!Combine.process}. *)


val maximize : E.t * Partition.t -> Jst.Eqtrans.t
  (** [maximize c a] returns either
    - [(b, rho)] such that [b+] is empty and [rho |- la => a = b], or
    - raises {!La.Unbounded} if [a] is unbounded in the linear 
    arithmetic [la] equality set of [c]. *)

val minimize : E.t * Partition.t -> Jst.Eqtrans.t
  (** Minimize is the dual of maximize. *)


val model : E.t * Partition.t -> Term.t list -> Term.t Term.Map.t
  (** Model construction. Experimental. *)
