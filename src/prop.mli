(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** {i Open inference system for propositional formulas.}

    This module provides an open inference system for propositional
    formulas. Truth values of literals are propagated between the
    propositional inference system and its interface.

    @author Harald Ruess *)

(** {i Propositional formulas.} *)
module type PROP = sig
  (** Representation of propositional variables. *)
  type var

  (** Representation of propositional formulas. *)
  type t

  val mk_true : t
  (** A trivially valid propositional formula. *)

  val mk_false : t
  (** A trivially unsatisfiable propositional formula. *)

  val mk_posvar : var -> t
  (** [mk_posvar x] constructs a propositional formula for the propositional
      variable [x]. *)

  val mk_negvar : var -> t
  (** [mk_negvar x] constructs a propositional formula for the negated
      propositional variable [x]. *)

  val mk_conj : t -> t -> t
  (** [mk_conj p q] returns the conjunction of propositional formulas [p],
      [q]. *)

  val mk_disj : t -> t -> t
  (** [mk_disj p q] returns the disjunction of propositional formulas [p],
      [q]. *)

  val union : var -> var -> t -> t
  (** [union x y p] is equivalent to [(x <=> y) & p]. *)

  val separate : var -> var -> t -> t
  (** [separate x y p] is equivalent to [(x # y) & p] with [#] exclusive or. *)

  val cofactor_pos : t -> var -> t
  (** [cofactor_pos p x] is equivalent with [p'] obtained by substituting
      every occurrence of [x] by [mk_true]. In particular,
      [not(occurs x p')]. *)

  val cofactor_neg : t -> var -> t
  (** [cofactor_neg p x] is equivalent with [p'] obtained by substituting
      every occurrence of [x] by [mk_false]. In particular,
      [not(occurs x p')]. *)

  val is_valid : t -> bool
  (** [is_valid p] holds iff propositional formula [p] is valid. *)

  val is_unsat : t -> bool
  (** [is_unsat p] holds iff propositional formula [p] is valid. *)

  val occurs : var -> t -> bool
  (** [occurs x p] holds iff [x] occurs in [p]. *)

  val and_elim : t -> var list * var list * t
  (** [and_elim] extracts implied literals in a propositional formula. In
      particular, [and_elim b] returns
      [(\[x{1};...;x{n}\],\[y{1};...;y{m}\],b')] with [b => x{i}],
      [b => y{j}] for [i=1,...,n], [j=1,...,m], and [b'] is such that none
      of the [x{i}] or [y{j}] occurs in [b'] and
      [b <=> b' & x{1} & ... & x{n} & y{1} & ... y{m}]. *)
end

(** {i Interface of propositional inference system.} Configurations of the
    interface inference system are equivalent to [(P, N)] with [P] a
    conjunction of propositional variables and [N] a conjunction of negated
    propostional variables. *)
module type INTERFACE = sig
  (** Representation of propositional variables. *)
  type var

  val valid : var -> unit
  (** [valid x] adds a propositional variable [x] to [P]. It throws an
      exception if the resulting configuration is inconsistent. *)

  val unsat : var -> unit
  (** [unsat x] adds a propositional variable [x] to [N]. It throws an
      exception if the resulting configuration is inconsistent. *)
end

(** {i Propositional inference system.} Configurations of the propositional
    inference systems are propositional formulas. The inference system
    progresses by updating a {i current configuration}. As an invariant, the
    current configuration is always satisfiable, and it {i reduced} in that
    it does not contain propositional variables [x] if it implies either [x]
    or its negation [~x]. *)
module type INFSYS = sig
  (** Representation of propositional variables. *)
  type var

  (** Representation of configurations, which are propositional formulas. *)
  type t

  val empty : t
  (** The empty configuration which is equivalent to the trivially valid
      formula. *)

  val initialize : t -> unit
  (** [initialize p] initializes the current state with the propositional
      formula [p]. *)

  val reset : unit -> unit
  (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the configuration has not been changed since
      the latest [initialize] or [reset]. *)

  val is_valid : unit -> bool
  (** Current configuration is valid. *)

  val current : unit -> t
  (** [current()] returns the current configuration. *)

  (** Exception [Unsat] is raised whenever the current configuration becomes
      unsatisfiable. *)
  exception Unsat

  val process_conjoin : t -> unit
  (** [process_conjoin p] conjoins [p] with the current configuration. *)

  val process_union : var -> var -> unit
  (** [process_union x y] conjoins the equivalence [x <=> y] with the
      current configuration. *)

  val process_separate : var -> var -> unit
  (** [process_separate x y] conjoins the exclusive or [x # y] with the
      current configuration. *)

  val process_sub : var -> var -> unit
  (** [process_sub x y] conjoins the implication [x => y] with the current
      configuration. *)

  val propagate_valid : var -> unit
  (** Whenever validity of the propositional variable [x] is derived in the
      interface, [propagate_valid x] needs to be called. This operation
      replaces every occurrence of [x] in the current configuration with the
      trivially valid formula [mk_true]. *)

  val propagate_unsat : var -> unit
  (** Whenever unsatisfiability of the propositional variable [x] is derived
      in the interface, [propagate_unsat x] needs to be called. This
      operation replaces every occurrence of [x] in the current
      configuration with the trivially unsatisfiable formula [mk_false]. *)
end

(** Construction of a closed inference system from a representation of
    propositional variables [Var], propositional formulas [Prop], and an
    interface inference system [I]. *)
module Make (Var : sig
  type t
end)
(Prop : PROP with type var = Var.t)
(_ : INTERFACE with type var = Var.t) :
  INFSYS with type var = Var.t and type t = Prop.t
