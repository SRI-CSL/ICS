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

(** {i Shostak inference system}

    Given a Shostak equality theory, construct an {i open inference system}

    - for online processing of term equalities and
    - propagating all implied variable equalities.

    @author Harald Ruess *)

(** {i Term variables}. Input signature for {!Shostak.Make}. *)
module type VAR = sig
  (** Representation of variables. *)
  type t

  val equal : t -> t -> bool
  (** Equality test on variables. *)

  val compare : t -> t -> int
  (** [compare x y] is [0] iff [equal x y] holds. Furthermore,
      [compare x y < 0] iff [compare y x > 0]. Thus, [compare] induces a
      total ordering [<<] on variables with, say, [x << y] if
      [compare x y < 0]. *)

  val hash : t -> int
  (** Nonnegative hash value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing a variable on given formatter. *)

  val fresh : unit -> t
  (** Generating a fresh variable that has not been seen before in the given
      context. *)
end

(** {i Shostak theory}. {i Terms} for a {i Shostak} equality theory [T]. We
    write [T |= s = t] when [s = t] is valid in theory [T]. Input signature
    for {!Shostak.Make}. *)
module type TERM = sig
  (** Representation of variables. *)
  type var

  (** Representation of terms. *)
  type t

  val equal : t -> t -> bool
  (** [equal s t] holds iff [T |= s = t]. That is, [equal s t] solves the
      {i word problem} for [T]. *)

  val diseq : t -> t -> bool
  (** [diseq s t] holds iff [T |= s <> t]. *)

  val compare : t -> t -> int
  (** [compare x y] is [0] iff [equal x y] holds. Furthermore,
      [compare x y < 0] iff [compare y x > 0]. Thus, [compare] induces a
      total ordering [<<] on variables with, say, [x << y] if
      [compare x y < 0]. *)

  val hash : t -> int
  (** Nonnegative hash value for a term. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-printing a term on a given formatter. *)

  val is_var : t -> bool
  (** [is_var t] iff [t] represents a {i variable term}. *)

  val of_var : var -> t
  (** [of_var x] constructs a {i variable term} [t] such that [to_var t]
      equals [x]. *)

  val to_var : t -> var
  (** For variable terms [t], [to_var t] returns a variable [x] with
      [of_var x] equals [t]. *)

  val iter : (var -> unit) -> t -> unit
  (** [iter f t] applies [f x] for each variable [x] occuring in [t]. The
      order of application is unspecified.*)

  val map : (var -> t) -> t -> t
  (** [map f t] replaces each variable [x] in [t] with [f x] and puts the
      resulting term in {i canonical} form [t']. If [T |= f x = x] then,
      [T |= map f t = t]. Define the canonizer [can t] for Shostak theory
      [T] as [map id] (with [id x = x] the identity), then [can s] equals
      [can t] iff [T |= s = t]. *)

  val for_all : (var -> bool) -> t -> bool
  (** [for_all p t] holds iff [p x] holds for all variables [x] in [t]. *)

  val occurs : var -> t -> bool
  (** [occurs x t] iff variable [x] occurs in term [t]. *)

  val choose : t -> var
  (** [choose t] chooses an arbitrary variable [x] in [t]. If there is no
      such variable, [Not_found] is raised. *)

  (** Representation of term {i substitutions} with binding [x |-> t] where
      [x] is a variable and [t] a term. In particular, none of the domain
      variables occurs in any of the codomain terms. *)
  module Subst : Subst.S with type var = var and type trm = t

  (** Exception raised by [solve]. *)
  exception Unsat

  val solve : t -> t -> Subst.t
  (** For terms [s, t], the solver [solve s t]

      - raises [Unsat] iff [T |= s <> t]; otherwise, it
      - returns a substitution [x{1} |-> t{1},...,x{n} |-> t{n}] with
        variable [x{i}] occurs in [s] or [t] and [T |= s = t] iff
        [T |= x{1} = t{1} & ... & x{n} = t{n}]. In particular, the [t{j}]
        might contain variables not in [s] or [t]. *)
end

(** {i Variable partitioning for Shostak inference system.}
    {i Variable partition.} A variable partition has configurations
    equivalent to a finite set of variable equalities [E] and a finite set
    of variable disequalities [D]. We say that [x] and [y] are equivalent
    modulo [E] if [E |= x = y] in the theory of pure identity. *)
module type V = sig
  (** Representation of variables. *)
  type var

  val find : var -> var
  (** [find x] returns the {i canonical} representative of the equivalence
      class modulo [E] containing [x]. *)

  val canonical : var -> bool
  (** [canonical x] holds iff [x] is the canonical representative of the
      equivalence class modulo [E]. *)

  val equal : var -> var -> bool
  (** [equal x y] iff [x] and [y] are equivalent modulo [E]. *)

  val diseq : var -> var -> bool
  (** [diseq x y] iff [E, D |= x <> y] in the theory of pure identity. *)

  val union : var -> var -> unit
  (** [union x y] adds an equality [x = y] to the current set [E] of
      equalities such that the updated equality configuration is equivalent
      to [{x = y}, E]. *)

  val separate : var -> var -> unit
  (** [separate x y] adds an equality [x <> y] to the current set [E] of
      equalities such that the updated disequality configuration is
      equivalent to [{x <> y}, D]. *)
end

(** {i Shostak inference system}. Inference system for online processing of
    equalities over terms of a Shostak theory.

    A {i Shostak configuration} [S] is a set of bindings
    [{x{1} |-> t{1},...,x{n} |-> t{n}}] with [x{i}] variables and [t{j}]
    terms such that

    - [x{i} /= x{j}] for [i /= j]
    - [x{i}] does not occur in [vars({t{1},...,t{n}})].

    Such a configuration [S] is logically equivalent to the conjunction of
    the equalities in [{x{1} = t{1},...,x{n} = t{n}}].

    The environment variable partitioning has configurations equivalent to a
    pair [(E, D)] of variable equalities and disequalities as described in
    {!Shostak.V}.

    The configuration [(E, D, S)] is {i confluent} if [S] contains only
    canonical variables modulo [S]. Notice that, in general, not all
    disequalities implied by [S] are propagated into [D].

    The inference system works by applying {i equivalence-preserving} (in
    Shostak theory [T]) transformations of {i current configurations} *)
module type INFSYS = sig
  (** Representation of variables. *)
  type var

  (** Representation of terms. *)
  type trm

  (** Representation of configurations which consist of finite sets of
      solved equality sets (see [config] below). *)
  type t

  (** Representation of configurations as substitutions. *)
  module Subst : Subst.S with type var = var and type trm = trm

  val config : unit -> Subst.t
  (** Return the current configuration. *)

  val pp : Format.formatter -> unit
  (** Print a configuration on the given formatter. *)

  val can : trm -> trm
  (** [can t] returns a term [t'] with [S,E |= t = t'] such that

      - every variable in [t'] is canonical wrt [E],
      - [t'] does not contain any domain variables in [S].

      It is the case that, [s] and [t] are [Term.equal] iff [E, S |= s = t]
      in the given Shostak theory [T]. *)

  val find : var -> trm
  (** [find x] returns [t] if there is a binding [x |-> t] in the current
      configuration [S]; otherwise, [Not_found] is raised. *)

  val inv : trm -> var
  (** [inv t] returns [x] if there is a binding [x |-> t] in the current
      configuration [S]; otherwise, [Not_found] is raised. *)

  val dom : var -> bool
  (** [dom x] holds iff there is a binding [x |-> t] in the current
      configuration. *)

  val cod : var -> bool
  (** [cod y] holds iff there is a binding [x |-> t] in the current
      configuration with [y] occurs in [t]. *)

  val local : var -> bool

  val empty : t
  (** The empty configuration [S] with no bindings. *)

  val is_empty : unit -> bool
  (** [is_empty()] holds iff the current configuration [S] is equivalent to
      the [empty] configuration. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current configuration [S] has been
      unchanged since the latest [initialize] or [reset]. *)

  val initialize : t -> unit
  (** [initialize s] sets the current configuration [S] to [s]. *)

  val reset : unit -> unit
  (** [reset()] is synonymous with [initialize empty]. *)

  val current : unit -> t
  (** Return the current configuration [S]. *)

  val canonical : trm -> bool
  (** [canonical t] iff every variable in [t] is canonical wrt to the
      variable equalities [E] and [t] does not contain any domain variable
      in [S]. *)

  val diseq : var -> var -> bool
  (** If the current state is {i confluent}, then [diseq x y] holds iff
      [S |= x <> y] in the Shostak theory [T], with [S] the current
      configuration. *)

  val process_eq : trm -> trm -> unit
  (** If the current state is {i confluent}, then [process_eq s t] adds an
      equality [s = t] to the current configuration or raises [Unsat] if the
      context becomes unsatisfiable. Notice that not all implied
      disequalities are propagated proactively to [D]. *)

  val alias : trm -> var
  (** If the current state is {i confluent}, then [alias t] returns a
      variable [x] such that [S, E |= x = t] in a possibly updated
      configuration. *)

  val propagate : var -> var -> unit
  (** If variables [x], [y] are equal modulo [E] with [x] non-canonical and
      [y] canonical, then [propagate x y] updates the current configuration
      by means of equivalence-preserving transformations such that [S] does
      not contain [x] any more.

      This function needs to be called until the resulting configuration is
      {i confluent} (which is, for example, a precondition of [process_eq]). *)

  val confluent : unit -> bool
  (** A configuration [S] is {i confluent} if all variables in [S] are
      canonical wrt. to [E]. *)

  val normalize : unit -> unit
  (** Normalizes some internal indices and possibly the representation of
      [S]. Does not update [(E, D)]. *)

  val close : unit -> unit
  (** In general, not all disequalities [x <> y] implied by [S] are
      propagated to [D]. These propagation steps are performed by explicitly
      calling [close()]. It assumes that the configuration is {i confluent}. *)
end

(** {i Closed Shostak inference system}. Construct a closed Shostak
    inference system from an implementation [Var] for variables, [Term] for
    terms with [Var.t] variables and the environment variable partitoning
    [V]. *)
module Make
    (Var : VAR)
    (Term : TERM with type var = Var.t)
    (V : V with type var = Var.t) :
  INFSYS with type var = Var.t and type trm = Term.t
