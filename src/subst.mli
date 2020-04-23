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

(** {i Term substitutions}

    This module provides operators for constructing and applying term
    substitutions.

    @author Harald Ruess *)

(** {i Variables}. Used as input signature for {!Subst.Make}. *)
module type VAR = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

(** {i Terms}. Used as input signature for {!Subst.Make}. *)
module type TRM = sig
  (** Representation of terms. *)
  type t

  val equal : t -> t -> bool
  (** Syntactic term equality. *)

  val compare : t -> t -> int
  (** Term comparison with [compare s t] equals [0] iff [equal s t] holds,
      and [compare s t] is negative iff [compare t s] is positive. *)

  val hash : t -> int
  (** Nonnegative hash values for terms. *)

  val pp : Format.formatter -> t -> unit
  (** Prints a term on the given formatter. *)

  (** Representation of variables. *)
  type var

  val of_var : var -> t
  (** [of_var x] constructs a term variable from the variable [x]. *)

  val iter : (var -> unit) -> t -> unit
  (** [iter f t] applies [f x] for all variables occurring in [t]. The order
      of application is unspecified. *)

  val map : (var -> t) -> t -> t
  (** [map f t] replaces each [x] occurring in [t] with [f x]. *)
end

(** {i Term substitutions.} A {i term substitution} is a map with a finite
    set [{x{1} |-> t{1},...,x{n} |-> t{n}}] with [x{i}] pairwise disjoint
    variables and [t{j}] terms such that no [x{i}] occurs in any of the
    [t{j}]. *)
module type S = sig
  (** Representation of variables. *)
  type var

  (** Representation of terms *)
  type trm

  (** Representation of term substituions. *)
  type t

  val pp : Format.formatter -> t -> unit
  (** Printing a term substitution on the given formatter. *)

  val equal : t -> t -> bool
  (** [equal rho tau] holds for two substitutions [rho] and [tau] iff they
      contain the same bindings. *)

  val apply : t -> trm -> trm
  (** [apply rho t] replaces all domain variables [x] of [rho] with [t] if
      there is a binding [x |-> t] in [rho]. *)

  val lookup : t -> var -> trm
  (** [lookup rho x] returns [t] if there is a binding [x |-> t] in [rho];
      otherwise the exception [Not_found] is raised. *)

  val inv : t -> trm -> var
  (** [inv rho t] returns [x] if there is a binding [x |-> t] in [rho];
      otherwise the exception [Not_found] is raised. *)

  val dom : var -> t -> bool
  (** [dom x rho] holds iff [x] is a {i domain} variable of the substitution
      [rho]; that is, there is a binding [x |-> t] in [rho]. *)

  val empty : unit -> t
  (** [empty()] is the substitution with an empty set of bindings. *)

  val is_empty : t -> bool
  (** [is_empty rho] iff substitution [rho] does not contain any bindings. *)

  val singleton : var -> trm -> t
  (** [singleton x t] represents the singleton substitution with only one
      binding [x |-> t]. *)

  val copy : t -> t
  (** [copy rho] returns a substitution [rho'] equal to [rho]; updates on
      [rho] do not affect this substitution [rho']. Copying is {i lazy} in
      the sense that initially only a constant amount of memory needs to be
      allocated. *)

  val fuse : t -> var -> trm -> unit
  (** [fuse y t rho] replaces all bindings of the form [x |-> s\[y\]] with
      [x |-> s\[t\]]. *)

  val add : t -> var -> trm -> unit
  (** For [x] not in the domain of [rho], [add rho x t] adds the binding
      [x |-> t] to [rho] and replaces all binding of the form [z |-> s\[x\]]
      with [z |-> s\[t\]]. *)

  val update : t -> var -> trm -> unit

  val remove : t -> var -> unit
  (** [remove rho x] removes a binding of the form [x |-> t] from [rho]. *)

  val compose : t -> t -> unit
  (** For [rho] and [tau] disjoint, [compose rho tau] adds all bindings
      [x |-> t] of [tau] to [rho] and replaces all occurrences of [x] in
      [rho] with [t]. *)

  val disjoint : t -> t -> bool
  (** [disjoint rho tau] holds iff the domains of [rho] and [tau] are
      disjoint. *)

  val fold : (var -> trm -> 'a -> 'a) -> t -> 'a -> 'a
  (** For [rho] of the form [{x{1} |-> t{1},...,x{n} |-> t{n}}],
      [fold f rho e] returns [f x{1} t{1} (... (f x{n} t{n} e)...)]; the
      order of accumulation is unspecified. *)

  val iter : (var -> trm -> unit) -> t -> unit
  (** [iter f rho] applies [f x t] for all bindings [x |-> t] of [rho]. The
      order of application is unspecified. *)

  val exists : (var -> trm -> bool) -> t -> bool
  (** [exists p rho] holds iff [p x t] holds for some binding [x |-> t] in
      [rho]. *)

  val for_all : (var -> trm -> bool) -> t -> bool
  (** [for_all p rho] holds iff [p x t] holds for all binding [x |-> t] in
      [rho]. *)

  val choose : (var -> trm -> bool) -> t -> var * trm
  (** [choose p rho] returns [(x, t)] iff there exists a binding [x |-> t]
      in [rho] with [p x t]; otherwise, [Not_found] is raised. *)
end

(** {i Term substitutions}. Given an implementation [Var] of variables, and
    terms [Trm] with [Var.t] variables, {!Subst.Make} generates an
    implementation of term substitions with domain variables of type [Var.t]
    and codomain [Trm.t]. *)
module Make (Var : VAR) (Trm : TRM with type var = Var.t) :
  S with type var = Var.t and type trm = Trm.t

(**/**)

(** Following for debugging only. *)
module Test : sig
  val run : unit -> unit
  (** Run a random simulation. *)
end
