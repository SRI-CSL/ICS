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

(** {i Inference system for functional arrays}.

    @author Harald Ruess *)

(** The signature of the theory of function arrays consist of the ternary
    function symbol [update], the binary function symbol [lookup], and the
    monadic predicate symbol [array].

    For [update(a,i,x)] we use the infix notation [a\[i:=x\]] and instead of
    [lookup(a,i)] we write [a\[i\]].

    An {i update} [a\[i:= x\]] updates a functional array at position [i]
    with value [x], and the {i lookup} [a\[i\]] returns the value of the
    functional array [a] at position [i].

    The predicate [array(a)] restricts interpretations of [a] to functional
    arrays only.

    These operators are axiomatized in the theory [F] as follows.

    - [a\[i:=x\]\[i\] = x],
    - [i/=j] => [a\[i:=x\]\[j\] = a\[j\]],
    - [i/=j => a\[i:=x\]\[j:=y\] = a\[j:=y\]\[i:=x\]],
    - [a\[i:=x\]\[i:=y\] = a\[i:=y\]]
    - [t = a\[i:=x\] => array(a) & array(t)]
    - [t = a\[i\] => array(a)] *)

(** {i Term variables.} Used as input signature for {!Funarr.Flat} and
    {!Funarr.Make}. *)
module type VAR = sig
  (** Representation of variables. *)
  type t

  val equal : t -> t -> bool
  (** Equality test on variables. *)

  val compare : t -> t -> int
  (** A {i total ordering function} over elements. This is a two-argument
      function [compare] such that [compare e1 e2] is zero iff [equal e1 e2]
      and [compare e1 e2] is negative iff [compare e2 e1] is positive. A
      total order [<<] might be defined as [e1 << e2] iff
      [compare e1 e2 <= 0]. *)

  val hash : t -> int
  (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
  (** Printing an element onto the given formatter. *)

  val fresh : unit -> t
  (** Creating a fresh variable. Here, the notion of freshness depends on
      the context. *)
end

(** {i Flat array terms} are either of the form [a\[i:=x\]] or [a\[i\]] with
    [a], [i], [x] variables. Used as input signature for {!Funarr.Make}. *)
module type FLAT = sig
  (** Representation of variables. *)
  type var

  (** Representation of flat array terms. *)
  type t = private Update of var * var * var | Lookup of var * var

  val update : var -> var -> var -> t
  (** [update a i x] constructs the update flat term [a\[i:=x\]]. *)

  val lookup : var -> var -> t
  (** [lookup a i] constructs the lookup flat term [a\[i\]]. *)

  val map : (var -> var) -> t -> t
  (** [map f t] replaces each variable [y] in [t] with [f y]. *)

  val equal : t -> t -> bool
  (** [equal s t] holds iff the two flat array terms [s] and [t] are
      syntactically equal. *)

  val compare : t -> t -> int
  (** [compare s t] equals [0] iff [equal s t] holds. Furthermore,
      [compare s t < 0] iff [compare t s > 0]. *)

  val hash : t -> int
  (** Nonnegative hash value of flat array terms. *)

  val pp : Format.formatter -> t -> unit
  (** Printing a flat array term on the specified formatter. *)
end

(** {i Flat array terms}. Given an implementation [Var] of variables,
    construct a representation of flat array terms with [Var.t] variables. *)
module Flat (Var : VAR) : FLAT with type var = Var.t

(** {i Interface inference system} for the open inference system for
    functional arrays. Configurations of the interface system consist of
    triples [(E, D, C, P)] with [E] a finite set of variable equalities, [D]
    a finite set of variable disequalities, [C] a finite set of domain
    constraints of the form [array(x)], and [P] a finite set of
    {i conditional equalities} of the form
    [if x{1} = y{1} then x{2} = y{2} else x{3} = y{3}]. Such a conditional
    equality is logically equivalent with
    [(x{1} = y{1} & x{2} = y{2}) | (x{1} <> y{1} & x{2} = y{2})]. *)
module type INTERFACE = sig
  (** Representation of variables. *)
  type var

  val find : var -> var
  (** [find x] returns a variable [y] with [E |= x = y] and [y] is the
      {i canonical representative} of the equivalence class modulo [E]
      containing [x]. *)

  val canonical : var -> bool
  (** [canonical x] holds iff [x] is a [E]-canonical representative. *)

  val equal : var -> var -> bool
  (** [equal x y] holds iff [x] and [y] are equal modulo [E]; that is,
      [E |= x = y] holds. *)

  val diseq : var -> var -> bool
  (** [diseq x y] holds iff [E,D |= x <> y] holds. *)

  val choose_equiv : (var -> bool) -> var -> var
  (** [choose_equiv f x] returns [y] with [E |= x = y] and [f x] holds.
      Otherwise, [Not_found] is raised. *)

  val iter_equiv : (var -> unit) -> var -> unit
  (** [iter_equiv f x] applies [f] exactly once to each [y] with
      [E |= x = y]. *)

  val iter_diseqs : (var -> var -> unit) -> var -> unit
  (** [iter_diseqs f x] applies [f x' y'] to all [E]-canonical [x'] and [y']
      with [E |= x = x'] and [E, D |= x' <> y']. *)

  val union : var -> var -> unit
  (** If [E |= x = y] does not hold, then [union x y] adds the equality
      [x = y] to [E]; otherwise, the configuration is left unchanged. *)

  val ite : var * var -> var * var -> var * var -> unit
  (** [ite (x, y) (u, u') (v, v')] adds a {i conditinal equality}
      [if x = y then u = u' else v = v'] to the [P] configuration. *)

  val array : var -> unit
  (** [array(x)] adds an array domain constraint for [x] to the current [C]
      configuration. *)
end

(** {i Inference system for functional arrays.} Configurations are of the
    form [(E, D, C, P, A)] with [(E, D, C, P)] the
    {i interface configuration} as described in {!Funarr.INTERFACE} and [A]
    is a finite set of equalities of the form [u = a\[i\]] or
    [u = a\[i:=x\]] with [u],[a],[i],[x] variables.

    Such a configuration is {i closed} in that

    - [P |= array(b)] if there is [u = b\[i:=x\]], [u = b\[i\]], or
      [b = a\[i:=x\]] in [A], and
    - [E |= x = y] if [E,D,A |= x = y]; that is all variable equalities
      derivable from [(E,D,A)] (but not necessarily from [P] are in [E].

    The inference system works by updating a {i current configuration}
    through equivalence-preserving transformations. *)
module type INFSYS = sig
  (** Representation of term variables. *)
  type var

  (** Representation of flat array terms (see {!Funarr.Flat}). *)
  type flat

  (** Representation of a finite set of equalities of the form [u = t] with
      [u] a variable and [t] a flat array term. *)
  type t

  val empty : t
  (** The empty [A] configuration with an empty set of equalities. *)

  val initialize : t -> unit
  (** [initialize a] intiializes the current [A] configuration with [a]. *)

  val reset : unit -> unit
  (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current [A] configuration has not been
      modified since the latest [initialize] or [reset]. *)

  val current : unit -> t
  (** [current()] returns the current [A] configuration. *)

  val pp : unit -> unit
  (** [pp()] prints the current [A] configuration. *)

  (** Finite set of bindings [u |-> t] with [u] a variable and [t] a flat
      array term. *)
  module Cfg : Maps.S with type key = var and type value = flat

  val config : unit -> Cfg.t
  (** Representation of the current [A] configuration as a finite set of
      bindings. *)

  val find : var -> flat
  (** [find u] returns a flat term [t] if there is [v = t] in the current
      configuration [A] with [E |= u = v]. Otherwise, [Not_found] is raised. *)

  val inv : flat -> var
  (** [inv t] returns variable [u] if there is an equality [u = t] in the
      current configuration [A]. Otherwise, [Not_found] is raised. *)

  val alias : flat -> var
  (** [alias t] returns a variable [u] with [E,A |= u = t] in a possibly
      modified configuration. *)

  val propagate_eq : var -> var -> unit
  (** For variables [x], [y] with [E |= x = y], [y] [E]-canonical, and [x]
      not [E]-canonical, [propagate_eq x y] adds newly derived variable
      equalities to the [E] configuration. *)

  val propagate_deq : var -> var -> unit
  (** For [E]-canonical variables [x], [y] with [E |= x <> y]
      [propagate_deq x y] adds newly derived variable equalities to the [E]
      configuration. *)
end

(** {i Closed inference system for functional arrays.}. Given an
    implementation [Var] of term variables, [Term] of flat array terms, and
    an interface inference system [I], the functor {!Funarr.Make} constructs
    a closed interface system for the theory of functional arrays. *)
module Make
    (Var : VAR)
    (Term : FLAT with type var = Var.t)
    (I : INTERFACE with type var = Var.t) :
  INFSYS with type var = Var.t and type flat = Term.t
