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

(** {i Open inference system for monadic literals.}

  A {i monadic predicate symbol} has arity [1], and a 
  {i monadic literal} is a formula which contains only atoms
  of the form [p(x)] and negations thereof, with [p] a 
  predicate symbol and [x] a term variable. 

  Given an interpretation [I(p)(x)] as a set over a given
  interpretation domain, we say that [p] and [q]
  are {i disjoint} if the intersection of [I(p)(x)] and [I(q)(x)]
  is empty. Furthermore, [p] {i subsumes} [q] if [I(p)(x)] is
  a subset of [I(q)(x)]. Now, the theory of monadic literals
  is given by
     - [p(x) => q(x)]      when [p] subsumes [q], and
     - [~(p(x) & q(x))]    when [p] and [q] are disjoint.

  This module defines an {i open inference module} for 
  online processing of monadic literals in this theory. 

  @author Harald Ruess
*)

(** {i Totally ordered set of variables.} Input
  signature for {!Literal.Make}. *)
module type VAR = Type.ORDERED

(** {i Monadic predicate symbols} have arity one.
  Inputsignature for {!Literal.Make}. *)
module type PREDSYM = sig
  type t
    (** Representation of monadic predicate symbols. *)

  val equal : t -> t -> bool  
    (** Equality test on predicate symbols. *)

  val compare : t -> t -> int 
    (** Total comparison function for predicate symbols.
      In particular, [compare p q] equals [0] iff [equal p q]
      holds, and [compare p q] is negative iff [compare q p]
      is positive. *)

  val hash : t -> int
    (** [hash p] returns a nonnegative hash value for [p]. *)

  val pp : Format.formatter -> t -> unit
    (** Printing a predicate on the given formatter. *)

  val sub : t -> t -> bool
    (** [sub p q] holds iff for all possible arguments, say [x],
      if [p(x)] holds than so does [q(x)]. *)

  val disjoint : t -> t -> bool
    (** [disjoint p q] holds iff for all possible arguments, say [x],
      [p(x)] and [q(x)] never hold simultaneously. *)
end


(** {i Variable partitioning inference system} for the
  theory of pure identity. 

  Configurations  [(E, D)] of this inference system are equivalent to 
  the conjunction of the variable equalities in [E] and the variable 
  disequalities in [D]. 

  Variables [x] and [y] are {i equivalent modulo} [E] if [E |= x = y]
  holds in the theory of pure identity. 

  The inference system works by updating a {i current context}. *)
module type PARTITION = sig
  type var
    (** Representation of variables. *)
  val find : var -> var
    (** [find x] returns the {i canonical representative} of
      the equivalence class modulo [E] containing [x]. *)

  val canonical : var -> bool
    (** [canonical x] holds iff [x] is the canonical 
      representative of the equivalence class modulo [E] containing [x]. *)

  val equal : var -> var -> bool
    (** [equal x y] holds iff [E |= x = y] is valid. *)

  val diseq : var -> var -> bool
    (** [diseq x y] holds iff [E, D |= x <> y] is valid. *)

  val union : var -> var -> unit
    (** If [equal x y] does not hold, then [union x y] extends [E] 
      of the current configuration with a variable equality [x = y];
      otherwise the current configuration is left unchanged. *)

  val separate : var -> var -> unit
    (** If [diseq x y] does not hold, then [separate x y] extends [D] 
      of the current configuration with a variable disequality [x <> y];
      otherwise the current configuration is left unchanged. *)
end


(** {i Inference system} for incrementally processing 
  predicates of the form [p(x)] and [~p(x)] with [p]
  a monadic predicate symbol and [x] a term variable.

  Configurations are of the form [(E, D, P, N)] with 
  the {i interface configuration} [(E, D)] a variable 
  partitioning as described in {!Literal.PARTITION}. 
  Furthermore,  [P] is a finite set of atoms
  of the form [p(x)] and [N] is a finite set of 
  negated atoms of the form [~p(x)].  Such a configuration
  is interpreted as the conjunction of all the literals
  in its components.

  The inference system works by updating a {i current configuration}. *)
module type INFSYS = sig
  type predsym
    (** Representation of monadic predicate symbols. *)

  type var
    (** Representation of term variables. *)

  type t
    (** Representation of configurations [(P, N)] as
      described above. *)

  val empty : t
    (** Configuration with empty [P] and [N]. *)
    
  val initialize : t -> unit
    (** [initialize s] initializes the current configuration
      with the argument configuration [s]. *)

  val reset : unit -> unit
    (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
    (** [unchanged()] holds iff the current configuration
      [(P, N)] has been unchanged since the latest [initialize]
      or [reset]. *)

  val current : unit -> t
    (** [current()] returns the current configuration [(P,N)].
      Further updates of the current configuration do not 
      affect the configuration returned by [current]. *)

  module Set : (Sets.S with type elt = predsym)
    (** Set of predicate symbols. Used by [Pos] and [Neg] below. *)

  module Pos : (Maps.S with type key = var and type value = Set.t)
    (** Finite set of bindings of the form [x |-> {p{1},...,p{n}}]
      with [x] a variable and [p{i}] are predicate symbols. Such
      a binding represents the conjunction [p{1}(x) & ... & p{n}(x)]. *)

  module Neg : (Maps.S with type key = var and type value = Set.t)
    (** Finite set of bindings of the form [x |-> {p{1},...,p{n}}]
      with [x] a variable and [p{i}] are predicate symbols. Such
      a binding represents the conjunction [~p{1}(x) & ... & ~p{n}(x)]
     of negated atoms. *)

  val pos : unit -> Pos.t
    (** [pos(n)] returns the [P] part of the current configuration. *)

  val neg : unit -> Neg.t
    (** [neg(n)] returns the [N] part of the current configuration. *)

  val synchronized : unit -> bool
    (** [synchronized()] holds of the current configuration
      [(E,D,P,N)] if all domain variables in [P] (as obtained by
      [pos()] and all domain variables in [N] (as obtained by [neg()]
      are [E]-canonical. *)

  val valid : predsym -> var -> bool
    (** If the current configuration is synchronized, then
      [valid p x] holds iff [p(x)] is valid in the current
      configuration [(E,D,P,N)]. *)

  val unsat : predsym -> var -> bool 
    (** If the current configuration is synchronized, then
      [unsat p x] holds iff [p(x)] is unsatisfiable in the 
      current configuration [(E,P,N)]. *)

  val diseq : var -> var -> bool
    (** If the current configuration is synchronized, then [diseq x y] 
      holds iff the variable disequality [x<>y] holds in the current 
      configuration [(E,P,N)]. *)

  exception Unsat
    (** Exception raised when current configuration is unsatisfiable. *)

  val processPos : predsym -> var -> unit
    (** If the current configuration is synchronized, then:
      if [valid p x] does not hold, then [processPos p x] adds the
      atom [p(x)] to the [P] part of the current configuration; otherwise
      the current configuration is left unchanged. If the addition of
      [p(x)] renders the current configuration unsatisfiable, then
      [Unsat] is raised. *)

  val processNeg : predsym -> var -> unit
    (**  If the current configuration is synchronized, then: 
      if [unsat p x] does not hold, then [processPos p x] adds the
      negated atom [~p(x)] to the [N] part of the current configuration; 
      otherwise the current configuration is left unchanged. If the 
      addition of [~p(x)] renders the current configuration unsatisfiable, 
      then [Unsat] is raised. *)

  val propagateEq : var ->  var -> unit
    (** Propagates variable equalities [x = y]. Whenever
      [p(x)] and [q(y)] holds then also [p(y)] and [q(x)] holds.
      This function assumes that [x = y] holds in [E],  [y] is 
      canonical and [x] non-canonical in [E]. Calling [propagateEq]
      for each such variable pair in [E] renders the configuration
      {i synchronized}. *)
end
  
(** {i Closed inference system for  processing monadic literals}.  
  Given an implementation [Var] of totally ordered variables,
  an implementation [P] of monadic predicates symbols, and a
  complete inference system [Partition] for the theory of pure
  identity, the functor {!Literal.Make} constructs a complete
  inference system for the theory of monadic literals. *)
module Make
  (Var: VAR)
  (Predsym: PREDSYM)
  (Partition: PARTITION with type var = Var.t)
  : (INFSYS with type predsym = Predsym.t
	    and type var = Var.t)

