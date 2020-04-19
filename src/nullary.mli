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

(** {i Inference system for propositional variables.}

  A {i propositional variable} is interpreted over the set [{true,false}]. 
  Such a propositional variable is said to be {i valid} ({i unsat})
  if the interpretation of [p] is restricted to [true] ([false]). 

  Configurations consist of a set of {i valid} propositional variables and
  a set of {i unsatisfiable} variables.  The environment inference
  system is notified whenever a variable becomes valid or unsatisfiable.

  @author Harald Ruess
*)

(** {i Propositional variables}. 
  Used as input signature of the functor {!Nullary.Make}.*) 
module type VAR = sig
  type t  
    (** Representation type. *)

  val equal : t -> t -> bool
    (** Equality on representations. *)
    
  val compare : t -> t -> int
    (** A {i total ordering function} over elements.This is a two-argument 
      function [compare] such that [compare e1 e2] is zero iff [equal e1 e2] 
      and [compare e1 e2] is negative iff [compare e2 e1] is positive. A total 
      order [<<] might be defined as [e1 << e2] iff [compare  e1 e2 <= 0]. *)

  val hash : t -> int
    (** Nonnegative hash function. *)

  val pp : Format.formatter -> t -> unit
    (** Printing an element onto the given formatter. *)
end

(** {i Inference system for propostional variables}
  and input signature of the  functor {!Nullary.Make}. 

  Configurations are of the form [(Valid, Unsat)] with [Valid] and
  [Unsat] finite sets of propositional variables. Such a
  configuration is logically equivalent to the conjunction
  of all the propositional variables in [Valid] with the negations
  of all the propositional variables in [Unsat].

  The inference system works by updating a {i current configuration}. *)
module type INFSYS = sig
  type var
    (** Representation of propositional variables. *)

  type t
    (** Representation of configurations. *)

  val empty : t
    (** The empty configuration with [Valid] and [Unsat] both empty. *)

  val initialize : t -> unit
    (** [initialize s] initializes the current configuration 
      of the inference system with the argument configuration [s]. *)

  val reset : unit -> unit
    (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
    (** [unchanged()] holds iff the current configuration
      is logically equivalent with the argument configuration
      [s] of the latest [initialize s] call. *)

  val current : unit -> t
    (** [current()] returns the current configuration. Any
      future updates of current configurations do not affect
      the result of [current()]. *)

  module Valid : (Sets.S with type elt = var)
    (** Set of propositional variables. Used for representing
      the [Valid] part of configurations. *)

  module Unsat : (Sets.S with type elt = var)
    (** Set of propositional variables. Used for representing
      the [Unsat] part of configurations. *)

  val valid : unit -> Valid.t
    (** [valid()] returns the [Valid] set of propositional
      variables of the current configuration. *)

  val unsat : unit -> Unsat.t
    (** [unsat()] returns the [Unsat] set of propositional
      variables of the current configuration. *)

  val isValid : var -> bool
    (** [isValid p] holds iff the current configuration implies [p]. *)

  val isUnsat : var -> bool  
    (** [isUnsat p] holds iff the current configuration implies 
      the negation of [p]. *)

  val pp : Format.formatter -> unit
    (** Pretty-print the current configuration. *)

  exception Unsat
    (** Exception used for flagging inconsistencies 
      of the current configuation. *)

  val processValid : var -> unit
    (** [processValid p] adds [p] to [Valid] if it is
      not already implied by the current context. *)

  val processUnsat : var -> unit
    (** [processValid p] adds [p] to [Unsat] if its
      negation is  not already implied by the current context. *)

end


module Make(Var: VAR): (INFSYS with type var = Var.t)
  (** Given an implementation [Var] of propositional
    variables return an inference system for sets
    of valid and unsatisfiable propositional variables. *)

