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

(** {i Configurations}

    This module provides inference systems with manipulating sets, maps,
    exponential maps, and substitutions.

    @author Harald Ruess *)

(** Inference system for manipulating a finite set configuration. *)
module Set (S : Sets.S) : sig
  val initialize : S.t -> unit
  (** [initialize s] sets the current configuration to the set [s]. *)

  val current : unit -> S.t
  (** [current()] returns the current configuation. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current configuration has been unchanged
      since the latest [initialize]. *)

  val mem : S.elt -> bool
  (** [mem x] holds iff [x] is an element of the current configuration set. *)

  val add : S.elt -> unit
  (** [add x] adds [x] to the current configuration sets. *)

  val union : S.t -> unit
  (** [union s] adds all elements [x] of [s] to the current configuration. *)

  val remove : S.elt -> unit
  (** [remove x] removes [x] from the current configuration. *)
end

(** Inference system for manipulating a onfiguration consisting of a finite
    map. *)
module Map (M : Maps.S) : sig
  val initialize : M.t -> unit
  (** [initialize m] sets the current configuration to the map [m]. *)

  val current : unit -> M.t
  (** [current()] returns the current configuration map. *)

  val reset : unit -> unit
  (** [reset()] reset the current configuration to the empty map. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current configuration has been unchanged
      since the latest [initialize]. *)

  val find : M.key -> M.value
  (** [find x] returns [t] if there is a binding [x |-> t] in the current
      configuration; otherwise [Not_found] is raised. *)

  val mem : M.key -> bool
  (** [mem x] holds iff [x] is in the domain of the current configuration;
      that is, there is a binding [x |-> t] in the current configuration. *)

  val set : M.key -> M.value -> unit
  (** [set x t] updates the current configuration with the binding
      [x |-> t]. *)

  val remove : M.key -> unit
  (** [remove x] removes a binding of the form [x |-> t] from the current
      configuration. *)
end

(** Inference system for manipulating a onfiguration consisting of a finite
    exponential map with bindings [x |-> {x{1},....x{n}}]. *)
module Powermap (M : Powermaps.S) : sig
  val initialize : M.t -> unit
  (** [initialize m] sets the current configuration to the map [m]. *)

  val current : unit -> M.t
  (** [current()] returns the current configuration map. *)

  val reset : unit -> unit
  (** [reset()] reset the current configuration to the empty map. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current configuration has been unchanged
      since the latest [initialize]. *)

  val find : M.key -> M.Values.t
  (** [find x] returns the set [s] if there is a binding [x |-> s] in the
      current configuration; and the empty set otherwise. *)

  val mem : M.key -> bool
  (** [mem x] holds iff [x] is in the domain of the current configuration;
      that is, there is a binding [x |-> s] in the current configuration. *)

  val is_singleton : M.key -> bool
  (** [is_singleton x] holds iff the current configuration has only one
      binding of the form [x |-> s]. *)

  val update : M.key -> M.Values.t -> unit
  (** [update x t] updates the current configuration with the binding
      [x |-> t]. *)

  val remove : M.key -> unit
  (** [remove x] removes the binding [x |-> s] from the current
      configuration. *)

  val merge : M.key -> M.key -> unit
  (** [merge x y] updates the current configuation with the binding
      [y |-> find(x) U find(y)] and removes the binding for [x]. *)

  val add : M.key -> M.key -> unit
  (** [add x y] updates the current configuartion with the binding
      [y |-> {x} U find(y)]. *)

  val rem : M.key -> M.key -> unit
  (** [rem x y] updates the current configuration with the binding
      [y |-> find(y)\{x}]. *)
end

(** Inference system for manipulating a onfiguration consisting of a
    substitution. *)
module Subst (S : Subst.S) : sig
  (** Configurations are substitions. *)
  type t = S.t

  val initialize : S.t -> unit
  (** [initialize s] initializes the {i current configuration} with the
      substitution [s]. *)

  val current : unit -> S.t
  (** [current()] returns the current configuration. *)

  val reset : unit -> unit
  (** [reset()] resets the current configuration to the empty substitution. *)

  val unchanged : unit -> bool
  (** [unchanged()] holds iff the current configuration has been unchanged
      since the latest [initialize] or [reset]. *)

  val lookup : S.var -> S.trm
  (** [lookup x] returns [t] if there is a binding [x |-> t] in the current
      configuration; otherwise, [Not_found] is raised. *)

  val mem : S.var -> bool
  (** [mem x] holds iff there is a binding [x |-> t] in the current
      configuration. *)

  val apply : S.trm -> S.trm
  (** [apply t] replaces all [x] in [t] with [s] for bindings [x |-> s] in
      the current configuration. *)

  val add : S.var -> S.trm -> unit
  (** For, [x] not in the domain of the current configuration, [add x t]
      adds the binding [x |-> t] to the current configuration and replaces
      all occurrences of [x] in codomain terms with [s]. *)

  val compose : S.t -> unit
  (** If [rho] is domain-disjoint from the current configuration, then
      [compose rho] adds all bindings in [rho] to the current configuration
      and replaces occurrences of domain variables [x] in codomain terms
      with [t], where [x |-> t] is in [rho]. *)

  val update : S.var -> S.trm -> unit
  (** [update x t] replaces a binding [x |-> s] with [x |-> t]. *)

  val remove : S.var -> unit
  (** [remove x] removes a binding [x |-> t] from the current configuation. *)

  val fold : (S.var -> S.trm -> 'a -> 'a) -> 'a -> 'a
  (** For [rho] the current configuration of the form
      [{x{1} |-> t{1},...,x{n} |-> t{n}}], the iterator [fold f e] returns
      [f x{1} t{1} (... (f x{n} t{n} e)...)]; the order of accumulation is
      unspecified. *)

  val iter : (S.var -> S.trm -> unit) -> unit
  (** For [rho] the current configuration, [iter f rho] applies [f x t] for
      all bindings [x |-> t] of [rho]. The order of application is
      unspecified. *)

  val exists : (S.var -> S.trm -> bool) -> bool
  (** For [rho] the current configuration, [exists p rho] holds iff [p x t]
      holds for some binding [x |-> t] in [rho]. *)

  val for_all : (S.var -> S.trm -> bool) -> bool
  (** For [rho] the current configuration, [for_all p rho] holds iff [p x t]
      holds for all binding [x |-> t] in [rho]. *)

  val choose : (S.var -> S.trm -> bool) -> S.var * S.trm
  (** For [rho] the current configuration, , [choose p rho] returns [(x, t)]
      iff there exists a binding [x |-> t] in [rho] with [p x t]; otherwise,
      [Not_found] is raised. *)
end
