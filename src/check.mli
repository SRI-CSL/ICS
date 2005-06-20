(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(**  {i Checker code.}

  Checker code for relations over terms randomly generates interpretations
  and tests if the specified relationship holds.

  @author Harald Ruess
*)

(** {i Totally ordered variables.} *)
module type VAR = sig
  type t 
    (** Representation of variables. *)

  val compare : t -> t -> int
    (** Total comparison function. If [compare x y] equals
      [0], then [x] and [y] are considered to be equal. 
      Furthermore, [compare x y] is negative iff [compare y x]
      is positive. *)

  val pp : Format.formatter -> t -> unit
    (** Printing a variable on specified formatter. *)
end

(** {i Interpretation domain}. *)
module type VALUE = sig
  type t 
    (** Representation of domain values. *)

  val equal : t -> t -> bool
    (** Equality on domain values. *)

  val random : unit -> t  
    (** Random generator for domain values. *)

  val pp : Format.formatter -> t -> unit
    (** Printing a domain values. *)
end

(** {i Terms} *)
module type TERM = sig
  type t 
    (** Representation of terms. *)

  type var
    (** Representation of variables. *)

  type value
    (** Representation of interpretation domain. *)

  val pp : Format.formatter -> t -> unit
    (** Printing terms on specified formatter. *)

  val iter : (var -> unit) -> t -> unit  
    (** [iter f t] applies [f x] for all term variables [x] of [t]. *)

  exception Partial
    (** Exception for indicating partiality of evaluation function. *)

  val eval : (var -> value) -> t -> value
    (** [eval rho t] evaluates a term [t] by applying the interpretation
      [rho] to all variables [x] in [t]; if [rho] is undefined for such
      a variable, [Partial] is raised. *)

end


(** {i Checker code}. For variables [X], terms [T] with variables in [X], and values [V], 
  {!Check.Make} generates checker code for various term relations by randomly generating
  term interpretations and checking for the specified relation. *)
module Make(X: VAR)(V: VALUE)(T: (TERM with type var = X.t and type value = V.t)) : sig
  val probe : int ref
    (** Number of generated interpretations for checking a relation. *)

  exception Violation of string * T.t list * T.t
    (** [Violation(name,[t{1};...;t{n}], t)] indicates that the relation
      [name(t{1};...;t{n}; t] does not hold. *)

  val valid : string -> (V.t list -> V.t -> bool) -> T.t list -> T.t -> bool
    (** [valid name relN argN res] holds if no violations of the relation
      [relN(argN, res)] could be detected. *)

  val valid1 : string -> (V.t -> V.t -> bool) -> T.t -> T.t -> bool
    (** Checks for binary relations [rel(arg, res)]. *)

  val valid2 : string -> (V.t -> V.t -> V.t -> bool) -> T.t -> T.t -> T.t -> bool
    (** Checks for ternary relations [rel(arg1, arg2, res)]. *)
end
