(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Nonlinear arithmetic.

  @author Harald Ruess

  A {i nonlinear arithmetic term} is an ordered sum-of-monomials built-up from 
  rational constants, nonlinear multiplication, and addition. Since, every 
  linear arithmetic term (see module {!Linarith}) is also a nonlinear arithmetic
  term we view the theory of nonlinear arithmetic as the extension of linear
  arithmetic with nonlinear multiplication.
  
  This module defines the theory [nl] of {i nonlinear multiplication}
  and provides constructors for building canonical nonlinear arithmetic
  terms, and an inference system for processing atoms over nonlinear terms.
*)

val nl : Theory.t

module Pprod : sig

  val is_interp : Term.t -> bool
    (** A term [a] is {i interpreted} in the theory of nonlinear arithmetic
      if its toplevel function symbol is a nonlinear multiplication or if 
      {!Linarith.is_interp a} holds. *)
    
  val is_pure : Term.t -> bool
    (** A term [a] is a {i pure nonlinear arithmetic term} if it is built-up entirely 
      from function symbols in the theory of linear arithmetic {!Linarith.theory},
      the theory of nonlinear multiplication {!Nl.nl}, and variables. *)
    
  val is_canonical : Term.t -> bool
    (** [is_canonical a] is true iff [a] is an ordered sum of monomials. *)

  val mk_mult : Term.t -> Term.t -> Term.t

end

module Nonlin : sig

  val mk_mult : Term.t -> Term.t -> Term.t
    (** Given two canonical terms [a] and [b], [mk_mult a b] returns a canonical
      term [c] such that [c] equals [a * b] in nonlinear arithmetic. *)
    
  val mk_expt : Term.t -> int -> Term.t
    (** [mk_expt a n] iterates the [mk_mult] constructor such that
      [mk_expt a n] equals [a * ... * a] ([n] times). *)

  val mk_div : Term.t -> Term.t -> Term.t

end


(** Inference system for nonlinear multiplication. 
  In addition to AC inferences, this inference system
  - deduces, for example, [x >= 0] if [x = y * z] and [y, z >= 0]
  - branches on [x = 0] or [a = 1] if [x = x * a]. *)
(*
module Infsys:  sig

  type t
    (** Representation of a set of equalities [x = y*z] with
      [x], [y], [z] variables. *)

  val is_empty : unit -> bool
    (** Empty logical context. *)

  val pp : Format.formatter -> unit
    (** [pp fmt] prints the current logical context on output channel [fmt]. *)

  val apply : Jst.Eqtrans.t
    (** For a variable [x], [apply x] returns [(a, rho)] if [x = a] is in
      the current context; in this case, [rho |= x = a]. 
      Otherwise [Not_found] is raised. *)

  val inv : Jst.Eqtrans.t
    (** If [x = a] is in the current context, then [inv a] returns [(x, rho)]
      where [rho |= x = a]. Otherwise, [Not_found] is raised. *)

  val current : unit -> t 
    (** Return the current logical context. *)

  val reset : unit -> unit
    (** Reset current logical context to the empty context. *)

  val initialize : t -> unit

  val is_unchanged : unit -> bool

  val finalize : unit -> t

  val abstract : Term.t -> unit

  val process: Eqs.process

  val propagate : Eqs.propagate

  val normalize : unit -> unit

end
*)

