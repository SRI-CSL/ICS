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

(** Variable partitioning.

  @author Harald Ruess

  A variable partitioning consists of a set of 
  - variable equalities,
  - variable disequalities, and 
  - variable constraints. 
*)

module Config : sig

  type t
    (** Representation of a conjunction of  
      - variable equalities [x = y],
      - variable disequalities [x <> y], and
      - variable constraints [x in c] with [c] a constraint (see module {!Cnstrnt.t}).
      The variable equalities induce an {i equivalence relation}, and 
      we say that [x] and [y] are equivalent modulo [s], if equality
      [x = y] is valid in [s] in the theory of variable equality. *)
    
  val pp : Format.formatter -> t -> unit
    (** Pretty-printing a variable context. *)
    
  val empty : unit -> t
    (** The empty variable context. *)
    
  val is_empty : t -> bool
    (** Test if argument represents an empty variable partitioning. *)
    
  val is_canonical : t -> Term.t -> bool
    (** For a term variable [x], [is_canonical s x] holds 
      iff [find s x] returns [x]. *)
    
  val find : t -> Term.t -> Term.t
    (** [find s x] returns canonical representative 
      for equivalence class for [x]. *)

  val justify : t -> Term.t -> Judgement.equal
    (** [justify s x] returns [e |- x = find s x]. *)
    
  val cnstrnt : t -> Term.t -> Cnstrnt.t
    (** For a canonical variable [x], [cnstrnt s x] returns the
      domain constraint associated with the equivalence class [x].
      Raises [Not_found] if the interpretation is unconstrained. *)
    
  val diseqs : t -> Term.t -> Judgement.diseq list
    (** Disequalities for a variable [x]. *)
    
  val has_cnstrnt : t -> Term.t -> Cnstrnt.t -> bool
    (** For a constraint [c] and a variable [x],
      [has_cnstrnt c s x] is true if [x in c] holds in [s]. *)
    
  val is_equal : t -> Term.t -> Term.t -> bool
    (** For variables [x], [y], [is_equal s x y] holds if and only 
      if [x] and [y] are in the same equivalence class modulo [s]. *)
    
  val is_diseq : t -> Term.t -> Term.t -> bool
    (** [is_diseq s x y] holds iff [x <> y] is valid in [s]. *)

  module Explain : sig
    val equal : t -> Term.t -> Term.t -> Judgement.equal
    val diseq : t -> Term.t -> Term.t -> Judgement.diseq
    val cnstrnt : t -> Term.t -> Cnstrnt.t -> Judgement.cnstrnt
  end 

end


(** {6 Inference System} *)

(** Inference system for incremental processing of variable equalities, 
  variable disequalities, and variable constraints.  The configuration 
  of this inference system is of type {!V.t}. *)
module Infsys : sig

  val current : unit -> Config.t
    (** Current configuration of inference system. *)

  val reset : unit -> unit
    (** Reset current configuration of inference system to {!V.empty}. *)

  val initialize : Config.t -> unit
    (** [V.Infsys.initialize s] sets current configuration to [s]. *)

  val is_unchanged : unit -> bool
    (** [V.is_unchanged()] holds if the current configuration has not
      been modified since last {!V.initialize} or {!V.reset}. *)

  val finalize : unit -> Config.t
    (** [V.finalize s] returns current configuration of inference system.
      In contrast to [V.current s] the resulting state is protected against
      destructive updates. *)

  val register_external_diseq : (Term.t -> Term.t -> Judgement.diseq) -> unit
    (** Register functions for producing disequalities from other sources.
      If such a function [f] applied to two variables [x], [y] returns [rho], 
      then [rho |- x <> y] must hold; otherwise [f] throws [Not_found]. *)

  val is_diseq : Term.t -> Term.t -> Judgement.diseq option
    (** When [is_diseq x y] returns [Some(rho)], then [rho |- x <> y] in the
      current configuration or the disequality has been deduced from one of the
      the registered disequalities. *)

  val is_equal : Term.t -> Term.t -> Judgement.equal option
    (** When [is_equal x y] returs [Some(rho)] then [rho |- x = y] in the
      current configuration. *)

  val has_cnstrnt : Term.t -> Cnstrnt.t -> Judgement.cnstrnt option

  val can : Term.t -> Term.t * Judgement.equal
    (** Canonical representative. *)

  val process_cnstrnt : Judgement.cnstrnt -> unit
    (** [add s x c rho] adds a constraint [rho |- x : c] to [s]. *)

  val process_equal : Judgement.equal -> unit
    (** Adding a variable equality [x = y] by destructively
      updating the current ontext [s]. *)

  val process_diseq : Judgement.diseq -> unit
    (** Adding a variable equality [x = y] by 
      destructively updating context [s]. *)

  val normalize : unit -> unit

end

