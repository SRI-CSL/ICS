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

(** Combined equality sets

  @author Harald Ruess

  Cross product of equality sets indexed by theories in {!Theory.t}. *)

(** Component equalities. *)
module type  CONFIG = sig
  type t
  val empty : unit -> t
  val is_empty : t -> bool
  val pp : Format.formatter -> t -> unit
  val apply : t -> Term.t -> Term.t * Judgement.equal
  val inv : t -> Term.t -> Term.t * Judgement.equal
  val dep : t -> Term.t -> Dep.Set.t
  val occ : Term.t -> t -> bool
  val model : t -> Term.Model.t
end

(** Component inference system. *)
module type INFSYS = sig
  type eqs
  val current : unit -> eqs
  val reset : unit -> unit
  val initialize : eqs -> unit
  val is_unchanged : unit -> bool
  val finalize : unit -> eqs
  val abstract : Term.t -> Judgement.atom -> unit
  val process_equal : (Judgement.equal -> unit) option
  val process_diseq : (Judgement.diseq -> unit) option  
  val process_nonneg : (Judgement.nonneg -> unit) option
  val process_pos : (Judgement.pos -> unit) option
  val propagate_equal : (Term.t -> unit) option
  val propagate_diseq : (Judgement.diseq -> unit) option 
  val propagate_cnstrnt : (Term.t -> unit) option 
  val propagate_nonneg : (Term.t -> unit) option
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end

(** A {i registrable} component. *)
module type COMPONENT = sig
  val th : Theory.t
  module Eqs : CONFIG
  module Infsys : (INFSYS with type eqs = Eqs.t)
end


(** Family of equality sets indexed by {!Theory.t}. *)
module Config : sig
  type t
  val empty : unit -> t
  val is_empty : t -> bool
  val pp : Format.formatter -> t -> unit
  val model : t -> Term.Model.t
  module Component : sig
    val is_empty : Theory.t -> t -> bool
    val pp : Theory.t -> Format.formatter -> t -> unit
    val dep : Theory.t -> t -> Term.t -> Dep.Set.t
    val occ : Theory.t -> Term.t -> t -> bool
    val model : Theory.t -> t -> Term.Model.t
    module Apply : sig
      val get : Theory.t -> t -> Term.t -> Term.t
      val justify : Theory.t -> t -> Term.t -> Judgement.equal
    end 
    module Inv : sig
      val get : Theory.t -> t -> Term.t -> Term.t
      val justify : Theory.t -> t -> Term.t -> Judgement.equal
    end 
    module Replace : sig
      val get : Theory.t -> t -> Term.t -> Term.t
      val justify : Theory.t -> t -> Term.t -> Judgement.equal
    end 
    module Diseq : sig
      val test : Theory.t -> t -> Term.t -> Term.t -> bool
      val justify : Theory.t -> t -> Term.t -> Term.t -> Judgement.diseq
    end
    module Equal : sig
      val test : Theory.t -> t -> Term.t -> Term.t -> bool
      val justify : Theory.t -> t -> Term.t -> Term.t -> Judgement.equal
    end
    module Nonneg : sig
      val test : Theory.t -> t -> Term.t -> Term.t -> bool
      val justify : Theory.t -> t -> Term.t -> Term.t -> Judgement.diseq
    end
    module Pos : sig
      val test : Theory.t -> t -> Term.t -> Term.t -> bool
      val justify : Theory.t -> t -> Term.t -> Term.t -> Judgement.equal
    end
  end
end

(** Abstract interface of an {i inference system} for equality theories.
  Such an inference system operates on configurations [(g, e, v)] with
  - [g] the global inputs (see module {!G}),
  - [e] a set of equalities of type {!Eqs.t}, and
  - [v] a set of variable equalities, disequalities, and other constraints (see module {!V}). *)
module Infsys: sig
  val current : unit -> Config.t
  val reset : unit -> unit
  val initialize : Config.t -> unit
  val is_unchanged : unit -> bool
  val finalize : unit -> Config.t
  val abstract : Theory.t -> Term.t -> Judgement.atom -> unit
  val process_equal : Theory.t -> Judgement.equal -> unit
  val process_diseq : Theory.t -> Judgement.diseq -> unit 
  val process_nonneg : Theory.t -> Judgement.nonneg -> unit
  val process_pos : Theory.t -> Judgement.pos -> unit
  val propagate_equal : Term.t -> unit 
  val propagate_diseq : Judgement.diseq -> unit 
  val propagate_cnstrnt : Term.t -> unit 
  val propagate_nonneg : Term.t -> unit
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end

(** {6 Registration} *)


module Register(C: COMPONENT): sig end
  (** Registration of an as yet unregistered component system.
    This affects the behavior of modules {!E.Infsys} and {!E.Eqs}. *)

val registered : unit -> Theory.Set.t
  (** Return the set of theories for the registered component systems. *)



(** {6 Registering Equality Components} *)

module type EQUAL = sig
  val th : Theory.t
  module Config : sig
    type t
    val empty : unit -> t
    val is_empty : t -> bool
    val pp : Format.formatter -> t -> unit
    val dep : t -> Term.t -> Dep.Set.t
    val occ : t -> Term.t -> bool
    val model : t -> Term.Model.t
    module Apply : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Inv : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Replace : sig
      val get : t -> Term.t -> Term.t
      val justify : t -> Term.t -> Judgement.equal
    end 
    module Diseq : sig
      val test : t -> Term.t -> Term.t -> bool
      val justify : t -> Term.t -> Term.t -> Judgement.diseq
    end
    module Equal : sig
      val test : t -> Term.t -> Term.t -> bool
      val justify : t -> Term.t -> Term.t -> Judgement.equal
    end
  end
  module Infsys : sig
    val current : unit -> Config.t
    val reset : unit -> unit
    val initialize : Config.t -> unit
    val is_unchanged : unit -> bool
    val finalize : unit -> Config.t
    val abstract : Term.t -> Judgement.atom -> unit
    val process_equal : (Judgement.equal -> unit)
    val process_diseq : (Judgement.diseq -> unit)
    val propagate_equal : (Term.t -> unit)
    val propagate_diseq : (Judgement.diseq -> unit) 
    val branch : unit -> Judgement.disjunction option
    val normalize : unit -> unit
  end
end


module Equal(E: EQUAL) : sig end









