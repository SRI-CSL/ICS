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

(** Combined inference system

  @author Harald Ruess
  @author N. Shankar 

  Inference system for the union of the theories in {!Th.t}.
  A {i configuration} consists of a triple [(g, e, p)] with
  - [g] the input facts,
  - [e] the equality sets for the individual theories
  - [v] the shared variable equalities and disequalities. *)


module Config : sig

  type t = { e: E.Config.t; v: V.Config.t; }

  val shared : t -> V.Config.t
  val components : t -> E.Config.t
 
  val empty: unit -> t 
    (** The empty configuration. *)

  module Print : sig
    val all : Format.formatter -> t -> unit
      (** Pretty-printing of configuration. *)
    val component : Theory.t -> Format.formatter -> t -> unit
      (** Pretty-printing of theory-specific component *)
    val shared : Format.formatter -> t -> unit
      (** Pretty-printing of shared part of configuration. *)
  end 
  
  val pp : Format.formatter -> t -> unit
    (** Pretty-printing of combined equality set. *)

  val dep : Theory.t -> t -> Term.t -> Dep.Set.t
    (** Dependency index for variable [x] and theory [i]. *)

  (** Theory-specific lookups. *)
  module Find : sig

    val lookup : Theory.t -> t -> Term.t -> Term.t
      (** [lookup s th x] is [a] if [x = a] is in the equality set for 
	theory [th] in [s]; otherwise, the result is just [x]. *)

    val justify : Theory.t -> t -> Term.t -> Judgement.equal
      (** [justify s th x] returns [e |- x = Find.lookup s th x]. *)

  end

  (** Theory-specific inverse lookups. *)
  module Inv : sig
    
    val lookup : t -> Term.t -> Term.t
      (** [lookup s t] is [x] if there is [x = t] in the solution 
	set for theory [th]; otherwise [Not_found] is raised. *)
     
    val justify : t -> Term.t -> Judgement.equal
      (** [justify s th x] returns [e |- x = Inv.lookup s th x]. *)

  end

  val occ : Theory.t -> t -> Term.t -> bool
    (** [occ i s x] holds for a variable [x] if [x] occurs in the equality
      set for theory [i]. *)

  module Can : sig 
    
    val term : t -> Term.t -> Term.t
      (** Given an equality set [e] and a partition [p] as obtained
	from processing, [can (e, p) a] returns a term [b] equivalent
	to [a] in [(e, p)] together with a justification [rho].  If
	no further branching is applicable on [(e, p)], then [can (e, p)]
	is a canonizer in the sense that [can (e, p) a] is syntactically
	equal to [can (e, p) b] iff the equality [a = b] follows from [(e, p)]
	in the supported union of theories. *)

    val justify : t -> Term.t -> Judgement.equal
      (** [justify s t |- t = Can.term s t]. *)

  end 

  module Diseq : sig

    val test : t -> Term.t -> Term.t -> bool

    val justify : t -> Term.t -> Term.t -> Judgement.diseq

  end 

  module Cnstrnt : sig

    val test : t -> Term.t -> Cnstrnt.t -> bool

    val justify : t -> Term.t -> Cnstrnt.t -> Judgement.cnstrnt

  end 


  module Nonneg : sig
    
    val test : t -> Term.t -> bool
      
    val justify : t -> Term.t -> Judgement.equal
      
  end 

  module Pos : sig
    
    val test : t -> Term.t -> bool
      
    val justify : t -> Term.t -> Judgement.equal
      
  end 



  val model : t -> Term.Model.t
    
end

module Infsys : sig

  val initialize : Config.t -> unit
  
  val current : unit -> Config.t

  val reset : unit -> unit
  
  val finalize : unit -> Config.t

  val is_unchanged : unit -> bool

  val process_equal : Judgement.equal -> unit

  val process_diseq : Judgement.diseq -> unit

  val process_nonneg : Judgement.nonneg -> unit

  val process_pos : Judgement.pos -> unit

  val process_cnstrnt : Judgement.cnstrnt -> unit

 (* val process : Judgement.atom -> unit *)
    (** Given a starting configuration [({fct}, e, p)], process applies
      all rules of the combined inference system (except branching rules).
      The source and target configuration of [process] are {i equivalent},
      although the target configuration might contain internally generated
      variables not present in the source configuration. *)

  val model : unit -> Term.Model.t

end
