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

(** Inference system for canonizable theories with forward chaining.

  @author Harald Ruess
*)

type iter = (Judgement.equal -> unit) -> unit

(** A {i canonizable} theory [th] is specified by means of a
  - homomorphic mapping [map f a],
  - forward chaining rules [chains], and
  - a branching function [disjunction].
  In addition, the signature of [th] is assumed not to contain any
  constant symbols. *)
module type T = sig
  val th : Theory.t
  val can : Funsym.t -> Term.Args.t -> Term.t
  val is_diseq : Term.t -> Term.t -> bool
  val chains : Axioms.Chain.t list
  val disjunction : iter -> Judgement.disjunction
end


(** {i Configuration} for inference systems on flat terms. *)
module type CONFIG = sig

  type t
    (** Representation of a conjunction of (directed) equalities [x = f(x{1},...,x{n}]] 
      with [x], [x{i}] variables.  In this case, [x] is also called a {i domain variable} 
      and [f(x{1},...,x{n}] is a {i codomain flat term}. *)

  val is_empty : t -> bool
    (** [is_empty s] holds iff [s] does not contain any equalities. *)

  val equalities : t -> Judgement.Equals.t
    (** [equalities s] returns the conjunction of equalities 
      represented by [s] as a set. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-print a configuration. *)
  
  val iter : (Judgement.equal -> unit) -> t -> unit
    (** [iter f s] applies [f e] for each equality [e] in [s]. The order
      of applying [f] on these equalities is unspecified. *)
    
  val iter_on : Term.t -> (Judgement.equal -> unit) -> t -> unit
    (** [iter_on x f s] applies [f e] for each equality [e] in [s] 
      containing [x] as a subterm (on lhs or rhs).  The order of applying [f] 
      on these equalities is unspecified. *)
    
  val fold : (Judgement.equal -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s acc] accumulates a result, starting with [acc], by calling [f e] 
      on the equalities [e] in [s]. *)
  
  val mem : Term.t -> t -> bool
    (** [mem x s] holds iff [x] is a domain variable or an 
      argument of a codomain flat term of an equality in [s]. *)

  exception Empty
    
  val dep : t -> Term.t -> Dep.Set.t
    (** [dep s y] returns the set of of domain variables [x] such that
      [x = f(...,y,...)] in [s]. [Empty] is raised, if there are no
      such [y]. *)
    
  val occ : Term.t -> t -> bool
    (** [occ x s] holds iff is [x] is a domain variable or an 
      argument variable of a codomain flat term of [s]. *)
    
  val in_dom : t -> Term.t -> bool 
    (** [in_dom s x] holds iff [x] is a domain variable of [s]. *)
    
  val in_cod : t -> Term.t -> bool
    (** [in_cod s y] holds iff [y] is an argument variable of a 
      codomain flat term of [s]. *)
    
  val apply : t -> Term.t -> Term.t * Judgement.equal
    (** [apply s x] returns [a, e] such that equality [x = a] in [s]
      and [e |- x = a]. In case there are several such equalities,
      an arbitrary (but fixed) choice is used, and if there is no such
      equality, [Not_found] is raised. *)
    
  val inv : t -> Term.t -> Term.t * Judgement.equal
 
  val empty : unit -> t
    (** [empty()] returns an empty configuration. *)
    
  val interp : t -> Term.Interp.t
    
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

(** An {i inference system} manipulates current {i configurations}. *)
module type INFSYS = sig

  type config 

  val current : unit -> config
    (** Return the current configuration. The resulting configuration
      should not be updated in that side effects might be propagated
      in an undesired way. *)

  val finalize : unit -> config
    (** Retrieve modified equality set. *)

  val reset : unit -> unit
    (** Reset the current configuration to the empty configuration. *)

  val initialize : config -> unit
    (** Intitialize current configuration with the given equalities. *)

  val is_unchanged : unit -> bool
    (** [is_unchange()] holds if the current configuration has not
      been modified since the last initialization or reset. *)

  val abstract : Term.t -> Judgement.atom -> unit
    (** [(g U {fct[a]}; e; p)] ==> [(g U {fct[x]}; e U {x = a}; p)]
      with 
      - [a] a nonvariable term, 
      - [a] an [i]-pure term, 
      - and [x] fresh. *)
    
  val process_equal : Judgement.equal -> unit
    (** [(g, a = b; e; p)] ==> [(g; e'; p')] 
      with 
      - [a], [b] [i]-pure, 
      - [|= e', p' <=> |= e, a = b, p]
      - if [e' |= x = y] then [p' |= x = y]. *)

  val propagate_equal : Term.t -> unit
    (** [(g, e; p)] ==> [(g; e'; p)]
      with 
      - [e |= x = y], 
      - not[p |= x = y], 
      - [|= e, p <=> |= e', p']  *)
  
  val process_diseq : Judgement.equal -> unit
    (** [(g, a <> a; e; p)] ==> [(g; e'; p')] 
      with [a], [b] [i]-pure, [|= e', p' <=> |= e, p, a <> b]. *)

  val propagate_diseq : Judgement.diseq  -> unit
    (** [(g; e; p)] ==> [(g; e'; p')] 
      with 
      - [p' |= x <> y]
      - [|= e', p' <=> |= e, p]. *)

  val branch : unit -> Judgement.disjunction option
    (** [(g; e; p)] ==> [(g, c1; e; p) | ... | (g, cn; e; p)]
      with 
      - [e, p |= c1 \/ ... \/ cn]
      - not [e, p |= ci] *)

  val normalize : unit -> unit
    (**  [(g; e; p)] ==> [(g'; e'; p')]
      where source and target configuration are equivalent. *)

end

module type COMPONENT = sig
  val th : Theory.t
  module Config : CONFIG
  module Infsys : (INFSYS with type config = Config.t)
end

module Make(Can: T): COMPONENT 
  (** Constructing an inference system from the 
    specification [Can] of a canonizable theory. *)
