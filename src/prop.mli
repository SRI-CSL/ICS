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

(** {i Open inference system for propositional formulas.}

  This module provides an open inference system for propositional
  formulas. Truth values of literals are propagated between the
  propositional inference system and its interface.
  
  @author Harald Ruess
*)

(** {i Ordered type of propositional variables.} *)
module type PROPVAR = Type.ORDERED

(** {i Propositional formulas.} *)
module type PROP = sig
  type var
    (** Representation of propositional variables. *)

  type t
    (** Representation of propositional formulas. *)

  val mk_true : t
    (** A trivially valid propositional formula. *)

  val mk_false : t 
    (** A trivially unsatisfiable propositional formula. *)

  val mk_posvar : var -> t
    (** [mk_posvar x] constructs a propositional formula for the 
      propositional variable [x]. *)

  val mk_negvar : var -> t  
    (** [mk_posvar x] constructs a propositional formula for the 
      negated propositional variable [x]. *)

  val mk_conj : t -> t -> t
    (** [mk_conj p q] returns the conjunction of propositional 
      formulas [p], [q]. *)

  val mk_disj : t -> t -> t
    (** [mk_disj p q] returns the disjunction of propositional formulas 
      [p], [q]. *)

  val union : var -> var -> t -> t
    (** [union x y p] is equivalent to [(x <=> y) & p]. *)

  val separate : var -> var -> t -> t 
    (** [union x y p] is equivalent to [(x # y) & p] with [#] exclusive or. *)

  val cofactorPos : t -> var -> t
    (** [cofactorPos p x] is equivalent with [p'] obtained by substituting
      every occurrence of [x] by [mk_true]. 
      In particular, [not(occurs x p')].  *)

  val cofactorNeg : t -> var -> t 
    (** [cofactorPos p x] is equivalent with [p'] obtained by substituting
      every occurrence of [x] by [mk_true]. In particular, 
      [not(occurs x p')]. *)

  val is_valid : t -> bool
    (** [is_valid p] holds iff propositional formula [p] is valid. *)

  val is_unsat : t -> bool 
    (** [is_unsat p] holds iff propositional formula [p] is valid. *)

  val occurs : var -> t -> bool  
    (** [occurs x p] holds iff [x] occurs in [p]. *)

  val andElim : t -> var list * var list * t
    (** [andElim] extracts implied literals in a propositional formula. 
      In particular, [andElim b] returns [([x{1};...;x{n}],[y{1};...;y{m}],b')]
      with [b => x{i}], [b => y{j}] for [i=1,...,n], [j=1,...,m], and [b'] 
      is such that none of the [x{i}] or [y{j}] occurs in [b'] and
      [b <=> b' & x{1} & ... & x{n} & y{1} & ... y{m}]. *)
end

(** {i Interface of propositional inference system.} 
  Configurations of the interface inference system are equivalent to [(P, N)]
  with [P] a conjunction of propositional variables and [N] a conjunction of 
  negated propostional variables. *)
module type INTERFACE = sig
  type var
    (** Representation of propositional variables. *)

  val valid : var -> unit
    (** [valid x] adds a propositional variable [x] to [P]. It throws
      an exception if the resulting configuration is inconsistent. *)

  val unsat : var -> unit    
    (** [unsat x] adds a propositional variable [x] to [N]. It throws
      an exception if the resulting configuration is inconsistent. *)
end

(** {i Propositional inference system.} 
  Configurations of the propositional inference systems are 
  propositional formulas.  The inference system progresses by updating
  a {i current configuration}. As an invariant, the current configuration
  is always satisfiable, and it {i reduced} in that it does not contain
  propositional variables [x] if it implies either [x] or its negation [~x]. *)
module type INFSYS = sig
  type var 
    (** Representation of propositional variables. *)

  type t
    (** Representation of configurations, which are propositional formulas. *)

  val empty : t
    (** The empty configuration which is equivalent to the trivially valid 
      formula. *)

  val initialize : t -> unit
    (** [initialize p] initializes the current state with the propositional 
      formula [p]. *)

  val reset : unit -> unit
    (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
    (** [unchanged()] holds iff the configuration has not been changed since 
      the latest [initialize] or [reset]. *)

  val is_valid : unit -> bool
    (** Current configuration is valid. *)

  val current : unit -> t
    (** [current()] returns the current configuration. *)

  exception Unsat
    (** Exception [Unsat] is raised whenever the current configuration 
      becomes unsatisfiable. *)

  val processConjoin : t -> unit
    (** [processConjoin p] conjoins [p] with the current configuration. *)

  val processUnion : var -> var -> unit  
    (** [processUnion x y] conjoins the equivalence [x <=> y] with the 
      current configuration. *)

  val processSeparate : var -> var -> unit 
    (** [processSeparate x y] conjoins the exclusive or [x  # y] with 
      the current configuration. *)

  val processSub : var -> var -> unit 
    (** [processSub x y] conjoins the implication [x => y] with the 
      current configuration. *)

  val propagateValid : var -> unit
    (** Whenever validity of the propositional variable [x] is derived 
      in the interface,  [propagateValid x] needs to be called. This 
      operation replaces every occurrence of [x] in the current configuration 
      with the trivially valid formula [mk_true]. *)

  val propagateUnsat : var -> unit  
    (** Whenever unsatisfiability of the propositional variable [x] is 
      derived in the interface, [propagateUnsat x] needs to be called. 
      This operation replaces every occurrence of [x] in the current 
      configuration with the trivially unsatisfiable formula [mk_false]. *)
end

(** Construction of a closed inference system from a representation of 
  propositional variables [Var], propositional formulas [Prop], and an 
  interface inference system [I]. *)
module Make
  (Var: PROPVAR)
  (Prop: PROP with type var = Var.t)
  (I: INTERFACE with type var = Var.t)
  : (INFSYS with type var = Var.t 
	    and type t = Prop.t)
  
