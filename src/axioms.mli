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

(** Rewriting and forward chaining.

  @author Harald Ruess

  This module defines data structures for representing {i rewrites}
  and {i forward chaining rules}.  These {i axioms} are compiled
  into a normalizinig function and a disequality test.
*)

exception Bot

module Subst : (Map.S with type key = Name.t)
  (** Substitutions map names of logical variables to terms. *)

(** Terms with logical variables only *)
module Lterm : sig
  type t = 
    | Var of Name.t
    | App of Funsym.t * t list

  val mk_var : string -> t
  val mk_app : Funsym.t -> t list -> t
	
  val pp : Format.formatter -> t -> unit
    
  val vars : t -> Name.Set.t  

  val matcher : t -> Term.t -> Term.t Subst.t -> Term.t Subst.t
end
  

(** Atoms over terms with logical variables. *)
module Atom : sig
  type t = 
    | Equal of Lterm.t * Lterm.t
    | Diseq of Lterm.t * Lterm.t
	
  val pp : Format.formatter -> t -> unit

  val mk_equal : Lterm.t -> Lterm.t -> t
  val mk_diseq : Lterm.t -> Lterm.t -> t
    
  val vars : t -> Name.Set.t
end 
  
(** A {i forward chaining rule} is of the form [n: h1,...,hn ==> c] with
  - [n] the name of the rule,
  - [h1],...,[hn] are the hypotheses, and
  - [c] is the conclusion. 
  Logical variables are assumed to be universally quantified,
  and the variables of [c] are a subset of the variables of all hypothesis. *)
module Chain : sig

  type t
    (** Representation of rules. *)
      
  val make: Name.t -> Atom.t list -> Atom.t -> t
    (** Create a rule from its elements. *)
    
  val mk_equal : Name.t -> Atom.t list -> Lterm.t -> Lterm.t -> t
    (** [mk_equal n hl a b] creates a rule [n: hl ===> a = b] with an
      equality conclusion. *)

  val mk_diseq : Name.t -> Atom.t list -> Lterm.t -> Lterm.t -> t
    (** [mk_diseq n hl a b] creates a rule [n: hl ===> a <> b] 
      with a disequality in the conclusion. *)
    
  val hyps : t -> Atom.t list
    (** Get the hypotheses of a rule. *)

  val concl : t -> Atom.t
    (** Get the conclusion of a rule. *)
    
  val pp : Format.formatter -> t -> unit  
    (** Pretty-printing a chaining rule. *)
end

(** A {i rewrite rule} is of the form [n: h1,..., hn --> f(a1,...,an) = b] with
  - [n] a name for the rule,
  - [h1],...,[hn] a set of hypotheses, and 
  - [f(a1,...,an) = b] the conclusion.
  It is assumed that the variables of [f(a1,...,an)] are a superset of the variables of
  [b] and the variables in the hypotheses. *)
module Rewrite : sig

  type t
    (** Representation of rewrite rules. *)
    
  val hyps : t -> Atom.t list
    (** Get hypotheses of a rewrite rule. *)

  val lhs : t -> Lterm.t
    (** Get left-hand side of conclusion. *)

  val rhs : t -> Lterm.t
    (** Get right-hand side of conclusion. *)
    
  val make : Name.t -> Atom.t list -> Funsym.t * Lterm.t list -> Lterm.t -> t
    (** Create a rewrite rule from its elements. *)
    
  val pp : Format.formatter -> t -> unit
    (** Pretty-print rewrite rules. *)
    
  val to_chain : t -> Chain.t
    (** Transform rewrite rule into a chaining rule. *)


end

(** Finite set of rewrites and chains. *)
module type AXIOMS = sig
  val rewrites : Rewrite.t list
  val chains : Chain.t list
end

(** Compilation of axioms into a normalizing function and a disequality test.
  It is assumed that the rewrites of [A] are {i canonical}, that is 
  {i strongly normalizing} and {i Church-Rosser}. *)
module Compile(A: AXIOMS) : sig
  
  val normalize : Term.interp
    (** [normalize f [a1;...;an]] reduces to [b] iff [f(a1,...,an)]
      rewrites to [b] using rewrite rules in [A]. *)
    
  val is_diseq : Term.t -> Term.t -> bool
    (** [is_diseq a b] holds iff [a<>b] follows from the axioms [A]. *)
    
end 

(** Compiled representation of a forward chaining rule. *)
module FlatChain : sig
      
  type t = Name.t * hyps * concl
  and hyps = hyp list
  and hyp = 
    | HypEqual of var * term
    | HypDiseq of var * var  
  and concl = 
    | ConclEqual of term * term
    | ConclDiseq of term * term  
  and var = Name.t
  and term =
    | Var of var
    | App of Funsym.t * var list

  val pp : Format.formatter -> t -> unit

  val to_chain : t -> Chain.t

  val of_chain : Chain.t -> t

  val of_chains : Chain.t list -> t list

  val reorder : Name.t list -> hyp list -> hyp list
	    
end 
