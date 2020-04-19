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

(** {i Open inference system for renaming monadic predicates.}

  This module provides an open inference system for renaming monadic 
  predicates [p(x)], where [p] is a predicate symbol and [x] a term, 
  and variable equalities[x = y] with propositional variables. 

  @author Harald Ruess
*)


(** {i Propositional variables.} *)
module type PROPVAR = sig
  type t
    (** Representation of propositional variables. *)

  val equal : t -> t -> bool
    (** Equality relation. *)

  val compare : t -> t -> int
    (** [compare u v] is [0] iff [equal u v] holds. Furthermore,
      [compare u v > 0] iff [compare v u < 0]. *)

  val hash : t -> int
    (** Nonnegative hash value of a variable. *)

  val pp : Format.formatter -> t -> unit
    (** Pretty-printing a variable. *)

  val fresh : unit -> t
    (** Generate a {i fresh} variable. *)
end

(** {i Monadic predicate symbols.} *)
module type PREDSYM = sig
  type t
    (** Represetnation of monadic predicate symbols. *)

  val equal : t -> t -> bool
    (** Equality relation. *)

  val compare : t -> t -> int
    (** [compare p q] is [0] iff [equal p q] holds. Furthermore,
      [compare p q > 0] iff [compare q p < 0]. *)

  val hash : t -> int
    (** Nonnegative hash value of a predicate symbol. *)

  val pp : Format.formatter -> t -> unit
    (** Printing a predicate symbol on the given formatter. *)

  val sub : t -> t -> bool
    (** [sub p q] holds iff [forall x. p(x) => q(x)] is valid. *)

  val disjoint : t -> t -> bool
    (** [disjoint p q] holds iff [forall x, y. not(p(x) & q(y))] is valid. *)
end

(** {i Term variables.} *)
module type VAR = sig
  type t
    (** Representation of term variables. *)

  val equal : t -> t -> bool
    (** Equality relation. *)

  val compare : t -> t -> int
    (** [compare x y] is [0] iff [equal x y] holds. Furthermore,
      [compare x y > 0] iff [compare y x < 0]. *)

  val hash : t -> int
    (** Nonnegative hash value of a predicate symbol. *)

  val pp : Format.formatter -> t -> unit
    (** Printing a predicate symbol on the given formatter. *)
end
  

(** {i Interface of renaming inference system.} 
  The interface for the open {i renaming} inference 
  system has logical contexts [(E,D,P,L)] with 
  - [E] a set of variable equalities,
  - [D] a set of variable disequalities,
  - [P] a set of propositional formulas,
  - [L] a set of monadic applications [p(x)] and negations thereof.
  [E] induces an equivalence relation [=E] with [x =E y] iff [E |=x = y],
  that is [x = y] is valid in [E]. The equivalence class modulo [E] 
  containting [x] is denoted by [E[x]]. *)
module type INTERFACE = sig
  type propvar
    (** Representation of propositional variables. *)

  type predsym
    (** Representation of monadic function symbols. *)

  type var
    (** Representation of term variables. *)

  val find : var -> var
    (** [find x] returns a canonical representative [y] of [E[x]] *)

  val canonical : var -> bool
    (** [canonical x] holds iff [x] is the canonical representative of [E[x]]*)

  val equal : var -> var -> bool
    (** [equal x y] holds iff [E |= x = y]. *)

  val diseq : var -> var -> bool
    (** [diseq x y] holds iff [E,D |= x <> y]. *)

  val equiv : propvar -> propvar -> unit
    (** [equiv u v] adds the equivalence constaint [u<=>v] to [P]. *)

  val disjoint : propvar -> propvar -> unit 
    (** [disjoint u v] conjoins the exclusive or constraint [u#v] to [P]. *)

  val implies : propvar -> propvar -> unit
    (** [implies u v] adds the implication [u=>v] to [P]. *)

  val valid0 : propvar -> unit
    (** [valid0 u] adds the propositional variable [u] to [P]. *)

  val unsat0 : propvar -> unit
    (** [unsat0 u] adds the negation [~u] of the 
      propositional variable [u] to [P]. *)

  val valid1 : predsym -> var -> unit
    (** [valid1 p x] adds the monadic constraint [p(x)] to [L]. *)

  val unsat1: predsym -> var -> unit 
    (** [valid1 p x] adds the negated monadic constraint [~p(x)] to [L]. *)

  val union : var -> var -> unit
    (** [union x y] adds the equality [x = y] to [E]. *)

  val separate : var -> var -> unit
    (** [separate x y] adds the disequality [x <> y] to [E]. *)
end


(** {i Renaming inference system.} 
  Configurations of the {i renaming} inference system consist of
  a finite set of bindings of the form [u |-> p(x)] and [v |-> y = z],
  with [u], [v] propositional variables, [p] a monadic predicate symbol,
  and [x],[y],[z] term variables. For a description of the interface
  configuration [(E,D,P,L)] see above. The inference system progresses by
  updating a {i current configuration}. *)
module type INFSYS = sig
  type propvar 
    (** Representation of propositional variables. *)

  type predsym
    (** Representation of monadic predicate symbols. *)

  type var 
    (** Representation of term variables. *)

  type t
    (** Representation of renaming configurations. *)

  val empty : t 
    (** The empty renaming configuration. *)

  val initialize : t -> unit
    (** [initialize s] initializes the {i current configuration} 
      with [s]. *)

  val reset : unit -> unit
    (** [reset()] is synonymous with [initialize empty]. *)

  val unchanged : unit -> bool
    (** [unchanged()] holds iff the current configuration
      has not been modified since the last [initialize] or [reset]. *)

  val is_empty : unit -> bool
    (** Test if current renaming map is empty. *)

  val current : unit -> t 
    (** [current()] returns the current configuration. *)

  module Monadic : (Maps.S with type key=propvar and type value = predsym*var) 

  module Equal : (Maps.S with type key = propvar and type value = var*var)

  val monadic: unit -> Monadic.t
    (** Return all current bindings of the form [u |-> p(x)]. *)

  val equal : unit -> Equal.t
    (** Return all current bindings of the form [u |-> x = y]. *)

  val aliasMonadic : predsym -> var -> propvar
    (** [aliasMonadic p x] returns [u] if there is a [y]
      with [x =E y] such that [u |-> p(y)]; otherwise the
      current configuration is extended with a binding [u |-> p(x)]
      where [u] a fresh propositional variable. *)

  val aliasEqual : var -> var -> propvar
    (** [aliasEqual x y] returns [u] if there is a [x'],[y']
      with [E |= x=y <=> x'=y'] such that [u |-> x' = y']; 
      otherwise the current configuration is extended with such 
      a binding [u |-> x = y] where [u] a fresh propositional variable. *)

  val propagateEq : var -> var -> unit
    (** [propagateEq x y] propagates new variable equalities [x = y]
      in [E] with [y] canonical in [E]. Whenever such a new equality 
      is derived, it is assumed that [propagateEq x y] is called before 
      any other function in this interface. [propagateEq] adds
      newly derived constraints to the interface configuration. 
      - [x=Ey, u|->p(x'), v|->p(y'), x=Ex', y=Ey' ==> u<=>v]
      - [x=Ey, u|->p(x'), v|->q(y'), disjoint(p,q),x=Ex', y=Ey' ==> u#v]
      - [x=Ey, u|->p(x'), v|->q(y'), sub(p,q),x=Ex', y=Ey' ==> u=>q]
      - [u|->x'=y', v|->x''=y'', x'=Ex'', y'=Ey'' ==>  u<=>v]. *)

  val propagateDeq : var -> var -> unit
    (** [propagateDeq x y] is called by the interface for newly
      derived disequalities [x <> y] and is used to derive new
      constraints from the renaming context as follows:
      - [x<>y, u |-> x'=y', E |= (x'=y')<=>(x=y) ==> ~u] *)

  val propagateValid0 : propvar -> unit
    (** [propagateValid0 u] is called by the interface for newly
      derived valid propositional variables [u]. It is used to
      derive new constraints from the renaming context as follows:
      - [u, u |-> p(x) ==> p(x)]
      - [u, u |-> x = y => x = y] *)

  val propagateUnsat0 : propvar -> unit
    (** [propagateUnsat0 u] is called by the interface for newly
      derived unsatisfiable propositional variables [u]. It is used 
      to derive new constraints from the renaming context.
      - [~u, u |-> p(x) ==> ~p(x)]
      - [~u, u |-> x = y => x <> y] *)

  val propagateValid1 : predsym -> var -> unit
    (** [propagateValid1 p x] is called by the interface for newly
      derived monadic constraints [p(x)]. It is used to derive new 
      constraints from the renaming context as follows:
      - [p(x), u |-> q(y), x =E y, implies(q,p) ==> u] *)

  val propagateUnsat1 : predsym -> var -> unit
    (** [propagateUnsat1 p x] is called by the interface for newly
      derived monadic constraints [~p(x)]. It is used to derive new 
      constraints from the renaming context as follows:
      - [~p(x), u |-> q(y), x =E y, implies(q,p) ==> ~u] *)
end


(** Construction of a closed renaming inference system from
  an implementation [Propvar] of propositional variables,
  [Sym] of predicate symbols, [Var] of term variables, and 
  an [Interface] inference system. 

  The interface must fulfill the following requirements:
  - For each newly derived facts [u], [~u], [p(x)], [~p(x)],
  [x = y], [x <> y] the corresponding propagator must be
  called at least once to guarantee completeness.
  - In addition, propagation of a variable equality [x = y]
  must be {i synchronized} as to ensure the corresponing
  [propagateEq x y] call is performed before any other function
  of the renaming interface system is used and before another
  equality is added in the environment itself. This stringent
  requirement ensure certain invariants of internal indices.
  - The interface must ensure that all functions of the
  renaming interface are {i executed atomically}. In 
  particular, an outcall of an interface functionality may not 
  trigger  another call of a renaming inference system functionality
  before the original call to the renaming system is not completed. *)
module Make(Propvar: PROPVAR)(Sym: PREDSYM)(Var: VAR)
  (Interface: INTERFACE with type propvar = Propvar.t
			and type predsym = Sym.t
			and type var = Var.t)
  : (INFSYS with type propvar = Propvar.t
	    and type predsym = Sym.t
	    and type var = Var.t)
  
