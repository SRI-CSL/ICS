(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(** Solution sets.

  @author Harald Ruess
  @author N. Shankar

  A {b solution set} is a set of equalities of the form [x = a],
  where [x] is term variable. For each such equality, a 
  {i justification} [rho] of type {!Justification.t} is 
  stored.

  As an invariant, solution sets [s] are kept in {i functional} form, 
  that is, if [x = a] and [x = b] in [s], then [a] is identical with [b]. 
  In addition, solution sets are {i injective}, that is, [x = a] and [y = a] 
  are not in a solution set for [x <> y].

  An equality set might have additional {i indices}. These are
  sets of dependent variables [x = a] such that [a] satisfies
  a specified constraint.

  A {i constant index} is special in that disequalities [x <> y]
  are generated for [x = c], [y = d] and [c], [d] two disequal
  constants.
*)


(** An {i equality theory} is specified by means of
  - its name [th],
  - a [nickname] for the theory,
  - an application [apply e a] of equality [e] of the form [x = b] to [a];
  that is, all [x] in [a] are replaced by [b], and the resulting term is
  canonized. *)
module type TH = sig
  val th : Th.t
  val nickname : string
  val apply : Term.Equal.t -> Term.t -> Term.t
end

type effects = 
    (equality -> unit) * 
    (equality -> unit)

and equality = Term.t * Term.t * Justification.t 

val effects0 : effects

val pp_index : bool ref

(** Signature for equality sets. *)
module type SET = sig
  type t 
    (** Representation of a set of equalities of the form [x = a],
      where [x] is a variable and [a] a non-variable term. Furthermore,
      this set is {i functional} in that, whenever [x = a] and [y = b],
      then [x] equals [y]. *)
    
  val eq : t -> t -> bool
    (** Test for identity of two solution sets. *)

  val pp : t Pretty.printer
    (** Pretty-printing a solution set. If {!Eqs.pp_index} is set
      to [true], then the {i dependency index} is printed, too. *)

  val empty : t
    (** The empty equality set. *)

  val is_empty : t -> bool
    (** [is_empty s] holds iff [s] does not contain any equalities. *)

  val is_dependent : t -> Term.t -> bool
    (** [is_dependent s x] holds iff there is an [a] such that [x = a] is in [s]. *)

  val is_independent : t -> Term.t -> bool
    (** [is_independent s x] holds iff [x] is a variable in some [a] with
      [y = a] in [s]. *)

  val fold : (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s e] applies [f x (a, rho)] for each [x = a] with justification
      [rho] in [s] and accumulates the result starting with [e]. The order of
      application is unspecified. *)

  val to_list : t -> (Term.t * Term.t) list
    (** [to_list s] returns the equality set [s] as a list of pairs [(x, a)]
      such that [x = a] is in [s]. *)

  val apply : t -> Justification.Eqtrans.t
    (** [apply s x] yields [(b, rho)] if [x = b] in [s] with justification [rho];
      if [x] is not a dependent variable, [Not_found] is raised. *)

  val equality : t -> Term.t -> Fact.Equal.t
    (** [equality s x] yields an equality [e] of the form [x = a] with
      justification [rho] if [x] is a {i dependent} variable. Otherwise,
      [Not_found] is raised. *)

  val find : t -> Justification.Eqtrans.t
    (** [find s x] yields [(b, rho)] if [x] is a dependent variable in [s]
      with [x = b]; otherwise, [(x, refl)] is returned with [refl] a
      justification of [x = x]. *)

  val inv : t -> Justification.Eqtrans.t 
    (** [inv s a] yields [(x, rho)] if [x = a] is in [s] with justification [rho]. *)

  val dep : t -> Term.t -> Term.Set.t
    (** [dep s y] returns all [x] such that [x = a] in [s], and 
      the variable [y] occurs in [a]. In this case, we also say
      that [x] is {i dependent} on [y]. *)

  val index : t -> int -> Term.Set.t
    (** If defined, [index s i] returns the [i]th {i index} for the 
      equality set [s]. Otherwise, [Invalid_argument] is raised. *)

  val cnstnt : t -> Term.Set.t
    (** If defined, [cnstnt s] returns the constant {i index} for the 
      equality set [s]. Otherwise, [Invalid_argument] is raised. *)

    (** Iterators over {i dependency} index. *)
  module Dep : sig
    val iter : t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
      (** [iter s f y] applies [f] to each equality [e] of the
	form [x = a] with justification [rho] such that [x]
	is {i dependent} on [y]. *)

    val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
      (** [fold s f y e] accumulates, starting with [e], applications 
        of [f] to each equality [x = a] which [x] dependent on [y]. *)

    val for_all : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
      (** [for_all s p y] holds iff [p e] holds for all equalities
	[x = a] with [x] dependent on [y]. *)

    val exists : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
      (** [exists s p y] holds iff [p e] holds for all equalities
	[x = a] with [x] dependent on [y]. *)

    val choose : t -> Term.t -> Fact.Equal.t
      (** [choose s y] returns equality [e] of the form [x = a]
	if [x] is dependent on [y]; otherwise, [Not_found] is raised. *)
  end

  type config = Partition.t * t

  val copy : t -> t
    (** [copy s] returns a {i shallow copy} of [s] for 
      protecting [s] against {i destructive} updates of any
      of the following update functions. *)
    
  val name:  effects -> config -> Justification.Eqtrans.t
    (** [name (f, g) (p, s) a] returns a variable [x] such 
      that [x = a] in [s]. If there is no such [x], a
      {i fresh} variable [v] is created, and [v = a] is
      added to [s].  In updating the state, {i effect} 
      functions [(f, g)] are called when updating the state. *)
    
  val update : effects -> config -> Fact.Equal.t -> unit
    (** [update (f, g) (p, s) e] updates [s] with a 
      new equality of the form, say, [x = a]. Any [x = b]
      already in the state, is removed. *)
    
  val restrict : effects -> config -> Term.t -> unit
    (** [restrict (f, g) (p, s) x] removes equalities of
      the form [x = a] in [s]. *)

  val fuse: effects -> config -> Fact.Equal.t list -> unit
    (** [fuse (f, g) (p, s) el] propagates the equalities [x = a]
      in [el] in the rhs [b] of all [y = b] in [s] by substituting
      [x] in [b]  by [a]. *)

  val compose : effects -> config -> Fact.Equal.t list -> unit
    (** [compose (f, g) (p, s) el] is similar to [fuse] in that
      the equalities in [el] are propagated to the rhs of the
      equalities in [s]. In addition, the equalities [el] are
      added to the solution set, possibly overwriting other
      equalities. *)
    
end


(** {6 Equality Theories} *)

module Make(Th: TH): SET
  (** Functor for constructing an equality set for theory {!Eqs.TH.th}. 
    {!Eqs.TH.apply} is used to propagate equalities in {!Eqs.Set.fuse}
    and {!Eqs.Set.compose}, and {!Eqs.TH.disapply} is used to propagate
    a disequality in {!Eqs.TH.fission}.  The {i effects} functions [(f, g)]
    in the {i update} operators are called when updating the state, namely,
    [f] is called whenever a new equality is added, and [g] whenever
    an equality is removed or overwritten. Also, if the trace level
    {!Eqs.Th.nickname} is enabled, a trace message is displayed
    using {!Trace.msg}, whenever the a new equality is added or
    removed. Otherwise, there are no side effects on equality sets. 
    The accessors [index] and [cnstnt] always raise [Invalid_argument]. *)


(** {6 Equality Theories with indices} *)

  (** Specification of indices [0] through [max-1] *)
module type INDEX = sig
  val max : int       
  val name: int -> string
  val holds : int -> Term.t -> bool
end

module MakeIndex(Th: TH)(Idx : INDEX): SET
  (** Given a theory [Th] and a specification of indices [0] through
    [Idx.max], this functor constructs an equality set that {i extends}
    {!Eqs.Make}[(Th)] with [max] indices, whereby [index i] coincides
    with the set of dependent variables [x] such that [x = a] in [s] and
    [Idx.holds i a] holds. The [i]th index can be obtained through
    the accessor [index i], whereas the accessor [cnstnt] is undefined. *)


(** {6 Solution sets with constant index} *)

  (** Specification of a constant index. *)
module type CNSTNT = sig
  val is_const : Term.t -> bool
  val is_diseq : Term.t -> Term.t -> bool
end 

module MakeCnstnt(Th: TH)(C: CNSTNT): SET
  (** Given a theory [Th] and a constant specification [C],
    this functor constructs an equality set that {i extends}
    {!Eqs.Make}[(Th)] with a {i constant index} [cnstnt s],
    which collects all dependent variables [x] such that [x = a]
    and [C.is_const a] holds. In addition, whenever [x = a], 
    [y = b] with [a], [b] constants such that [C.is_diseq a b],
    then the disequality [x <> y] is generated. *)


(** {6 Equality set with constant index and other indices} *)

module MakeIndexCnstnt(Th: TH)(Idx: INDEX)(C: CNSTNT): SET
  (** Combines the extensions of equality sets as specified
    by {!eqs.MakeIndex} and {!Eqs.MakeCnstnt}. *)


module type INDEX1 = sig
  val name : string
  val holds : Term.t -> bool
end

module MakeIndexCnstnt1(Th: TH)(Idx1: INDEX1)(C: CNSTNT): SET
  (* Same as {!MakeIndexCnstnt} but only one index. *)



(** {6 Side-effect free equality sets} *)

  (** Same as {!Eqs.SET} but update functions are side-effect free. *)
module type SET0 = sig
  type t 
  type ext
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val is_dependent : t -> Term.t -> bool
  val is_independent : t -> Term.t -> bool
  val fold : (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list : t -> (Term.t * Term.t) list
  val apply : t -> Justification.Eqtrans.t
  val equality : t -> Term.t -> Fact.Equal.t
  val find : t -> Justification.Eqtrans.t
  val inv : t -> Justification.Eqtrans.t 
  val dep : t -> Term.t -> Term.Set.t
  val index : t -> int -> Term.Set.t
  val cnstnt : t -> Term.Set.t
  val ext : t -> ext
  module Dep : sig
    val iter : t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold : t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists : t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose : t -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:  config -> Justification.Eqtrans.t
  val update : config -> Fact.Equal.t -> unit
  val restrict : config -> Term.t -> unit
  val fuse: config -> Fact.Equal.t list -> unit
  val compose : config -> Fact.Equal.t list -> unit
end

module Close(Set: SET) : SET0 with type ext = unit
  (** Construct an equality set with no further side effects. *)

module type EXT = sig
  type t
  type ext
  val empty : ext
  val pp : ext Pretty.printer
  val eq : ext -> ext -> bool
  val do_at_add :  Partition.t * ext * t -> equality -> ext
  val do_at_restrict : Partition.t * ext * t -> equality -> ext
end

module Extend(Set: SET)(Ext: EXT with type t = Set.t)
: SET0 with type ext = Ext.ext
(** Add side effects [Ext] to obtain an equality set with no further side effects. *)


(** {6 Combining two equality sets} *)


type tag = Left | Right

val other : tag -> tag

module type EFFECTS2 = sig
  type left
  type right

  val do_at_update : tag -> Partition.t * left * right -> equality -> unit
  val do_at_restrict : tag -> Partition.t * left * right -> equality -> unit
end

(** Combining two equality sets [Left] and [Right] and close with the
  specified effects. Most operators work component-wise and are parameterized
  with respect to {!Eqs.tag}. *)
module Union
  (Left: SET)
  (Right: SET)
  (Effects2: EFFECTS2 with type left = Left.t with type right = Right.t) : 
sig
  type t 
  val eq : t -> t -> bool
  val pp : t Pretty.printer
  val empty : t
  val is_empty : t -> bool
  val is_dependent : tag -> t -> Term.t -> bool
  val is_independent :  tag -> t -> Term.t -> bool
  val fold :  tag -> (Term.t -> Term.t * Justification.t -> 'a -> 'a) -> t -> 'a -> 'a
  val to_list :  tag -> t -> (Term.t * Term.t) list
  val apply :  tag -> t -> Justification.Eqtrans.t
  val equality :  tag -> t -> Term.t -> Fact.Equal.t
  val find :  tag -> t -> Justification.Eqtrans.t
  val inv :  tag -> t -> Justification.Eqtrans.t 
  val dep :  tag -> t -> Term.t -> Term.Set.t
  val index :  tag -> t -> int -> Term.Set.t
  val cnstnt :  tag -> t -> Term.Set.t
  module Dep : sig
    val iter :  tag -> t -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    val fold :  tag -> t -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    val for_all :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val exists :  tag -> t -> (Fact.Equal.t -> bool) -> Term.t -> bool
    val choose :  tag -> t -> Term.t -> Fact.Equal.t
  end
  val copy : t -> t
  type config = Partition.t * t
  val name:   tag -> config -> Justification.Eqtrans.t
  val update :  tag -> config -> Fact.Equal.t -> unit
  val restrict :  tag -> config -> Term.t -> unit
  val fuse:  tag -> config -> Fact.Equal.t list -> unit
  val compose :  tag -> config -> Fact.Equal.t list -> unit
end
