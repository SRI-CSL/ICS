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

  A {b solution set} for theory [th] is a set of equalities of the 
  form [x = a], where [x] is term variable and [a] is a [th]-pure
  term application. For each such equality, a {i justification} [rho] 
  of type {!Jst.t} is maintained.

  As an invariant, solution sets [s] are kept in 
  - {i functional} form, that is, if [x = a] and [x = b] in [s], 
  then [a] is identical with [b], and
  - solution sets are {i injective}, that is, [x = a] and [y = a] 
  are not in a solution set for [x <> y].

  @author Harald Ruess
  @author N. Shankar
*)

val pp_index : bool ref


(** Iterators over {i dependency} index. *)
module type DEP = sig
  type eqs
  val iter : eqs -> (Fact.Equal.t -> unit) -> Term.t -> unit 
    (** [iter s f y] applies [f] to each equality [e] of the
      form [x = a] with justification [rho] such that [x]
      is {i dependent} on [y]. *)
    
  val fold : eqs -> (Fact.Equal.t -> 'a -> 'a) -> Term.t -> 'a  -> 'a
    (** [fold s f y e] accumulates, starting with [e], applications 
      of [f] to each equality [x = a] which [x] dependent on [y]. *)
    
  val for_all : eqs -> (Fact.Equal.t -> bool) -> Term.t -> bool
    (** [for_all s p y] holds iff [p e] holds for all equalities
      [x = a] with [x] dependent on [y]. *)
    
  val exists : eqs -> (Fact.Equal.t -> bool) -> Term.t -> bool
    (** [exists s p y] holds iff [p e] holds for all equalities
      [x = a] with [x] dependent on [y]. *)
    
  val choose : eqs -> (Fact.Equal.t -> bool) -> Term.t -> Fact.Equal.t
    (** [choose s y] returns equality [e] of the form [x = a]
      if [x] is dependent on [y]; otherwise, [Not_found] is raised. *)
end


(** Signature for equality sets. *)
module type SET = sig
  type t 
    (** Representation of a set of equalities of the form [rho |- x = a],
      where [x] is a variable, [a] a non-variable term, and [rho] is
      a justification of the equality [x = a]. *)

  type ext
    (** Extension field. *)
    
  val eq : t -> t -> bool
    (** Test for identity of two solution sets. *)

  val pp : t Pretty.printer
    (** Pretty-printing a solution set. If [Eqs.pp_index] is set
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

  val iter : (Fact.Equal.t -> unit) -> t -> unit
    (** [iter f s] applies [f e] for each equality fact [e] in [s].
      The order of application is unspecified. *)

  val fold : (Fact.Equal.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s acc] applies [f e] for each equality [e] in [s]
      and accumulates the result starting with [acc]. The order of
      application is unspecified. *)

  val for_all : (Fact.Equal.t -> bool) -> t -> bool
    (** [for_all f s] checks if [f e] holds for all equalities [e] in [s]. *)

  val exists : (Fact.Equal.t -> bool) -> t -> bool
    (** [exists f s] checks if [f e] holds for all equalities [e] in [s]. *)

  val to_list : t -> Fact.Equal.t list
    (** [to_list s] builds up a list of equalities from the solved form [s]. *)

  val equality : t -> Term.t -> Fact.Equal.t
    (** [equality s x] yields an equality [e] of the form [x = a] with
      justification [rho] if [x] is a {i dependent} variable. Otherwise,
      [Not_found] is raised. *)

  val apply : t -> Jst.Eqtrans.t
    (** [apply s x] yields [(b, rho)] if [x = b] in [s] with justification [rho];
      if [x] is not a dependent variable, [Not_found] is raised. *)

  val find : t -> Jst.Eqtrans.t
    (** [find s x] yields [(b, rho)] if [x] is a dependent variable in [s]
      with [x = b]; otherwise, [(x, refl)] is returned with [refl] a
      justification of [x = x]. *)

  val inv : t -> Jst.Eqtrans.t 
    (** [inv s a] yields [(x, rho)] if [rho |- x = a] is in [s] with 
      justification [rho]. *)

  val dep : t -> Term.t -> Term.Var.Set.t
    (** [dep s y] returns all [x] such that [x = a] in [s], and 
      the variable [y] occurs in [a]. In this case, we also say
      that [x] is {i dependent} on [y]. *)

  val ext : t -> ext
    (** Return the value of the extension field. *)

  module Dep : (DEP with type eqs = t)
    (** Iterators over dependency index. *)
    
  val restrict : t -> Term.t -> unit
    (** [restrict s x] removes equalities of the form [x = a] in [s]. *)

  type config = Partition.t * t

  val update : config -> Fact.Equal.t -> unit
    (** [update s e] updates [s] with a 
      new equality of the form, say, [x = a]. 
      Any [x = b] already in the state, is removed. *)
  

  val diff : t -> t -> t
    (** [diff s1 s2] contains all equalities in [s1] that are not in [s2]. *)

  val copy : t -> t

    
end

(** An {i equality theory} is specified by means of
  - its name [th],
  - a term [map f a] for replacing uninterpreted positions [x] of [a]
  with [f x] and canonizing the resulting term in theory [th].
  - side effects when updating and restricting solution sets. *)
module type EXT = sig
  type t
  val pp: t Pretty.printer
  val empty : t
  val update : t -> Fact.Equal.t -> t
  val restrict : t -> Fact.Equal.t -> t
end

module Make(Ext: EXT): (SET with type ext = Ext.t)
  (** Functor for constructing an equality set for theory specification [T].
    Updates and restrictions have the respective side effects as before methods. *)
   


module type SET0 = (SET with type ext = unit)
  (** Solution set without field extension. *)

module Set: SET0
  (** Functor for constructing a solution set for theory specification [T0].
    The resulting solution set does not have side effects. *)

module Proj(S: SET): SET0
  (** Projecting out extensions of a solution set. *)
