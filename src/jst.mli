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

(** Justifications

  @author Harald Ruess
*)

class type proof = 
   object
     method pp : Format.formatter -> unit
     method assumptions : Atom.Set.t
     method validate : bool
   end

class type eq = 
  object
   inherit proof
   method lhs : Term.t
   method rhs : Term.t
  end

class type diseq =
  object
   inherit proof
   method lhs : Term.t
   method rhs : Term.t
  end

class type cnstrnt =
  object
   inherit proof
   method arg : Term.t
   method cnstrnt : Cnstrnt.t
  end

class axiom : Atom.t -> proof

class reflexivity : Term.t -> eq
  (** [refl a |- a = a]. *)

class symmetry : eq -> eq
  (** If [sym p |- a = b] then [sym p |- b = a]. *)

class transitivity : eq -> eq -> eq
  (** If [p1 |- a = b] and [p2 |- b = c], then [trans p1 p2 |- a = c]. *)

class join : eq -> eq -> eq
  (** If [p1 |- a = c] and [p2 |- b = c], then [join p1 p2 |- a = c].
    Although [join] can be expressed in terms of [sym] and [trans], it is
    usueful for producing small sets of assumptions from equational valley proofs. *)

class congruence : Funsym.t -> eq list -> eq
  (** If [p{i} |- a{i} = b{i}] for [i = 1,...,n], then
    [cong f [p{1};...;p{n}] |- f(a{1},...,a{n}) = f(b{1},...,b{n}). *)

class functionality : int -> diseq -> diseq
  (** If [p |- f(a{1},...,a{i-1},b{i},a{i+1},...,a{n}) <> 
               f(a{1},...,a{i-1},c{i},a{i+1},...,a{n})], 
    then [functionality i p |- b{i} <> c{i}]. *)

exception Unsat of proof


(** Every atom [atm] may have an associated {i justification} [rho]; in this
  case we also write [rho |- atm]. There are three different kinds of
  ju1stifications
  - [All] justifies every
  - A set of {i dependencies} [{atm1, ...,atmn}]
  - A proof skeleton (disabled in ICS 2.0)
  
  Depending on the current proof mode, as obtained with {!Jst.Mode.get},
  justifications of these kinds are generated.
*)

(** Proof Mode *)
module Mode : sig

  type t = No | Dep (* | Yes *)

  val of_string : string -> t
  val to_string : t -> string

  val is_none : unit -> bool

  val get : unit -> t
  val set : t -> unit

end


type t
   
val pp : t Pretty.printer

val axioms_of: t -> Atom.Set.t

exception Inconsistent of t
exception Valid of t


val axiom : Atom.t -> t

val dep0 : t

val dep1 : t -> t

val dep2 : t -> t -> t

val dep3 : t -> t -> t -> t

val dep4 : t -> t -> t -> t -> t

val dep5 : t -> t -> t -> t -> t -> t

val dep : t list -> t

val xor : t -> t -> t

type jst = t
    (** Nickname. *)


(** Justifying ternary relations. *)
module Three : sig

  type t =
    | Yes of jst
    | No of jst
    | X

end


(** Justifying equality Transformers *)
module Eqtrans : sig
  type t = Term.t -> Term.t * jst

  val id : t
    (** [id a] returns [(a, rho)] such that [rho |- a = a]. *)

  val compose : t -> t -> t
    (** If [g a = (b, rho)] and [f b = (c, tau)] with [rho |- a = b]
      and [tau |- b = c], then [compose f g a] returns [(c, sigma)]
      with [sigma |- a = c]. *)

  val totalize : t -> t

  val trace : Trace.level -> string -> t -> t
end

module Doprint : Ref.BOOLEAN

module Equal : sig

  type t = Term.t * Term.t * jst
     
  val pp : Format.formatter -> t -> unit

  val is_var : t -> bool

  val is_pure : Theory.t -> t -> bool

  val both_sides: (Term.t -> bool) -> t -> bool

  module Set : (Sets.S with type elt = t)
    (** Set of equalities. *)

end 


module Diseq : sig

  type t = Term.t * Term.t * jst
      
  val pp : Format.formatter -> t -> unit

  val is_var : t -> bool

  val is_pure : Theory.t -> t -> bool

 val both_sides: (Term.t -> bool) -> t -> bool

  module Set : (Sets.S with type elt = t)
    (** Set of disequalities *)

end 

module Ineq: sig

  type t = Term.t * Term.t * jst
      
  val pp : Format.formatter -> t -> unit

end 


module Cnstrnt : sig

  type t = Term.t * Cnstrnt.t * jst
      
  val pp : Format.formatter -> t -> unit

  module Set : (Sets.S with type elt = t)
    (** Set of disequalities *)

end 


module Nonneg : sig

  type t = Term.t * jst
      
  val pp : Format.formatter -> t -> unit

end 



module Fact : sig

  type t = Atom.t * jst

  val pp : Format.formatter -> t -> unit

  val eq : t -> t -> bool

  val replace : Equal.t -> t -> t

  val of_equal : Equal.t -> t


end
  

  
(** Set of pairs [(a, rho)] with [a] a term and [rho] a justification.
  These sets are used in the codomain of nonfunctional equality sets below. *)
module Terms : sig

  type elt = Term.t * t
  
  type t
    (** The type of sets. *)

  val pp : Format.formatter -> t -> unit

  val empty: t
    (** The empty set. *)

  val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

  val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)
    
  val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
    
  val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)
    
  val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)
    
  val union: t -> t -> t
    (** Set union. *)
    
  val inter: t -> t -> t
    (** Set intersection. *)
    
  val diff: t -> t -> t  
    (** Set difference. *)
    
  val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
      equal, that is, contain equal elements. *)

  val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
      the set [s2]. *)
    
  val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
      The order in which the elements of [s] are presented to [f]
      is unspecified. *)
    
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s].
      The order in which elements of [s] are presented to [f] is
      unspecified. *)

  val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
      satisfy the predicate [p]. *)

  val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)
        
  val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
      that satisfy predicate [p]. *)

  val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
      [s1] is the set of all the elements of [s] that satisfy the
      predicate [p], and [s2] is the set of all the elements of
      [s] that do not satisfy [p]. *)

  val cardinal: t -> int
    (** Return the number of elements of a set. *)

  val elements: t -> elt list
    (** Return the list of all elements of the given set.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Set.Make}. *)

  val min_elt: t -> elt
    (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the set is empty. *)

  val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
      given set. *)

  val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
      the set is empty. Which element is chosen is unspecified,
      but equal elements will be chosen for equal sets. *)

  val choose_from : (elt -> bool) -> t -> elt

  val choose_eq : Term.t -> t -> jst
	
end 


