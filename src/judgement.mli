(*
 * The contents of this file aresubject to the ICS(TM) Community Research
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


(** Judgements

  @author Harald Ruess

  A {i judgement} [p |- a] consists of proof [p] for an atom [a] of type {!Atom.t}.
*)

(** A {i judgement} is of the form [h{1},...,h{n} |- c] with {i hypotheses} [h{i}]
  and a conclusion [c]. *)
class type atom = object
  method concl : Atom.t
  method name : string
  method hyps : atoms
  method pp : Format.formatter -> unit
  method assumptions : Atom.Set.t -> unit
  method validate : bool
end

and atoms = object
  method to_list : atom list
  method assumptions : Atom.Set.t -> unit
  method pp : Format.formatter -> unit
  method validate : bool
end

class type equal = object
  inherit atom
  method lhs : Term.t
  method rhs : Term.t
end

class type diseq = object
  inherit atom
  method lhs : Term.t
  method rhs : Term.t
end

class type cnstrnt = object
  inherit atom
  method arg : Term.t
  method cnstrnt : Cnstrnt.t
end

class type valid = object
  inherit atom
end

class type unsat = object
  inherit atom
end

class type nonneg = object
  inherit atom
  method arg : Term.t
end

class type pos = object
  inherit atom
  method arg : Term.t
end

exception Unsat of unsat
exception Valid of atom

  
(** {6 Operators} *)

val pp : Format.formatter -> atom -> unit

val to_atom : atom -> atom

val to_unsat : atom -> unsat
val to_equal : atom -> equal
val to_diseq : atom -> diseq
val to_nonneg : atom -> nonneg
val to_pos : atom -> pos
val to_cnstrnt : atom -> cnstrnt

val validates : atom -> Atom.t -> bool

val validates_equal : equal -> Term.t -> Term.t -> bool

(** {6 Homogeneous sets of judgements} *)

module Set : (Sets.S with type elt = atom)


(** {6 Heterogeneous sets of judgements} *)

class virtual topatoms : 
  object
    method virtual to_list : atom list
    method virtual assumptions : Atom.Set.t -> unit
    method virtual pp : Format.formatter -> unit
    method virtual validate : bool
  end 

val mk_empty : atoms
val mk_add : atom -> atoms -> atoms
val mk_add_equal : equal -> atoms -> atoms
val mk_add_diseq : diseq -> atoms -> atoms
val mk_singleton : atom -> atoms
val mk_union : atoms -> atoms -> atoms
val of_list : atom list -> atoms
val of_equal_list : equal list -> atoms
val of_nonneg_list : nonneg list -> atoms
val of_cnstrnt_list : cnstrnt list -> atoms


(** {6 Atoms} *)

val mk_axiom : Atom.t -> atom
  (** [assumption atm |- atm]. *)

val mk_triv : valid
  (** [triv |- true]. *)

val mk_bot : diseq -> unsat
  (** If [d |- a <> a], then [bot d |- false]. *)

val mk_refl : Term.t -> equal
  (** [refl a |- a = a]. *)

val mk_sym : equal -> equal
  (** If [sym p |- a = b], then [sym p |- b = a]. *)

val mk_trans : equal -> equal -> equal
  (** If [p1 |- a = b] and [p2 |- b = c], 
    then [trans p1 p2 |- a = c]. *)

val mk_trans3 : equal -> equal -> equal -> equal
  (** If [e1 |- a = b] and [e2 |- b = c] and [e3 |- c = d],
    then [trans3 e1 e2 e3 |- a = d]. *)

val mk_contra : equal -> diseq -> unsat
  (** If [e |- a = b] and [d |- a <> b],
    then [contra e d |- false]. *)

val mk_join : equal -> equal -> equal
  (** If [p1 |- a = c] and [p2 |- b = c], 
    then [join p1 p2 |- a = b]. *)

val mk_alias : Term.t -> Term.t -> equal
  (** For a fresh variable [x], then [alias x a |- x = a]. *)

val mk_func : int -> diseq -> diseq
  (** If [p |- f(a{1},...,a{i-1},b{i},a{i+1},...,a{n}) <> 
               f(a{1},...,a{i-1},c{i},a{i+1},...,a{n})], 
    then [functionality i p |- b{i} <> c{i}]. *)

val mk_inter : cnstrnt -> cnstrnt -> cnstrnt
  (** If [p1 |- a in c] and [p2 |- a in d], 
    then [inter p1 p2 |- a in (c /\ d)]. *)

val mk_disjoint : cnstrnt -> cnstrnt -> diseq
  (** If [c1 |- a in c] and [c2 |- b in d] and [c], [d] are disjoint, 
    then [disjoint c1 c2 |- a <> b]. *)


(** {6 Replacements} *)

val mk_replace_in_atom : equal -> atom -> atom
  (** If [e |- s = t], then [replace_in_atom e a |- a[s:=t]],
    where the atome a[s:=t] may be further simplified according
    to {!Term.sigma}. *)


val mk_replace_in_equal : equal list -> equal -> equal
  (** If [e{i} |- a{i} = b{i}] and [e |- a = b]
    then [replace_in_equal [e{1};...;e{n}] e |- a' = b']. *)

val mk_replace_in_cnstrnt : equal -> cnstrnt -> cnstrnt
  (** If [e1 |- a = b] and [c2 |- a in c], 
    then [replace_in_cnstrnt e1 c2 |- b in c]. *)

val mk_replace_in_diseq : equal -> equal -> diseq -> diseq
  (** If [e1 |- a = a'] and  [e2 |- b = b'] and [d3 |- a <> b], 
    then [replace_in_diseq e1 e2 d3 |- a' <> b']. *)

val mk_replace_in_nonneg : equal -> nonneg -> nonneg
 (** If [e |- a = a'] and  [n |- a >= 0], then [replace_in_nonneg e n |- a' >= 0]. *)

val mk_replace_in_pos : equal -> pos -> pos
 (** If [e |- a = a'] and  [n |- a > 0], then [replace_in_nonneg e n |- a' > 0]. *)


(** {6 Transformations} *)

val mk_transform_equal : (Term.t -> equal) -> equal -> equal
  (** If [e |- t1 = t2],  [f t1 = e] with [e |- t1 = t1'], 
    and [f t1 = e] with [e |- t1 = t1'], then [transform_equal f e |- t1' = t2']. *)

val mk_transform_rhs_equal : (Term.t -> equal) -> equal -> equal
  (** If [e |- t1 = t2],  [f t2 = e] with [e |- t2 = t2'], 
    then [transform_rhs_equal f e |- t1 = t2']. *)


val mk_transform_diseq : (Term.t -> equal) -> diseq -> diseq
 (** If [d |- t1 <> t2],  [f t1 = e] with [e |- t1 = t1'], 
    and [f t1 = e] with [e |- t1 = t1'], then [transform_diseq f e |- t1' <> t2']. *)

val mk_transform_nonneg : (Term.t -> equal) -> nonneg -> nonneg
 (** If [n |- t >= 0],  [f t = e] with [e |- t = t'], 
   then [transform_nonneg f e |- t' >= 0]. *)

val mk_transform_pos : (Term.t -> equal) -> pos -> pos
 (** If [n |- t > 0],  [f t = e] with [e |- t = t'], 
   then [transform_pos f e |- t' > 0]. *)

val mk_transform_cnstrnt : (Term.t -> equal) -> cnstrnt -> cnstrnt
 (** If [cc |- t in c],  [f t = e] with [e |- t = t'], 
   then [transform_cnstrnt f cc |- t' in c]. *)


(** {6 Disjunction of atoms} *)

class type disjunction = 
 object
     method concl : atoms
     method hyps : atoms
     method name : string
     method assumptions : Atom.Set.t -> unit
     method validate : bool
 end

val mk_disj: atoms -> atoms -> disjunction
   (** [disj pl [a{1};...;a{n}] |- a{1} | ... | a{n}]. *)



(** {6 Homogeneous sets} *)


module Equals : (Sets.S with type elt = equal)
  (** Set of equality judgements *)

val equals_to_atoms : Equals.t -> atoms

module Diseqs : (Sets.S with type elt = diseq)
  (** Set of disequality judgements *)

module Cnstrnts : (Sets.S with type elt = cnstrnt)
  (** Set of constraint judgements *)


(** {6 Virtual Judgements} *)

module Top : sig

  class virtual atom : object
    method virtual concl : Atom.t
    method virtual name : string
    method virtual hyps : atoms
    method pp : Format.formatter -> unit
    method assumptions : Atom.Set.t -> unit
    method validate : bool
  end 

  class virtual unsat : object
    inherit atom
    method concl: Atom.t
  end
    
  class virtual equal : object
    inherit atom
    method virtual lhs : Term.t
    method virtual rhs : Term.t
    method concl : Atom.t
  end
    
  class virtual diseq : object
    inherit atom
    method virtual lhs : Term.t
    method virtual rhs : Term.t
    method concl : Atom.t
  end
    
  class virtual cnstrnt : object
    inherit atom
    method virtual arg : Term.t
    method virtual cnstrnt : Cnstrnt.t
    method concl : Atom.t
  end

  class virtual nonneg : object
    inherit atom
    method virtual arg : Term.t
    method concl : Atom.t
  end

  class virtual pos : object
    inherit atom
    method virtual arg : Term.t
    method concl : Atom.t
  end

end
