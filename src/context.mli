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


(** Logical contexts

  @author Harald Ruess
  
  A {b logical context} is simply a conjunction [ctxt] of atoms.  When atoms 
  are added to a logical context, the following datastructures are maintained.
  - A {b partitioning} [p] of type {!Partition.t} consisting of variable 
    equalities and variable disequalities.
  - A set of {b arithmetic constraints}.
  - A {b solution set} for every equality theory defined in {!Th}. 
  - An upper bound on the indices of all fresh variables in all the data
    structures above.
*)

type t
  (** Representation of logical contexts. *)

(** {6 Accessors} *)

val ctxt_of : t -> Atom.Set.t
  (** [ctxt_of s] returns the logical context of [s] as a set of atoms. *)

val p_of : t -> Partition.t
  (** [p_of s] returns the partitioning associated with [s]. *)

val eqs_of : t -> Th.t -> Solution.t
  (** [eqs_of s th] returns the solution set for equality theory [th]
    in the logical context [s]. *)

val mem : Th.t -> t -> Term.t -> bool
  (** [mem th s x] iff [x = _] is in the solution set for theory [th]
    in [s]. *)

val apply : Th.t -> t -> Term.t -> Term.t
  (** [apply th s x] is [a] when [x = a] is in the solution set for theory [th]
    in [s]; otherwise [Not_found] is raised. *)

val find : Th.t -> t -> Term.t -> Term.t
  (** [find th s x] is [a] if [x = a] is in the solution set for theory [th]
    in [s]; otherwise, the result is just [x]. *)

val inv : Th.t -> t -> Term.t -> Term.t
  (** [inv th s a] is [x] if there is [x = a] in the solution set for
    theory [th]; otherwise [Not_found] is raised. *)

val use : Th.t -> t -> Term.t -> Term.Set.t
  (** [use th s x] consists of the set of all term variables [y] such
    that [y = a] in [s], and [x] is a variable [a]. *)


val upper_of : t -> int
  (** [upper_of s] returns an upper bound on the indices of all fresh
    variables in [s]. *)

val v_of : t -> V.t
val d_of : t -> D.t
val c_of : t -> C.t

val cnstrnt_of : t -> Term.t -> Fact.cnstrnt
  (** [cnstrnt_of s x] returns a constraint [c] for a variable [x]
    or throws [Not_found]. *)

val deqs_of : t -> Term.t -> Fact.diseq list
  (** [diseqs_of s x] returns the set of known disequalities for [x] *)

(*
val equality : Th.t -> t -> Term.t -> Fact.equal
  (** [equality th s x] returns a fact for the equality [x = a],
    if in [s]; otherwise [Not_found] is raised. *)
*)

val pp : t Pretty.printer
  (** Pretty-printing the context of a state. *)

val cnstrnt : t -> Term.t -> Interval.t

val is_int : t -> Term.t -> bool

val v : t -> Term.t -> Term.t
  (** [v s x] returns the canonical variable in [s] of the equivalence 
    class containing [x]. *)

val d : t -> Term.t -> Term.t list
  (** [d s x] returns the set of variable terms known to be disequal
    in the partitioning of [s]. *)

val c : t -> Term.t -> Interval.t
  (** [c s x] looks up a constraint for [x] in the partitioning part
    of [s]. If no such constraint is known, [Not_found] is raised. *)


(** {6 Predicates} *)

val is_equal : t -> Term.t -> Term.t -> Three.t

val eq : t -> t -> bool
  (** Identity test on contexts. *)


(** {6 Canonizer and Solver} *)

val sigma : t -> Sym.t -> Term.t list -> Term.t
  (** [sigma] normal form. *)

module Can : sig
  val term : t -> Term.t -> Term.t
  val atom : t -> Atom.t -> Atom.t
  val eq : t -> Term.t -> Term.t -> bool
end

val solve : Th.t -> t -> Fact.equal -> Fact.equal list
  (** [solve i s (a, b)] applies solver for theory [i] on equality [a = b]. *)

val integer_solve : bool ref


(** {6 Constructors} *)

val empty : t
  (** The empty logical context *)


module Status : sig

  type 'a t = 
    | Valid 
    | Inconsistent
    | Ok of 'a

  val pp : 'a Pretty.printer -> 'a t Pretty.printer
end


val add : t -> Atom.t -> t Status.t
  (** [process s a] extends the logical context [s] with an atom [a].
    The return value is 
    - [Valid] if [a] can be shown to be valid in [s],
    - [Inconsistent] if [s] conjoined with [a] can be shown to be inconsistent, and 
    - [Satisfiable(s')] otherwise. 
    
    In the latter case, [s'] is a logical state equivalent to [s] conjoined with [a]. *)


val split : t -> Atom.Set.t
  (** List all constraints with finite extension. *)
