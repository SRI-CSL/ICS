
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
  
  A {b logical context} is simply a set [ctxt] of atoms.  When atoms are
  added to a logical context, the following datastructures are maintained.
  - A {b partitioning} [p] of type {!Partition.t} consisting of variable 
    equalities, variable disequalities, and constraints on variables
  - A {b solution set} for every equality theory defined in {!Th}. 
  - An upper bound on the indices of all fresh variables in all the data
    structures above.
*)

type t
  (** Representation of logical contexts. *)

(** {6 Accessors} *)

val ctxt_of : t -> Atom.Set.t
  (** [ctxt_of s] returns the logical context of [s]. *)

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

val equality : Th.t -> t -> Term.t -> Fact.equal
  (** [equality th s x] returns a fact for the equality [x = a],
    if in [s]; otherwise [Not_found] is raised. *)

val upper_of : t -> int
  (** [upper_of s] returns an upper bound on the indices of all fresh
    variables in [s]. *)

val v_of : t -> V.t
val d_of : t -> D.t
val c_of : t -> C.t

val pp : t Pretty.printer
  (** Pretty-printing the context of a state. *)

val cnstrnt : t -> Term.t -> Cnstrnt.t
  (** For an term [a] which is either uninterpreted or an interpreted
    constant, [cnstrnt s a] returns a most refined constraint. *)

val v : t -> Term.t -> Term.t
  (** [v s x] returns the canonical variable in [s] of the equivalence 
    class containing [x]. *)

val d : t -> Term.t -> Term.Set.t
  (** [d s x] returns the set of variable terms known to be disequal
    in the partitioning of [s]. *)

val c : t -> Term.t -> Cnstrnt.t
  (** [c s x] looks up a constraint for [x] in the partitioning part
    of [s]. If no such constraint is known, [Not_found] is raised. *)

val lookup : t -> Term.t -> Term.t
  (** [lookup s a] returns, if possible, a canonical variable equal to [a].
    It is either [v s (inv th s a)], where [th] is the theory of the 
    toplevel symbol of a term application [a], or [v s a], if [a] is
    a variable or [inv th s a] raises [Not_found]. *)


(** {6 Predicates} *)

val is_equal : t -> Term.t -> Term.t -> Three.t

val eq : t -> t -> bool
  (** Identity test on contexts. *)


(** {6 Iterators} *)

val fold : t -> (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a
  (** Fold over an equivalence class. *)

val folduse : Th.t -> Term.t -> (Term.t * Term.t -> t -> t) -> t -> t
  (** Folding over the use list. *)

(** [choose s p x] returns [z] if for some [y] in the equivalence class of [x] 
  [p y] yields [Some(z)]. If there is not such [y], [Not_found] is raised. *)
  
val choose : t -> (Term.t -> 'a option) -> Term.t -> 'a


(** {6 Canonizer and Solver} *)

val sigma : t -> Sym.t -> Term.t list -> Term.t
  (** [sigma] normal form. *)

val solve : Th.t -> t -> Fact.equal -> Fact.equal list
  (** [solve i s (a, b)] applies solver for theory [i] on equality [a = b]. *)

val integer_solve : bool ref

(** {6 Constructors} *)

val empty : t
  (** The empty logical context *)

val extend : Atom.t -> t -> t

val union : Th.t -> Fact.equal -> t -> t

val restrict : Th.t -> Term.t -> t -> t

val fuse : Th.t -> Fact.equal -> t -> t

val compose : Th.t -> Fact.equal -> t -> t

val propagate : Th.t -> Fact.equal -> t -> t

val update: Partition.t -> t -> t

val name : Th.t -> t * Term.t -> t * Term.t


val split : t -> Atom.Set.t
  (** List all constraints with finite extension. *)


(** {6 Changed Set Management} *)

module Changed : sig

  type t

  val in_v : t -> Term.Set.t
  val in_d : t -> Term.Set.t
  val in_c : t -> Term.Set.t

  val in_eqs : Th.t -> t -> Term.Set.t

  val reset : unit -> unit

  val save : unit -> t

  val restore : t -> unit

  val stable : unit -> bool

  val pp : t Pretty.printer
 



end

val protect : (t -> t) -> t -> t
  (** Update rules work on the following global variables together with the index
    for creating new variables. Within a [protect] environment, updates are performed
    destructively. Global variables are not protected! *)
  
