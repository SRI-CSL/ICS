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
    equalities, variable disequalities, and variable constraints.
  - A {b solution set} for every equality theory defined in {!Th}. 
  - An upper bound on the indices of all fresh variables in all the data
    structures above.
*)

type t
  (** Representation of logical contexts. *)

val pp : t Pretty.printer
  (** Pretty-printing the context of a state. *)

val eq : t -> t -> bool
  (** Identity test on contexts. *)

val empty : t
  (** The empty logical context *)

val ctxt_of : t -> Atom.t list
  (** [ctxt_of s] returns the logical context of [s] as a list of atoms. *)

val eqs_of : t -> Combine.t
  (** [eqs_of s] returns ths solution sets associated with [s]. *)

val partition_of : t -> Partition.t
  (** [p_of s] returns the partitioning associated with [s]. *)

val config_of : t -> Partition.t * Combine.t

val upper_of : t -> int
  (** [upper_of s] returns an upper bound on the indices of all fresh
    variables in [s]. *)

val apply : Th.t -> t -> Jst.Eqtrans.t
  (** [apply th s x] is [a] when [x = a] is in the solution set for theory [th]
    in [s]; otherwise [Not_found] is raised. *)

val find : Th.t -> t -> Jst.Eqtrans.t
  (** [find th s x] is [a] if [x = a] is in the solution set for theory [th]
    in [s]; otherwise, the result is just [x]. *)

val inv : t -> Jst.Eqtrans.t
  (** [inv s a] is [x] if there is [x = a] in the solution set for
    theory [th]; otherwise [Not_found] is raised. *)

val dep : Th.t -> t -> Term.t -> Term.Var.Set.t
  (** [use th s x] consists of the set of all term variables [y] such
    that [y = a] in [s], and [x] is a variable [a]. *)

val cheap : bool ref

val simplify : t -> Fact.t -> Fact.t


module Status : sig

  type 'a t = 
    | Valid of Jst.t
    | Inconsistent of Jst.t
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

val compactify : bool ref

val is_invalid : t -> Atom.t list -> Jst.t option
