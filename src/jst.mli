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


(** {6 Justifications} *)

type t
   
val pp : t Pretty.printer

val axioms_of: t -> Atom.Set.t

val is_none : t -> bool



(** {6 Proof Mode} *)

module Mode : sig

  type t = No | Dep | Yes

  val of_string : string -> t
  val to_string : t -> string

end

val proofmode : Mode.t ref


(** {6 Exceptions} *)

exception Inconsistent of t
exception Valid of t

val inconsistent : t -> unit
val inconsistent2 : t -> t -> unit
val inconsistent3 : t -> t -> t -> unit
val inconsistent_star : t list -> unit

(** {6 Constructors} *)

val axiom : Atom.t -> t

val trans : Term.t -> Term.t -> Term.t -> t -> t -> t
  (** If [j1 |- a = b] and [j2 |- b = c], then [trans a b c j1 j2 |- a = c]. *)

val apply : Term.t * Term.t -> t list -> t

val apply1 : Term.t * Term.t -> t -> t

val contradiction : t -> t -> t
  (** If [j1 |- atm1], [j2 |- atm2], and [atm1] and [atm2] are contradictory, then
     [contradiction j1 j2 |- false]. *)

val contradiction1 : t -> t
  (** If [j1 |- atm] and [atm] is equivalent with [false], then [contradiction1 j1 |- false]. *)

val contradiction_star : t list -> t

val equiv : Atom.t -> t -> t -> t
 (** If [j1 |- atm1], [j2 |- atm1 <=> atm2], then [equiv atm2 j1 j2 |- atm2] *)

val valid : t -> t -> t
  (** If [j1 |- atm], [j2 |- atm <=> true], then [valid atm j1 j2 |- true] *)

val invalid : t -> t -> t
  (** If [j1 |- atm], [j2 |- atm <=> false], then [valid atm j1 j2 |- false] *)


val solve : Th.t -> Term.t * Term.t -> t -> t
  (** If [j |- a = b] and [(x = c)] is in [Th.solve (a = b)], then [solve (x, c) j |- x = c]. *)
  
val sigma : (Sym.t * Term.t list) * Term.t -> t list -> t
  (** If [j_k |- atm_k], then [sigma ((f, [a_1,...,a_m]), b) j_1 ... j_n |- f(a_1,...,a_m) = b *)

val extend : Term.t * Term.t -> t
  (** [extend (x, a) |- x = a] with [x] a fresh variable. *)

val abstract : Term.t * Term.t -> t list -> t


val slackify : Term.t * Term.t -> t -> t
  (** [slackify (k, a) j |- k = a] with [k] a fresh slack variable and [j |- a >= 0]. *)

val nonzero : Term.t -> t -> t
 (** If [j |- a > 0], then [nonzero a j |- a <> 0]. *)


val implied : Atom.t -> t -> t
  (** If [j1 |- atm1] and [atm] is implied by [atm1], then [implied atm j1 |- atm]. *)

val implied_equal : Term.t -> Term.t -> t -> t
  (** If [j |- atm] and [a = b] is implied by [atm], 
    then [implied_equal a b j |- a = b]. *)


val weaken : Term.t -> t -> t
  (** If [j |- a >= 0], then [weaken a j |- a > 0]. *)

val groebner : Term.t * Term.t -> t -> t
  (** If [j |- x = c] and there exists a [d] such that [c * d] equals [b] and
     [a] equals [x * d], then [groebner (a, b) [j_1;...;j_n] |- a = b]. *)

val gomory : Term.t -> t -> t
  (** Justification for gomory cuts. If [rho |- x = a] and [b >= 0] follows
    from [x = a] by means of a Gomory cut, then [gomory b rho |- b >= 0]. *)

val const : Term.t * Term.t -> t -> t -> t
  (** If [j1 |- x = c], [j2 |- y = d] with [c], [d] nonequal constants in the same theory,
     then [const (x, y) j1 j2 |- x <> y]. *)

val posint : Term.t -> t -> t -> t
  (** If [j1 |- a in int], [j2 |- a > 0], then [posint a j1 j2 |- a >= 0]. *)

val negation : Atom.t -> t -> t

val oracle : string -> t list -> t
  (** Because an oracle [string] says so. *)

val array : int -> Term.t -> Term.t -> t list -> t
  (** [array i a b jl] justifies the equality [a = b], where the
    hypothesesis [jl] justify the hypotheses for index [i] by
    "simple" equality reasoning.
    - 1. [a[i:=x][i] = x]
    - 2. [i <> j] implies [a[i:=x][j] = a[j]]
    - 3. [a[j:=x] = b[j:=y]] implies [x = y].
    - 4. [a[j:=x] = b[k:=y]], [i<>j], [i<>k] implies [a[i] = b[i]]
    - 5. [i <> j] and [i <> k] implies [a[j:=x][i] = a[k:=y][i]]
    - 6. [i <> j] ==> a[i:=x][j:= y] = a[j:=y][i:=x]
  *)


(** {6 Record only dependencies} *)

val dependencies : t list -> t

val dependencies0 : t

val dependencies1 : t -> t

val dependencies2 : t -> t -> t
  

(** {6 Derived Rules} *)

val trans3 : Term.t * Term.t * Term.t * Term.t -> t -> t -> t -> t

val subst_equal : Term.t * Term.t -> t -> t list -> t

val subst_equal1 : Term.t -> Term.t -> t -> t -> t
val subst_equal2 : Term.t -> Term.t -> t -> t -> t -> t

val subst_diseq : Term.t * Term.t -> t -> t list -> t

val subst_nonneg : Term.t -> t -> t list -> t

val subst_pos : Term.t -> t -> t list -> t


(** {6 Justifying Relations} *)

type just = t

module Three : sig

  type t =
    | Yes of just
    | No of just
    | X


  val to_three : just list ref -> ('a -> 'b -> t) -> 'a -> 'b -> Three.t
    (** [to_three fcts p a[ accumulate facts in the result of [p a]
        in global variable [fcts] and returns a corresponding result of type
        {!Three.t}. *)

end


(** {6 Equality Transformers} *)

module Eqtrans : sig

  type t = Term.t -> Term.t * just

  val acc : just list ref -> t -> Term.t -> Term.t

  val id : t

  val compose : t -> t -> t

  val compose3 : t -> t -> t -> t

  val totalize : t -> t

  val compose_partial1 : t -> t -> t
    (** [compose_partial1 f g a] behaves like [compose f g a] when [f] 
      does not throw an exception. In this case, the result is [g a]. *)

  val trace : Trace.level -> string -> t -> t

end

module Pred : sig

  type t = Term.t -> just option

  val disj : t -> t -> t

  val apply : Eqtrans.t -> t -> t

end


module Pred2 : sig

  type t = Term.t -> Term.t -> just option

  val apply : Eqtrans.t -> t -> t

end


module Relation : sig

  type 'a t = 'a -> Three.t
 

end

module Rel1 : sig

  type t = Term.t -> Three.t

  val apply : Eqtrans.t -> t -> t

end

module Rel2 : sig

  type t = Term.t -> Term.t -> Three.t

  val apply : Eqtrans.t -> t -> t

  val orelse : t -> t -> t

  val of_preds : Pred2.t -> Pred2.t -> t

end
