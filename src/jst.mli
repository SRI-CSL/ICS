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

val axioms_of
 : t -> Atom.Set.t

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


(** {6 Constructors} *)

val axiom : Atom.t -> t

val trans : Term.t -> Term.t -> Term.t -> t -> t -> t
  (* If [j1 |- a = b] and [j2 |- b = c], then [trans a b c j1 j2 |- a = c]. *)

val apply : Term.t * Term.t -> t list -> t
val apply1 : Term.t * Term.t -> t -> t

val contradiction : t -> t -> t
  (* If [j1 |- atm1], [j2 |- atm2], and [atm1] and [atm2] are contradictory, then
     [contradiction j1 j2 |- false]. *)

val contradiction_star : t list -> t

val all : Atom.t -> t -> t
  (* If [j |- false], then [all atm j |- atm]. *)

val solve : Term.t * Term.t -> t -> t
  (* If [j |- a = b] and [(x = c)] is in [Th.solve (a = b)], then [solve (x, c) j |- x = c]. *)
  
val sigma : (Sym.t * Term.t list) * Term.t -> t list -> t
  (* If [j_k |- atm_k], then [sigma ((f, [a_1,...,a_m]), b) j_1 ... j_n |- f(a_1,...,a_m) = b *)

val extend : Term.t * Term.t -> t
  (* [extend (x, a) |- x = a] with [x] a fresh variable. *)

val slackify : Term.t * Term.t -> t -> t
  (* [slackify (k, a) j |- k = a] with [k] a fresh slack variable and [j |- a >= 0]. *)

val nonzero : Term.t -> t -> t
 (** If [j |- a > 0], then [nonzero a j |- a <> 0]. *)

val zero : Term.t -> t -> t -> t
  (* If [j1 |- a1], [j2 |- a2], and [a1] conjoined with [a2] is equivalent with [a = 0], then
     [zero a j1 j2 |- a = 0]. *)

val weaken : Term.t -> t -> t
  (* If [j |- a >= 0], then [weaken a j |- a > 0]. *)

val inter : Term.t * Dom.t -> t -> t -> t
  (* If [j1 |- a in d1], [j2 |- a in d2], and [d] is equal to [{!Dom.inter} d1 d2],
     then [inter (a, d) j1 j2 |- a in d]. *)

val groebner : Term.t * Term.t -> t -> t
  (* If [j |- x = c] and there exists a [d] such that [c * d] equals [b] and
     [a] equals [x * d], then [groebner (a, b) [j_1;...;j_n] |- a = b]. *)

val equal0 : Term.t * Term.t -> t
  (* [equal0 (a, b) |- a = b] if [{!Term.is_equal} a b = Yes]. *)

val diseq0 : Term.t * Term.t -> t
 (* [diseq0 (a, b) |- a <> b] if [{!Term.is_equal} a b = No]. *)

val dom0 : Term.t * Dom.t -> t
  (** [dom0 (a, d) |- a in d] if {!Term.dom_of a} is a subdomain of [d]. *)


val const : Term.t * Term.t -> t -> t -> t
  (* If [j1 |- x = c], [j2 |- y = d] with [c], [d] nonequal constants in the same theory,
     then [const (x, y) j1 j2 |- x <> y]. *)

val posint : Term.t -> t -> t -> t
  (* If [j1 |- a in int], [j2 |- a > 0], then [posint a j1 j2 |- a >= 0]. *)

val oracle : string -> t
  (** Because an oracle says so. *)

val dom : Term.t * Dom.t -> t list -> t


val abstract : Term.t * Term.t -> t list -> t


(** {6 Record only dependencies} *)

val dependencies : t list -> t

val dependencies0 : t

val dependencies1 : t -> t

val dependencies2 : t -> t -> t
  

(** {6 Derived Rules} *)

val trans3 : Term.t * Term.t * Term.t * Term.t -> t -> t -> t -> t

val subst_equal : Term.t * Term.t -> t -> t list -> t

val subst_diseq : Term.t * Term.t -> t -> t list -> t

val subst_in : Term.t * Dom.t -> t -> t list -> t

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
