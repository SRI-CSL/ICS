

(*i
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
 * 
 * Author: Harald Ruess, N. Shankar
 i*)

(*s Module [Subst]: Datatype of substitutions for theory-specific
 solution sets. *)

module type INTERP = sig
  val name : string  
  val fold : (Term.t -> 'a -> 'a) -> Term.t -> 'a -> 'a  
  val map : (Term.t -> Term.t) -> Term.t -> Term.t
end

module Make(Th: INTERP): sig

  type t 

  val apply : t -> Term.t -> Term.t
  val find : t -> Term.t -> Term.t
  val inv : t -> Term.t -> Term.t
  val mem : t -> Term.t -> bool
  val use : t -> Term.t -> Term.Set.t
  val occurs : t -> Term.t -> bool

(*s [adduse x ys] explicitly manipulates the
 [use] structure so that [use s x] equals [ys].
 Use with caution! *)

  val adduse : Term.t -> Term.Set.t -> t -> t

(*s Normalization. *)

  val norm : t -> Term.t -> Term.t

(*s Empty substitution. *)

  val empty : t

(*s Test for emptyness. *)

  val is_empty : t -> bool

(*s Restrict. *)

  val restrict : Term.t -> t -> t 

(*s Extend. *)

  val extend : Term.t -> t -> Term.t * t


(*s Union. *)

  val union : Term.t -> Term.t -> t -> t

(*s Composing a solved form. Returns the modified substitution,
 the set of all derived variable equalities, and the set of
 variables for which the [find] changed. *)

  val compose : t -> (Term.t * Term.t) list -> t * Veqs.t * Term.Set.t

(*s Propagation of equalities on lhs. *)

  val propagate : t -> (Term.t * Term.t) list -> t * Veqs.t * Term.Set.t

(*s Solution set *)

  val solution : t -> (Term.t * Term.t) list

(*s Instantiation. *)

  val inst : (Term.t -> Term.t) -> t -> t

(*s [fold f s e] applies [f x a] to all [x = a] in
 the solution set of [s] in an unspecified order and
 accumulates the result. *)

  val fold : (Term.t -> Term.t -> 'a -> 'a) -> t -> 'a -> 'a

(*s Pretty-printing. *)

  val pp : t Pretty.printer

end
