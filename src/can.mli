(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Versicn 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)


(** Term canonizers and normalization functions for atoms. 

  @author Harald Ruess
*)


val term : Context.t -> Term.t -> Term.t
  (** [term s a] computes a canonical normal form of [a] with respect
    to the equality theory in [s]. That is, [Term.eq (term s a) (term s b)]
    holds if and only if the [term s a] and [term s b] are equal in the
    equality theory of [s]. *)

val arith : Context.t -> Sym.arith -> Term.t list -> Term.t
  (** [arith s op al] yields a canonical term [b] such 
    that [Term.mk_app op al] is equal to [b] in the equality
    theory of [s]. *)

val eq : Context.t -> Term.t -> Term.t -> bool
  (** [eq s a b] is [true] if and only if [a] and [b] are
    equal in the equality theory of [s]. *)

val atom : Context.t -> Atom.t -> Atom.t
  (** [atom s a] normalizes atom [a] in context [s]. 
    For equalities [e], this function is a canonizer in the
    sense that [atom s e] reduces to [Atom.mk_true] if [e]
    holds in [s] and [Atom.mk_false] if the negation of 
    [e] holds in [s].  Denumerators in equalities and inequalities 
    are cross-multiplied.  Similarly, constraints [c] of the form [a in i], 
    [atom s c] are normalized by crossmultiplying denumerators with
    a fixed sign in [s]. *)
