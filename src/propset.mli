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

(** Theory of propositional sets

  @author Harald Ruess
 
  Set connective, including recognizers and destructors.
  [ite(x,p,n)] can be thought of being defined as
  [union (inter x p) (inter (compl x) n)], where [union],[inter],
  and [compl] are just set union, set intersection, and set complement,
  respectively. [diff s1 s2] is the set difference, and [sym_diff]
  is the symmetric set difference operator. There is one 
  disequality [empty <> full].
*)

val d_interp : Term.t -> Sym.propset * Term.t list

val is_empty : Term.t -> bool
val is_full : Term.t -> bool

val is_diseq : Term.t -> Term.t -> bool
val is_const : Term.t -> bool
   
val mk_empty : unit -> Term.t
val mk_full : unit -> Term.t    
val mk_ite : Term.t -> Term.t -> Term.t -> Term.t

val mk_inter : Term.t -> Term.t -> Term.t
val mk_union : Term.t -> Term.t -> Term.t
val mk_compl : Term.t -> Term.t


val sigma : Sym.propset -> Term.t list -> Term.t
  (** Canonizer. *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** [map f a] applies [f] to all top-level uninterpreted
    subterms of [a], and rebuilds the interpreted parts in order.
    It can be thought of as replacing every toplevel uninterpreted
    [a] with ['f(a)'] if [Not_found] is not raised by applying [a],
    and with [a] otherwise, followed by a sigmatization
    of all interpreted parts using [mk_sigma]. *)


val solve : Term.Equal.t -> Term.Equal.t list
  (** Solver *)
