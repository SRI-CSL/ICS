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

(** Solution sets.

  @author Harald Ruess
*)

type t
  (** Representation of a finite set [{x{1} = t{1},...,x{n} = t{n}}]  
    of (oriented) equalities with
    - [x{i}] are term variables,
    - [x{i}], x{j}] are pairwise disjoint, and
    - none of the [x{i}] occurs in any [a{j}]. *)

val is_empty : t -> bool
 (** [is_empty s] holds iff [s] represents the empty set solution [{}]. *)

val pp : Format.formatter -> t -> unit
  (** Pretty-printing a solution set. *)

val apply : t -> Term.t -> Judgement.equal
  (** [apply s x] returns [t, e] with justification [e |- x = t] if
    [x = t] is in [s]; otherwise, [Not_found] is raised. *)

val inv : t -> Term.t -> Judgement.equal

val equalities : t -> Judgement.Equals.t
 
val iter : (Judgement.equal -> unit) -> t -> unit

val fold : (Term.t -> Judgement.equal -> 'a -> 'a) -> t -> 'a -> 'a
  
val dep : t -> Term.t -> Dep.Set.t

val is_solved : t -> bool

val is_canonical : V.Config.t -> t -> bool
 
val occ : Term.t -> t -> bool
 
val in_dom : Term.t -> t -> bool    

val in_cod : Term.t -> t -> bool

val model : t -> Term.Model.t
   
val empty : unit -> t

val copy : t -> t

val extend : t -> Judgement.equal -> unit

val restrict : t -> Term.t -> unit

val update : t -> Judgement.equal -> unit
