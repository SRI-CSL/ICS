
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

(*s Module [Solution]: abstract datatype for representing
 and manipulating conjunctions of equations [x = a], where
 [x] is a variable and [a] is a non-variable term. As an invariant,
 solution sets [s] are kept in functional form, that is, if [x = a]
 and [x = b] in [s], then [a] is identical with [b]. *)

type t


(*s [fold f s e] applies [f x a] to all [x = a] in
 the solution set [s] in an unspecified order and
 accumulates the result. *)

val fold : (Term.t -> Term.t -> 'a -> 'a) -> t -> 'a -> 'a


(*s [apply s x] returns [b] if [x = b] is in [s], and raises [Not_found]
 otherwise. For non-variable terms [b], [apply s b] always returns [b]. *)

val apply : t -> Term.t -> Term.t


(* [find s x] returns [b] if [x = b] is in [s], and [x] otherwise. 
 For non-variable [x], [find s x] always returns [x]. *)

val find : t -> Term.t -> Term.t


(*s [inv s b] returns [x] if [x = b] is in [s]; otherwise [Not_found]
 is raised. *)

val inv : t -> Term.t -> Term.t


(*s [mem s x] holds iff [x = b] in [s]. *)

val mem : t -> Term.t -> bool


(*s [occurs s x] holds if either [mem s x] or if [x] is
 a variable in some [b] where [y = b] is in [s]. *)

val occurs : t -> Term.t -> bool

(*s [use s x] returns all [y] such that [y = a] in [s]
 and [x] is a variable in [vars a] (see module [Term]).
 However, if [s] has been manipulated using [adduse x ys s],
 then [ys] is returned. *)

val use : t -> Term.t -> Term.Set.t



(*s The [empty] solution set *)

val empty : t


(*s [is_empty s] holds iff [s] does not contain any equalities. *)

val is_empty : t -> bool

(*s Are the two solutions states identical. *)

val unchanged : t -> t -> bool


(*s [restrict s x] removes equalities [x = a] from [s]. *)

val restrict : Term.t -> t -> t 


(*s [extend a s] generates a fresh variable [x] and add [x = a]
 to [s]. It is assumed that there is no [y = a] already in [s]. 
 The result is the generated variable [x] together with the extended
 solution set. *)

val extend : Term.t -> t -> Term.t * t

(*s [union (x, b) s] *)

val union : Term.t * Term.t -> t -> t

(*s [name e a] returns a variable [x] if there is
 a solution [x = a]. Otherwise, it creates a new name
 [x'] and installs a solution [x' = a] in [e]. *)

val name : Term.t * t -> Term.t * t


(*s [fuse norm v s sl] propagates the equalities in [sl] on the
  right-hand side of equalities in [s]. 
  The return value [(v', s', ch')] consists of an extension of
  [v] by means of newly generated variable equalities, the
  modified solution set [s'], and  the set of variables with a 
  new canonical form in [v'] are returned. Thus,
      [s' = { x = b' | x = b in s, b' = norm sl b, b' a non-variable}],
      [v' = { x = y | x = b in s, y = norm sl b, y a variable}], and
      [ch' = {x | x = y in v'}].  Here, it is assumed that [y] is
  'more canonical' than [x] using the variable ordering specified in
  the module [V]. *)

val fuse : ((Term.t -> Term.t) -> Term.t -> Term.t)
              -> Partition.t * t -> (Term.t * Term.t) list -> Partition.t * t


(*s [compose norm v s sl] is a [fuse] step followed by
 extending (and possibly overwriting [x = ...]) 
 the resulting [s'] with all [x = b], for [b]
 a non-variable term, in [sl]. If [b] is a variable, then
 it is added to [v'] and [ch'] is extended accordingly. *)

val compose :  ((Term.t -> Term.t) -> Term.t -> Term.t)
                  -> Partition.t * t -> (Term.t * Term.t) list -> Partition.t * t

(*s Normalize. *)

val normalize : ((Term.t -> Term.t) -> Term.t -> Term.t) 
                  -> Partition.t * t -> Partition.t * t


(*s Pretty-printing. *)

val pp : t Pretty.printer

(*s Instantiating lhs with canonical variables. *)

val inst : V.t -> t -> t

(*s Change sets. *)

val changed : t -> Term.Set.t

val reset : t -> t
