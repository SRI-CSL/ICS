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

(** Theory of arrays.

  @author Harald Ruess

  Terms in the theory of arrays are selection [a[j]] of element
  at postition [j] in the array [a], and updating [a[i := x]]
  array [a] at position [i] with value [x].

  The theory of arrays is specified by
  - [a[i:=x][j] = x] when {!Term.is_equal}[(i, j)] is [Yes]
  - [a[i:=x][j] = a[j]] when {!Term.is_equal}[(i, j)] is [No]
  - [a[i:=x][i:=y] = a[i:=y]]
*)

(** {6 Function Symbols} *)

val select : Sym.t
val update : Sym.t


(** {6 Constructors} *)

val mk_select : Term.t -> Term.t -> Term.t
  (** [mk_select a j] constructs a canonical term equivalent
    to [App(select, [a; j])]. *)

val mk_update : Term.t -> Term.t -> Term.t -> Term.t
  (** [mk_update a x i] constructs a canonical term equivalent
    to [App(update, [a;x;i])]. *)


(** {6 Canonizer} *)

val sigma : Sym.arrays -> Term.t list -> Term.t


(** {6 Iterators} *)

val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** Applying a term transformer at all uninterpreted positions
    - [map f (mk_select a j)] equals [mk_select (map f a) (map f j)]
    - [map f (mk_update a i x)] equals [mk_select (map f a) (map f i) (map f x)]
    - Otherwise, [map f x] equals [f x] *)
