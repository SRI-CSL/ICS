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

(** Theory of arrays.

  @author Harald Ruess
*)

(** Terms in the theory of arrays are 
  - creation [create(a)] of a constant array
  - selection [a[j]] of element at position [j] in the array [a], and 
  - updating [a[i := x]] array [a] at position [i] with value [x].

  Given an equality relation [=E] and a disequality relation [<>D], the
  {i theory of arrays} is specified by
  - [create(a)[j] = a]
  - [a[i:=x][j] = x] when [i =E j]
  - [a[i:=x][j] = a[j]] when [i <>D j]
  - [a[i:=x][i:=y] = a[i:=y]]
  - [a[j:=y][i:=x] = a[i:=x][j:=y]] if [i <>D j] and {!Term.(<<<)}[i j]. 
*)

val theory : Theory.t

val is_interp : Term.t -> bool
  
val mk_create : Term.t -> Term.t
  
val mk_select : Term.t -> Term.t -> Term.t
  (** [mk_select a j] constructs a canonical term equivalent
    to [App(select, [a; j])]. *)
  
val mk_update : Term.t -> Term.t -> Term.t -> Term.t
  (** [mk_update a x i] constructs a canonical term equivalent
    to [App(update, [a;x;i])]. *)
  
  
val sigma : Term.interp
  
val map: (Term.t -> Term.t) -> Term.t -> Term.t
  (** Applying a term transformer at all uninterpreted positions
    - [map f (mk_select a j)] equals [mk_select (map f a) (map f j)]
    - [map f (mk_update a i x)] equals [mk_select (map f a) (map f i) (map f x)]
    - Otherwise, [map f x] equals [f x] *)


module Infsys: Can.INFSYS
  (** Inference system for the theory {!Th.arr} of extensional arrays
    as defined in module {!Funarr}.

    A context consists of equalities of the form [x = b] with [x] a
    variable and [b] a flat array term with variables as arguments:
    - [create(a)] for creating a constant array with 'elements' [a]
    - [a[i:=x]] for updating array [a] at position [i] with [x]
    - [a[j]] for selection the value of array [a] at position [j]
    
    Right-hand sides of context equalities [x = a] are kept in 
    canonical form.  That is, if the variable equality [y = z]
    has been merged using [Arr.merge], then the noncanonical [y]
    is not appearing on any right-hand side.
    
    Forward chaining is used to keep configurations {i confluent}.
    - (1) [u = a[i:=x]] ==> [x = u[i]],
    - (2) [i<>j], [u = a[i:=x]] ==> [u[j] = a[j]],
    - (3) [i<>j], [v = a[i:=x][j:=y]] ==> [v = a[j:=y][i:=x]],
    - (4) [u = a[i:=y][i:=x]] ==> [u = a[i:=x]],
    - (5) [u = create(a)[j]] ==> [u = a]
    
    Here, [i<>j] are the known disequalities in a variable partition
    (see {!Partition.t}). *)
