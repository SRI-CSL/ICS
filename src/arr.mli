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

(** Inference system for the theory {!Th.arr} of functional array

  @author Harald Ruess
  @author N. Shankar
*)

(** A context consists of equalities of the form [x = b] with [x] a
  variable and [b] a flat array term with variables as arguments:
  - [create(a)] for creating a constant array with 'elements' [a]
  - [a[i:=x]] for updating array [a] at position [i] with [x]
  - [a[j]] for selection the value of array [a] at position [j]
  
  The extensional array theory {!Th.arr} is defined in module {!Funarr}.
  
  Right-hand sides of context equalities [x = a] are kept in 
  canonical form.  That is, if the variable equality [y = z]
  has been merged using {!Arr.merge}, then the noncanonical [y]
  is not appearing on any right-hand side.
  
  Forward chaining is used to keep configurations {i confluent}.
  - (1) [u = a[i:=x]] ==> [x = u[i]],
  - (2) [i<>j], [u = a[i:=x]] ==> [u[j] = a[j]],
  - (3) [i<>j], [v = a[i:=x][j:=y]] ==> [v = a[j:=y][i:=x]],
  - (4) [u = a[i:=y][i:=x]] ==> [u = a[i:=x]],
  - (5) [u = create(a)[j]] ==> [u = a]
  
  Here, [i<>j] are the known disequalities in a variable partition
  (see {!Partition.t}).
*)

module E: Can.EQS
  (** Equality set for array inference system {!Arr.Infsys}. 
    Equalities are of the form [x = a] with [x] a variable
    and [a] a {i flat} array term. *)

module Infsys: (Infsys.IS with type e = E.t)
  (** Inference system for the theory {!Th.arr} of arrays
    as defined in module {!Funarr}. *)

