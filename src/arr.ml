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

(** Decision procedure for the theory of arrays [Arr] as defined in 
  module {!Funarr}. This procedure works by canonization of array terms
  and forward chaining on configurations.

  A configuration [C] consists of a triple [(V, D, S)] with 
  - [V] a set of variable equalities,
  - [D] a set of variable disequalities,
  - [S] a set of array equalities [x = a] with [x] a variable 
  and [a] a pure array term.

  The pair [(V, D)] is passed around as a partitioning [P].

  Forward chaining is used to keep configurations {i confluent}.
  - (1) [u = a[i:=x]] ==> [x = u[i]]
  - (2) [i<>j] and [v = u[j]], [u = a[i:=x]] ==> [v = a[j]]
  - (3) [i<>j] and [u = a[i:=x]] and [v = u[j:=y]] ==> [v = a[j:=y][i:=x]], 
  - (4) [u = v[i:=x]], [v = a[i:=y]] ==> [u = a[i:=x]]
  - (5) [v = u[j]], [u = create(a)] ==> [v = a]
  i.e., flatten the rhs and add to the equality graph.

  These rules are {i eagerly} applied whenever the antecedent
  becomes applicable, that is, when
  - a new disequality is propagated
  - a new equality [u = a] is added

  Since not all disequalities are propagated, the rules [(2)], [(3)]
  are also applied {i lazily}. For rule [(2)], for example, whenever
  [a[j]] is canonized, then it is checked if rule [(2)] actually fires
  with an {i implicit} disequality.

  Case splitting needs to be performed on all pairs [(i, j)] such
  that [u = a'[j]], [a' = a[i:=x]] in order to make the procedure 
  complete.
*)

module P = V


module T: Can.T = struct
      
  let th = Theory.create "arr" "Theory of arrays"
	     
  let map = Funarr.map Term.is_equal

  let mk_var = Term.mk_external_var
  let mk_create = Funarr.Flat.mk_create
  let mk_select = Funarr.Flat.mk_select
  let mk_update = Funarr.Flat.mk_update

  let rec chains () =
    [chain1(); chain2(); chain3(); chain4(); chain5()]

  (** I. [a[i:=x][i] = x]. *)
  and chain1 () = 
    let a = mk_var "a" 
    and i = mk_var "i"
    and x = mk_var "x" in
      ([], 
       Atom.mk_equal (mk_select (mk_update a i x) i, x))

 (** II. [i <> j ==> a[i:=x][j] = a[j]]. *)
  and chain2 () = 
    let a = mk_var "a" 
    and i = mk_var "i"
    and j = mk_var "j"
    and x = mk_var "x" in
      ([Atom.mk_diseq (i, j)], 
       Atom.mk_equal (mk_select (mk_update a i x) j, 
		      mk_select a j))

 (** III. [a[i:=x][i:=y] = a[i:=y]]. *)
  and chain3 () = 
    let a = mk_var "a" 
    and i = mk_var "i"
    and j = mk_var "j"
    and x = mk_var "x"
    and y = mk_var "y" in
      ([],
       Atom.mk_equal (mk_update (mk_update a i x) i y, 
		      mk_update a i y))


  (** IV. [i <> j ==> a[i:=x][j:=y] = a[j:=y][i:=x]]. *)
  and chain4 () = 
    let a = mk_var "a" 
    and i = mk_var "i"
    and j = mk_var "j"
    and x = mk_var "x"
    and y = mk_var "y" in
      ([Atom.mk_diseq (i, j)],
       Atom.mk_equal (mk_update (mk_update a i x) j y, 
		      mk_update (mk_update a j y) i x))

  
  (** V. [create(a)[j] = a] *)
  and chain5 () = 
    let a = mk_var "a" 
    and j = mk_var "j" in
      ([], 
       Atom.mk_equal (mk_select (mk_create a) j, a))

  let disjunction e =
    raise Not_found
 
end

module Infsys: Can.S =
  Can.Make(T)


module Unit = 
  Eqs.Register(Eqs.Make(Infsys))
