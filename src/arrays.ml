
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

(*i*)
open Term
open Three
open Sym
open Context
(*i*)


(*s From the equality [x = y] and the facts
 [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
 and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
 that [z1 = k2]. *)

let rec propagate e s =
  Trace.msg "rule" "Array(=)" e Fact.pp_equal;
  let (x, y, prf) = Fact.d_equal e in
    propagate1 (x, y, prf)
      (propagate1 (y, x, prf) s)

and propagate1 (x, y, prf) s =
    Set.fold
      (fun z1 s1 -> 
	 match apply Th.arr s1 z1 with 
	   | App(Arrays(Select), [upd1; j1])
	       when is_equal s1 y j1 = Three.Yes ->
	       fold s1
		 (fun z2 s2  -> 
		    match apply Th.arr s2 z2 with 
		      | App(Arrays(Update), [a2; i2; k2])
			  when is_equal s2 x i2 = Three.Yes -> 
			  let e' = Fact.mk_equal (v s2 z1) (v s2 k2) None in
			    update (Partition.merge e' (p_of s2)) s2
		      | _ -> s2)
		 upd1 s1
	   | _ -> s1)
      (use Th.arr s x)
      s


(*s Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [z1 = select(upd, j')], [z2 = update(a,i',x)],
 [i = i'], [j = j'], [upd = z2], it follows that
 [z1 = z3], where [z3 = select(a,j)]. *)

let rec diseq d s =
  Trace.msg "rule" "Array(<>)" d Fact.pp_diseq;
  let (i, j, prf) = Fact.d_diseq d in
    diseq1 (i, j, prf)
      (diseq1 (j, i, prf) s)

and diseq1 (i, j, prf) s =
  Set.fold
   (fun z1 s1 -> 
      match apply Th.arr s1 z1 with
	| App(Arrays(Select), [upd; j']) 
	    when is_equal s1 j j' = Three.Yes ->
	    fold s 
	      (fun z2 s2 ->
		 match apply Th.arr s2 z2 with
		   | App(Arrays(Update), [a; i'; _])
		       when is_equal s2 i i' = Three.Yes ->
		       let (s', z3) = name Th.arr (s, Arr.mk_select a j) in
		       let e' = Fact.mk_equal (v s2 z1) (v s2 z3) None in
		       let p' = Partition.merge e' (p_of s2) in
			 update p' s'
		   | _ -> s2)
	      upd s1
	| _ -> s)    
    (use Th.arr s j)
    s
