
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
open Partition
(*i*)

(*s Selectors. *)

let d_update a = match a with 
  | App(Builtin(Update), [b; i; x]) -> (b, i, x)
  | _ -> raise Not_found

and d_select a = match a with 
  | App(Builtin(Select), [a; i]) -> (a, i)
  | _ -> raise Not_found


(*s From the equality [x = y] and the facts
 [z1 = select(upd1,j1)], [z2 = update(a2,i2,k2)] in [s]
 and [upd1 == z2 mod s], [x == i2 mod s], [y == j1 mod s] deduce
 that [z1 = k2]. *)

let rec propagate e =
  let (x, y, _) = Fact.d_equal e in
    from_eq (x, y)

and from_eq (x, y) (p, u) =
  let p' = 
    Set.fold
      (fun z1 p1 -> 
	 try
	   let (upd1, j1) = d_select (Solution.apply u z1) in
	     if not(V.is_equal p1.v y j1) then
	       p1
	     else 
	       V.fold p1.v
		 (fun z2 p2  -> 
		    try
		      let (a2, i2, k2) = d_update (Solution.apply u z2) in
			if V.is_equal p2.v x i2 then
			  let e' = Fact.mk_equal (v p2 z1) (v p2 k2) None in
			    Partition.merge e' p2
			else
			  p2
		    with
			Not_found -> p2)
		 (v p1 upd1)
		 p1
	 with
	     Not_found -> p1)
      (Solution.use u x)
      p
  in
    (p', u)


(*s Propagating a disequalities.
 From the disequality [i <> j] and the facts
 [z1 = select(upd, j')], [z2 = update(a,i',x)],
 [i = i'], [j = j'], [upd = z2], it follows that
 [z1 = z3], where [z3 = select(a,j)]. *)

let rec diseq d (p, u) =
  let (x, y, _) = Fact.d_diseq d in
    from_diseq (x, y)
      (from_diseq (y, x) (p, u))

and from_diseq (i, j) ((p, u) as s) =
  Set.fold
   (fun z1 ((p1, u1) as s1) -> 
      try
	let (upd, j') = d_select (Solution.apply u1 z1) in 
	  if not(V.is_equal p1.v j j') then
	    s1
	  else 
	  V.fold p1.v
	    (fun z2 ((p2, u2) as s2) ->
	       try
		let (a, i', x) = d_update (Solution.apply u z2) in
		  if not(V.is_equal p2.v i i') then
		    s2
		  else 
		    let x = Term.mk_app Sym.select [a;j] in
		      try
			let z3 = Solution.inv u2 x in 
			let e' = Fact.mk_equal (v p2 z1) (v p2 z3) None in
			  (Partition.merge e' p2, u2)
		      with
			  Not_found ->
			    let z3 = Term.mk_fresh_var (Name.of_string "v") None in
			    let e3 = Fact.mk_equal (v p2 z1) (v p2 z3) None in
			      (Partition.merge e3 p2, Solution.union (z3, x) u2)
		      with
			  Not_found -> s2)
	    (v p1 upd)
	    s1
      with
	  Not_found -> s1)
    (Solution.use u j)
    s
