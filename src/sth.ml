
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
 * Author: Harald Ruess
 i*)

(*i*)
open Term
(*i*)

module type Sth =
  sig
    val map: (Term.t -> Term.t) -> Term.t -> Term.t
    val solve : Term.t * Term.t -> (Term.t * Term.t) list
  end


module Make(S: Sth) = struct
  
(*s An context consists of a solution set of equations
 of the form [x = a], where [x] is a variable and [a] is an arithmetic
 term not containing any of the rhs. *)

  type t = Solution.t

  let empty = Solution.empty

  let is_empty = Solution.is_empty

		(*s Accessors. *)

  let solutions s = s
		      
  let apply s = Solution.apply s
  let find s = Solution.find s
  let inv s = Solution.inv s
  let mem s = Solution.mem s
  let use s = Solution.use s

		(*s Add equations between slack variables. *)

  let merge (x, y) (v, s, focus) =
    assert(V.eq v x y);
    let a = find s x in
    let b = find s y in
    if not(Term.eq a b) then
      let sl = S.solve (a, b) in
      let (v', s', focus', _) = Solution.compose S.map (v, s, sl, focus) in
      (v',s', focus')
    else 
      (v, s, focus)
      
  let close (v, s, focus) = 
    V.Focus.fold 
      (fun x ((v, _, _) as acc) ->
	 if Set.is_empty (use s x) then
	   acc
	 else
	   merge (x, V.find v x) acc)
      focus 
      (v, s, V.Focus.empty)
      

      (*s Extend. *)

  let extend = Solution.extend

      (*s instantiation. *)

  let inst = Solution.inst

end
