
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

(*s An external variable is a non-slack variable.  *)

let is_slack_combination a =
  Arith.for_all (fun (_,x) -> C.is_slack x) a

(*s An arithmetic context consists of a solution set of equations
 of the form [x = a], where [x] is a variable and [a] is an arithmetic
 term not containing any of the rhs. *)


module A = Subst.Make(
  struct
    let name = "a"
    let fold = Arith.fold
    let map = Arith.map
  end)


type t = { 
  solution : A.t;
  cnstrnt : C.t
}

(* Constraint of a term. *)

let to_cnstrnt s a =
  let rec of_term a =
    if C.is_slack a then
      C.cnstrnt s.cnstrnt a
    else 
      match Arith.d_interp a with
	| Some(Sym.Num(q), []) -> Cnstrnt.mk_singleton q
	| Some(Sym.Mult, l) -> Cnstrnt.multl (List.map of_term l)
	| Some(Sym.Add, l) -> Cnstrnt.addl (List.map of_term l)
	| Some(Sym.Expt(n), [x]) -> Cnstrnt.expt n (of_term x)
	| _ -> of_term (A.apply s.solution a)
  in
  try Some(of_term a) with Not_found -> None


(*s Empty context. *)

let empty = {
  solution = A.empty;
  cnstrnt = C.empty
}


(*s Pretty-printing. *)

let pp fmt s = 
  if not(s.solution == A.empty) then
    begin
      Pretty.string fmt "a:";
      A.pp fmt s.solution;
      if not(C.is_empty s.cnstrnt) then
	begin
	  Pretty.string fmt " with: ";
	  C.pp fmt s.cnstrnt
	end
    end


(*s Accessors. *)

let solution_of s = A.solution s.solution
let apply s = A.apply s.solution
let find s = A.find s.solution
let inv s = A.inv s.solution
let mem s = A.mem s.solution
let use s = A.use s.solution


(*s Extend. *)

let extend b s = 
  try
    (inv s b, s)
  with
      Not_found ->
	let (x, solution') =  A.extend b s.solution in
	Trace.msg 6 "Extend(a)" (x,b) pp_equal;
	(x, {s with solution = solution'})


(*s Merging in an equality between two arbitrary arithmetic
 terms possibly containing slack variables. *)

let rec equality (a, b) s =
  let a = A.find s.solution a 
  and b = A.find s.solution b 
  in
  try
    match Arith.solve is_var (a, b) with
      | None -> 
	  (s, Veqs.empty)
      | Some(x, b) -> 
	  compose (x, b) s
  with
      Exc.Unsolved ->
	try
	  match Arith.solve C.is_slack (a, b) with
	    | None -> 
		(s, Veqs.empty)
	    | Some(k, b) ->
		assert(C.is_slack k);
		if is_slack_combination b then
		  propagate (k, b) s
		else 
		  abstract (k, b) s
	with
	    Exc.Unsolved ->
	      abstract (a, b) s


(*s Propagate an equality [x = b], where [x] is a variable 
 in the solution set of [s]. *)
		
and compose eq s =
  let (solution', veqs') = A.compose s.solution [eq] in
  let s' = {s with solution = solution'} in
  (s', veqs')


(*s Propagate an equality ['k = b'] between a slack parameter
 and a combination [b] of slack parameters by, first, propagating
 the equality in the constraint part and, second, eliminating [k] 
 in the solution set. *)

and propagate (k, b) s =
  assert(C.is_slack k);
  let (cnstrnt', keqs') = C.propagate (k,b) s.cnstrnt in
  let (solution', veqs') = A.propagate s.solution ((k,b) :: keqs') in
  let s' = {s with solution = solution'; cnstrnt = cnstrnt'} in
  (s', veqs')

(*s Either [a] or [b] is outside of the decidable scope. 
 In this case, abstract [a] and [b] with possibly new variables,
 which are merged. *)

and abstract (a, b) s =
  let (x, s') = extend a s in 
  let (y, s'') = extend b s' in
  (s'', Veqs.singleton (Veq.make x y))



(*s Merging in an equation between two variables ['x = y']. 
 In the resulting state, [x] does not occur any more, and
 new equalities between variables may be infered. *)

let merge e s = 
  let (x, y) = Veq.destruct e in
  if not(A.occurs s.solution x) then   
    (s, Veqs.empty)
  else 
    begin
      Trace.msg 6 "Merge(a)" (x,y) pp_equal;
      let (s', veqs') = equality (x,y) s in
      (s', Veqs.remove e veqs')
    end


(*s Adding a constraint ['x in c'] to [s] by introducting a
 fresh slack parameter [k] followed by propagation of the
 equality ['x = k']. *)

let rec add (x,c) s =
  assert(is_var x);
  Trace.msg 6 "Add(a)" (x,c) pp_in;
  let (k, cnstrnt') = C.extend c s.cnstrnt in
  equality (x, k) {s with cnstrnt = cnstrnt'}


































































