
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
open Mpa
open Term
(*i*)

(*s Constraint context as a map of term variables to constraints. *)

type t = {
  find : Symbolic.t Term.Map.t;
  use : Use.t;
}

let apply s x = Term.Map.find x s.find

let find s x = 
  try Term.Map.find x s.find with Not_found -> Symbolic.full

let use s = Use.find s.use

(*s Check if [x] is in the domain. *)

let dom x s = Term.Map.mem x s.find


(*s Check if [x] is either in the domain or a subterm of an implicit constraint. *)

let mem x s =
  dom x s ||
  not(Term.Set.is_empty (use s x))


(*s [meaning s sc] instantiates the symbolic constraint [sc] in context [s]. *)

let meaning s = Symbolic.meaning (apply s)


(*s [cnstrnt s a] computes best constraint for [a] in [s]. *)

let cnstrnt s = Symbolic.cnstrnt (apply s)


(*s Constraints as a list. *)

let to_list s =
  Term.Map.fold (fun x sc acc -> (x, meaning s sc) :: acc) s.find []


(*s Pretty-printing. *)

let rec pp fmt s = 
  Pretty.list Term.pp_in fmt (to_list s)


(*s The empty constraint map. *)

let empty = {
  find = Term.Map.empty;
  use = Use.empty
}


(*s Test for emptyness. *)

let is_empty s = (s == empty)


(*s Extend domain. *)

let extend (x,d) s =
  assert(not(mem x s));
  Trace.msg 6 "Extend(c)" (x,d) Term.pp_in;
  if Cnstrnt.is_empty d then
    raise Exc.Inconsistent
  else
    {s with find = Term.Map.add x (Symbolic.of_cnstrnt d) s.find}

(*s Restrict domain. *)

let restrict x s =
  try
    let symbolic = Symbolic.symbolic (apply s x) in
    Trace.msg 6 "Restrict(c)" x Term.pp;
    let find' = Term.Map.remove x s.find
    and use' =  List.fold_right (fun (_,a) -> Use.remove Arith.fold x a) symbolic s.use
    in
    {s with find = find'; use = use'}
  with
      Not_found -> s

(*s Updating constraint information. *)

let rec update_explicit x c (s, eqs) =
  let sc' = 
      try
	let sc = apply s x in
	Symbolic.make (c, Symbolic.symbolic sc)
      with
	  Not_found -> Symbolic.of_cnstrnt c
  in
  let s' = {s with find = Term.Map.add x sc' s.find} in
  derive x (s', eqs)


(*s Derive inconsistencies or equalities from all entries [y |-> ...]
  which contain [x]. *)

and derive x (s, eqs) =         (* to do: recursive updates. *)
  Set.fold
    (fun y (s, eqs) ->
       let c = cnstrnt s y in      
       if Cnstrnt.is_empty c then
	 raise Exc.Inconsistent
       else match Cnstrnt.d_singleton c with
	 | Some(q) ->
	     (restrict y s, (y, Arith.mk_num q) :: eqs)
	 | None -> 
	     (s, eqs))
    (use s x)
    (s, eqs)

(*s Adding a new symbolic constraint. *)

let extend_symbolic x (c,a) s =
  let sc' =
    try
      let (e,s) = Symbolic.destruct (apply s x) in
      Symbolic.make (e, (c,a) :: s)
    with
	Not_found ->
	  Symbolic.make (Cnstrnt.mk_real, [(c,a)])
  in
  let find' = Map.add x sc' s.find in
  let use' = Use.add Arith.fold x a s.use in
  {find = find'; use = use'}
  

(*s Normalize constraint such as ['2 * x + 5' in 'c'] 
 to ['x' in '1/2 ** (c -- 5)'], where ['**'], ['--']
 are abstract interval operations for linear multiplication and subtraction. *)

let normalize q p c =           
  if Q.is_zero q && Q.is_one p then
    c
  else if Q.is_zero q then
    Cnstrnt.multq (Q.inv p) c
  else 
    Cnstrnt.multq (Q.inv p) (Cnstrnt.addq (Mpa.Q.minus q) c)
 

(*s Adding a constraint. *)
    
let rec add (a,c) s =
  Trace.msg 6 "Add(c)" (a,c) Term.pp_in;
  if Cnstrnt.is_empty c then
    raise Exc.Inconsistent
  else match Cnstrnt.d_singleton c with
    | Some(q) ->
	(restrict a s, [(a, Arith.mk_num q)])
    | None ->
	(match Arith.decompose a with     
	   | Arith.Const(q) ->              (* Case: [a = q] *)
	       if Cnstrnt.mem q c then 
		 (s, []) 
	       else 
		 raise Exc.Inconsistent
	   | Arith.One(q, p, x) ->          (* Case: [q + p * x in c] *)
	       let c' = normalize q p c in  (* iff [x in 1/p * (c - q)] *) 
	       add_explicit x c' (s, [])
	   | Arith.Many(q, p, x, y) ->      (* Case: [q + p * x + y in c] *)
	       let c' = normalize q p c in  (* iff [x in ((1/p * (c - q)) - cnstrnt(1/p * y)] *)
	       let y' = Arith.mk_multq (Q.inv p) y in
	       add_symbolic x (c', y') (s, []))
	
and add_explicit x c (s, eqs) =            (* add an explicit constraint [x in c]. *)
  try
    let d = meaning s (apply s x) in
    match Cnstrnt.cmp c d with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | (Binrel.Super | Binrel.Same) ->
	  (s, eqs)
      | Binrel.Sub ->
	  update_explicit x c (s, eqs)
      | Binrel.Singleton(q) ->
	  (restrict x s, (x, Arith.mk_num q) :: eqs)
      | Binrel.Overlap(cd) ->
	  update_explicit x cd (s, eqs)
  with
      Not_found -> 
	update_explicit x c (s, eqs)

and add_symbolic x (c, a) (s, eqs) =
  let c' = Cnstrnt.subtract c (cnstrnt s a) in
  let (s', eqs') = add_explicit x c' (s, eqs) in
  let s'' = extend_symbolic x (c,a) s in
  (s'', eqs')
 
