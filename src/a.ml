
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

let is_linear_slack_combination a =
  Arith.for_all (fun (_,x) -> C.is_slack x) a

let is_slack_equality (a,b) = 
  is_linear_slack_combination a &&
  is_linear_slack_combination b


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
  try
    let b = A.apply s.solution a in
    Some(C.cnstrnt s.cnstrnt b)
  with
      Not_found ->
	if is_linear_slack_combination a then
	  Some(C.cnstrnt s.cnstrnt a)
	else 
	  None


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


(*s Add equations between variables. *)


let rec merge e s = 
  let (x,y) = Veq.destruct e in
  Trace.msg 6 "Merge(a)" (x,y) pp_equal;
  let (s', veqs') = merge1 (x,y) s in
  (s', Veqs.remove e veqs')

and merge1 (x,y) s =
  if not(A.occurs s.solution x) then   
    (s, Veqs.empty)
  else 
    let a = A.norm s.solution x and b = A.norm s.solution y in
    try
      match Arith.solve is_var (a, b) with
	| None -> (s, Veqs.empty)
	| Some(x, b) -> 
	    let (solution', veqs') = A.compose s.solution [(x, b)] in
	    ({s with solution = solution'}, veqs')
    with
      Exc.Unsolved ->
	abstract (a, b) s

and abstract (a, b) s =
  let (u, solution') = A.extend a s.solution in
  let (v, solution'') = A.extend b solution' in
  ({s with solution = solution''}, Veqs.singleton (Veq.make u v))

and compose eqs s =
  let (solution', veqs') = A.compose s.solution eqs in
  ({s with solution = solution'}, veqs')

(*s Extend. *)

let extend b s = 
  let (x, solution') =  A.extend b s.solution in
  (x, {s with solution = solution'})


(*s Adding a constraint. *)

let rec add (x,c) s =
  Trace.msg 6 "Add(a)" (x,c) pp_in;
  match to_cnstrnt s x with
    | Some(d) ->
	(match Cnstrnt.cmp c d with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent
	   | (Binrel.Super | Binrel.Same) ->
	       (s, Veqs.empty)
	   | Binrel.Sub ->
	       cnstrnt (x,c) s
	   | Binrel.Singleton(q) ->
	       let n = Arith.mk_num q in
	       (try
		  let y = A.inv s.solution n in
		  merge1 (x,y) s
		with
		    Not_found ->
		      let (y, solution') = A.extend n s.solution in
		      merge1 (x,y) {s with solution = solution'})
	   | Binrel.Overlap(cd) ->
	       cnstrnt (x, cd) s)
    | None ->
	cnstrnt (x,c) s

and cnstrnt (x, c) s =
  assert(is_var x);
  Trace.msg 5 "Cnstrnt(a)" (x, c) pp_in;
  let (k, cnstrnt') = C.extend c s.cnstrnt in
  try
    let b = A.apply s.solution x in
    if is_linear_slack_combination b then
      let cnstrnt'', eqs'' = C.merge (k,b) cnstrnt' in
      let s'' = {s with cnstrnt = cnstrnt''} in
      compose eqs'' s''
    else 
      merge1 (x,k) {s with cnstrnt = cnstrnt'}
  with
      Not_found -> 
        merge1 (x,k) {s with cnstrnt = cnstrnt'}























































(*





type t = {
  a : A.t;                     (* Equalities of the form [x = a]. *)
  c : C.t                      (* Constraint table. *)
}


(*s Empty context. *)

let empty = { a = A.empty; c = C.empty }

(*s Pretty-printing. *)

let pp fmt s =
  if not(A.is_empty s.a) then
    begin
      Pretty.string fmt "a:";
      A.pp fmt s.a;
      if not(C.is_empty s.c) then
	begin
	  Pretty.string fmt " with "; 
	  C.pp fmt s.c;
	  Pretty.string fmt "\n"
	end 
      else 
	Pretty.string fmt "\n"
    end


(*s Accessors. *)

let solution_of s = A.solution s.a

let apply s = A.apply s.a
let find s = A.find s.a
let inv s = A.inv s.a
let mem s = A.mem s.a
let use s = A.use s.a

let cnstrnt s a =
  try
    let b = A.apply s.a a in
    Some(C.cnstrnt s.c b)
  with
      Not_found ->
	if is_linear_slack_combination a then
	  Some(C.cnstrnt s.c a)
	else 
	  None

(*s Extend the domain of equality map. *)

let extend b s = 
  Trace.call 6 "Extend(a)" b Term.pp;
  let (x,a') = A.extend b s.a in 
  Trace.exit 6 "Extend(a)" x Term.pp;
  let s' = {s with a = a'} in
  (x, s')


(*s Adding a constraint for a linear combination of internal variables
  to the constraint part and return resulting internal equalities. Also 
  make  sure that only slack variables are added to the constraint part
  by creating new slack variables. *)

let rec refine s (a, d) = 
 Trace.msg 6 "Refine(a)" (a, d) pp_in;
 let (sc', seqs') = C.add (a, d) s.c in
 ({s with c = sc'}, seqs') 


(*s Propagate an equality between two terms. First, solve for an
 external variable if possible and compose the solution in the
 equality part. Otherwise, solve for a slack variable and propagate
 the solution on the rhs of the equality part. Besides external
 variable equalities his may also generate new constraints for 
 slack variables. *)

let rec propagate s (a, b) =
  try
    match Arith.solve is_var (a, b) with
      | None -> 
	  (s, Veqs.empty, [])
      | Some(x, b) ->
	  compose s (x, b)
  with
      Exc.Unsolved -> 
	if is_slack_equality (a, b) then
	  cnstrnt s (a, b)
	else 
	  abstract s (a, b)

and compose s (x, b) = 
  let (sa', veqs') = A.compose s.a [(x, b)] in
  ({s with a = sa'}, veqs', [])

and abstract s (a, b) =
  let (u, a') = A.extend a s.a in
  let (v, a'') = A.extend b a' in
  ({s with a = a''}, Veqs.singleton (Veq.make u v), [])

and cnstrnt s (a, b) = 
  let (sc', eqs') = C.propagate (a,b) s.c in
  let (sa', veqs') = A.compose s.a eqs' in
  ({s with a = sa'; c = sc'}, veqs',[])
  

(*s Iterate [refine] and [propagate] until new new [seqs] equalities between slack
 variables or constraints [cnstrnts] are generated. It returns an updated state and
 a set of generated variable equalities. *)

let rec install s (eqs, cnstrnts) veqs = 
  match eqs, cnstrnts with
    | [], [] -> 
	(s, veqs)
    | (a,b) :: eqs', _ ->
	let (s', veqs', cnstrnts') = propagate s (a,b) in
	install s' (eqs', cnstrnts' @ cnstrnts) (Veqs.union veqs' veqs)
    | _, (a,c) :: cnstrnts' ->
	let (s', seqs') = refine s (a, c) in
	install s' (seqs' @ eqs, cnstrnts') veqs

let install_equality s (a, b) =
  Trace.msg 5 "Merge(a)" (a,b) pp_equal;
  install s ([(a,b)], []) Veqs.empty
    
let install_cnstrnt s (x, c) =
  Trace.msg 5 "Cnstrnt(a)" (x, c) pp_in;
  try
    let b = apply s x in
    if is_linear_slack_combination b then
      install s ([], [(b,c)]) Veqs.empty
    else 
      let k = mk_slack () in
      install s ([(x,k)], [(k,c)]) Veqs.empty
  with
      Not_found -> 
	let k = mk_slack () in
	install s ([(x,k)], [(k,c)]) Veqs.empty


(*s Adding a new equality between variables and infer new facts. *)

let rec merge e s = 
  Trace.msg 6 "Merge(a)" (Veq.destruct e) pp_equal;
  let (s', veqs') = merge1 e s in
  (s', Veqs.remove e veqs')

and merge1 e s =
  let (x,y) = Veq.destruct e in
  if eq x y || not (A.occurs s.a x) then
    (s, Veqs.empty)
  else 
    install_equality s (A.norm s.a x,  A.norm s.a y)
   


(*s Adding a constraint. *)

let rec add (x,c) s =
  Trace.msg 6 "Add(a)" (x,c) pp_in;
  match cnstrnt s x with
    | Some(d) ->
	(match Cnstrnt.cmp c d with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent
	   | (Binrel.Super | Binrel.Same) ->
	       (s, Veqs.empty)
	   | Binrel.Sub ->
	       install_cnstrnt s (x,c)
	   | Binrel.Singleton(q) ->
	       let n = Arith.mk_num q in
	       (try
		  let y = A.inv s.a n in
		  install_equality s (x,y)
		with
		    Not_found ->
		      let (y, sa') = A.extend n s.a in
		      install_equality {s with a = sa'} (x,y))
	   | Binrel.Overlap(cd) ->
	       install_cnstrnt s (x, cd))
    | None ->
	install_cnstrnt s (x,c)





























(*


and propagate_external s (x, b) =
  Trace.msg 10 "External(a)" (x,b) pp_equal;
  match cnstrnt s x, cnstrnt s b with
    | Some(cx), Some(cb) ->
	(match Cnstrnt.cmp cx cb with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent
	   | (Binrel.Super | Binrel.Same) ->
	       let (sa', veqs') = A.compose s.a [(x, b)] in
	       ({s with a = sa'}, veqs', [])
	   | Binrel.Singleton(q) ->
	       let (sa', veqs') = A.compose s.a [(x, Arith.mk_num q)] in
	       ({s with a = sa'}, veqs', [])
	   | Binrel.Sub ->
	       let (sa', veqs') = A.compose s.a [(x, b)] in
	       ({s with a = sa'}, veqs', [b, cx])
	   | Binrel.Overlap(cxb) ->
	       let (sa', veqs') = A.compose s.a [(x, b)] in
	       ({s with a = sa'}, veqs', [b,cxb]))
    | _ ->
	let (sa', veqs') = A.compose s.a [(x, b)] in
	({s with a = sa'}, veqs', [])

*)







(*
and propagate_internal s (k,b) =
  Trace.msg 10 "Internal(a)" (k,b) pp_equal; 
  match cnstrnt s k, cnstrnt s b with
    | Some(ck), Some(cb) ->
	(match Cnstrnt.cmp ck cb with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent 
	   | (Binrel.Super | Binrel.Same) ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let (sc', eqs') = C.restrict (k,b) (s.c, []) in  (* what about eqs' *)
	       ({s with a = sa'; c = sc'}, veqs', [k, cb])
	   | Binrel.Sub ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let (sc', eqs') = C.restrict (k,b) (s.c, []) in
	       ({s with a = sa'; c = sc'}, veqs', [(b, ck)])
	   | Binrel.Singleton(q) ->
	       let n = Arith.mk_num q in
	       let (sa', veqs') = A.propagate s.a [k, n] in
	       let (sc', eqs') = C.restrict (k,n) (s.c, []) in
	       ({s with a = sa'; c = sc'}, veqs', [])
	   | Binrel.Overlap(ckb) ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let (sc', eqs') = C.restrict (k,b) (s.c, []) in
	       ({s with a = sa'; c = sc'}, veqs', [(b, ckb)]))
    | None, None ->
	(s, Veqs.empty, [])
    | _ ->
	failwith ("Propagate: Unreachable" ^ (Pretty.to_string pp_equal (k,b)))
 *)

*)
