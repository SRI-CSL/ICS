
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

(*s Slack variables *)

let slacks = ref Term.Set.empty
let _ = Tools.add_at_reset (fun () -> slacks := Term.Set.empty)

let is_slack x = Term.Set.mem x !slacks

let mk_slack = 
  let slackname = Name.of_string "k" in
  fun () -> 
    let k = Term.mk_fresh slackname None in
    slacks := Term.Set.add k !slacks;
    k


(*s An external term is a non-slack variable.  *)

let is_external x = 
  not(is_slack x)


(*s An arithmetic context consists of a solution set of equations
 of the form [x = a], where [x] is a variable and [a] is an arithmetic
 term not containing any of the rhs. *)

module A = Subst.Make(
  struct
    let name = "a"
    let is_external = is_external
    let fold = Arith.fold
    let map = Arith.map
  end)


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
  let rec loop a =
  match Arith.d_interp a with
    | Some(Sym.Num(q), []) -> Cnstrnt.mk_singleton q
    | Some(Sym.Mult, l) -> Cnstrnt.multl (List.map loop l)
    | Some(Sym.Add, l) -> Cnstrnt.addl (List.map loop l)
    | Some(Sym.Expt(n), [x]) -> Cnstrnt.expt n (loop x)
    | _ ->
	(match C.cnstrnt a s.c with
	   | Some(c) -> c
	   | None -> loop (A.apply s.a a))      (* might raise [Not_found] *)
  in
  try
    Some(loop a)
  with 
      Not_found -> None


(*s Extend the domain of equality map. *)

let extend b s = 
  Trace.call 6 "Extend(a)" b Term.pp;
  let (x,a') = A.extend b s.a in 
  Trace.exit 6 "Extend(a)" x Term.pp;
  let s' = {s with a = a'} in
  (x, s')


(*s Adding a new equality between variables and infer new facts. *)

let rec merge e s =
  Trace.call 6 "Merge(a)" e Veq.pp;
  let (s', es') = merge1 e s in
  Trace.exit 6 "Merge(a)" es' Veqs.pp;
  (s', es')

and merge1 e s =
  Trace.msg 3 "Merge1(a):" s pp;
  let not_is_slack x = not (is_slack x) in
  let (x,y) = Veq.destruct e in
  if not (A.occurs s.a x) then
    (s, Veqs.empty)
  else 
    let a' = A.norm s.a x and b' = A.norm s.a y in
    try
      match Arith.solve not_is_slack (a',b') with
	| None -> (s, Veqs.empty)
	| Some(x, b) ->
	    assert(not_is_slack x);
            Trace.msg 3 "Merge1_after_solve(a):" s pp;
	    let (a',veqs') = A.compose s.a [(x,b)] in
	    ({s with a = a'}, Veqs.remove e veqs')
    with
	Exc.Unsolved ->
	  Trace.exit 7 "Solve(a)" "Unsolved for nonslack" Pretty.string;
	  try
	  match Arith.solve is_slack (a',b') with
	    | None -> 
		(s, Veqs.empty)
	    | Some(k, b) ->
		let (s',veqs') = propagate s (k, b) in
		let s'' = remove s' k in
		let veqs'' = Veqs.remove e veqs' in
		(s'', veqs'')
	  with
	      Exc.Unsolved ->
		Trace.exit 7 "Solve(a)" "Unsolved for slack" Pretty.string;
		(s, Veqs.empty)

and propagate s (k, b) = 
  assert(is_slack k);
  match cnstrnt s k, cnstrnt s b with
    | Some(ck), Some(cb) ->
	(match Cnstrnt.cmp ck cb with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent
	   | (Binrel.Super | Binrel.Same) ->
	       let (a', veqs') = A.propagate s.a [(k,b)] in
	       let s' = {s with a = a'} in
	       (s', veqs')
	   | Binrel.Sub ->
	       propagate_overlap s (k,b) ck
	   | Binrel.Singleton(q) ->
	       let (a', veqs') = A.propagate s.a [k, Arith.mk_num q] in
	       let s' = {s with a = a'} in
	       (s', veqs')
	   | Binrel.Overlap(ckb) ->
	       propagate_overlap s (k,b) ckb)
    | None, Some(cb) ->
	assert false
    | Some(ck), None ->
	assert false
    | None, None ->
	(s, Veqs.empty)

and propagate_overlap s (k,b) c  =
 let (b, c) = normalize (b, c) in
 let (c', seqs') = C.add (b, c) s.c in
 let (a', veqs') = A.propagate s.a ((k,b) :: seqs') in
 let s' = {s with a = a'; c = c'} in
 (s', veqs')

and normalize (a,c) =
  match Arith.decompose a with
    | (q, None) -> 
	(a,c)
    | (q, Some(p, a')) ->
	let cq = Cnstrnt.mk_singleton (Mpa.Q.minus q) in
	let c' = Cnstrnt.multq (Mpa.Q.inv p) (Cnstrnt.add c cq) in
	(a',c')

and remove s k =
  assert(is_slack k);
  if A.occurs s.a k then
    s
  else 
    {s with c = C.restrict k s.c}
    

(*s Adding a constraint. *)

let add (x,d) s =
  assert(is_var x && is_external x);
  Trace.msg 6 "Add(a)" (x,d) (Pretty.infix Term.pp "in" Cnstrnt.pp);
  if A.mem s.a x then
    let a = apply s x in
    if is_slack a then
      let (c', seqs') = C.add (a,d) s.c in 
      let (a', veqs') = A.propagate s.a seqs' in
      ({s with c = c'; a = a'}, veqs')
    else 
      let k = mk_slack () in
      let c' = C.extend (k,d) s.c in    
      merge1 (Veq.make k x) {s with c = c'}
  else     
    let k = mk_slack () in
    let a' = A.union x k s.a in
    let c' = C.extend (k,d) s.c in
    ({a = a'; c = c'}, Veqs.empty)

