
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
    Trace.msg 10 "Slack" k Term.pp;
    slacks := Term.Set.add k !slacks;
    k


(*s An external variable is a non-slack variable.  *)

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
      Not_found -> 
	if is_slack a then Some(Cnstrnt.mk_real) else None

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
 if is_slack a then
   refine_slack s (a, d)
 else
   let (a, d) = normalize (a, d) in
   if is_slack a then
     refine_slack s (a, d)
   else 
     refine_nonslack s (a, d)
     

and refine_slack s (k, c) =
  assert(is_slack k);
  let (sc', seqs') = C.add (k, c) s.c in
  ({s with c = sc'}, seqs')

and refine_nonslack s (a,c) =
  match cnstrnt s a with
    | Some(d) ->
	(match Cnstrnt.cmp c d with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent
	   | (Binrel.Super | Binrel.Same) ->  (* already known. *)
	       (s, [])
	   | _ ->
	       let k = mk_slack () in          (* this may loop... *)
	       ({s with c = C.extend (k, c) s.c}, [(k, a)]))
    | None ->
	let k = mk_slack () in
	({s with c = C.extend (k, c) s.c}, [(k, a)])

(*s Normalize constraint such as ['2 * x + 5' in 'c'] 
 to ['x' in '1/2 ** (c -- 5)'], where ['**'], ['--']
 are abstract interval operations for linear multiplication and subtraction. *)

and normalize (a,c) =
  match Arith.decompose a with
    | (q, None) -> 
	(a, c)
    | (q, Some(p, a')) ->
	let c' = Cnstrnt.multq (Mpa.Q.inv p) (Cnstrnt.addq (Mpa.Q.minus q) c) in
	(a', c')


(*s Propagate an equality between two terms. First, solve for an
 external variable if possible and compose the solution in the
 equality part. Otherwise, solve for a slack variable and propagate
 the solution on the rhs of the equality part. Besides external
 variable equalities his may also generate new constraints for 
 slack variables. *)

let rec propagate s (a,b) =
  Trace.msg 6 "Propagate(a)" (a,b) Term.pp_equal;
  let a = find s a and b = find s b in  (* propagate on interpretations. *)
  try
    match Arith.solve is_external (a,b) with
      | None -> 
	  (s, Veqs.empty, [])
      | Some(x, b) ->
	  propagate_external s (x, b)
  with
      Exc.Unsolved ->
	try
	  match Arith.solve (A.occurs s.a) (a,b) with
	    | Some(k,b) ->
		propagate_internal s (k,b)
	    | None ->
		(s, Veqs.empty, [])
	with
	    Exc.Unsolved ->  (* Ignore, since none of the slacks occurs in equality part of [s]. *)
	      Trace.exit 7 "Solve(a)" "Unsolved in propagation" Pretty.string;
	      (s, Veqs.empty, [])

and propagate_external s (x,b) = 
  let (sa', veqs') = A.compose s.a [(x, b)] in
  ({s with a = sa'}, veqs', [])

and propagate_internal s (k,b) = 
  match cnstrnt s k, cnstrnt s b with
    | Some(ck), Some(cb) ->
	(match Cnstrnt.cmp ck cb with
	   | Binrel.Disjoint ->
	       raise Exc.Inconsistent 
	   | (Binrel.Super | Binrel.Same) ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let sc' = C.restrict k s.c in
	       ({s with a = sa'; c = sc'}, veqs', [(k, cb)])
	   | Binrel.Sub ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let sc' = C.restrict k s.c in
	       Trace.msg 0 "Generate" (b,ck) Term.pp_in;
	       ({s with a = sa'; c = sc'}, veqs', [(b, ck)])
	   | Binrel.Singleton(q) ->
	       let (sa', veqs') = A.propagate s.a [k, Arith.mk_num q] in
	       let sc' = C.restrict k s.c in
	       ({s with a = sa'; c = sc'}, veqs', [])
	   | Binrel.Overlap(ckb) ->
	       let (sa', veqs') = A.propagate s.a [(k, b)] in
	       let sc' = C.restrict k s.c in
	       ({s with a = sa'; c = sc'}, veqs', [(b, ckb)]))
    | None, None ->
	(s, Veqs.empty, [])
    | _ ->
	failwith "Unreachable"


(*s Iterate [refine] and [propagate] until new new [seqs] equalities between slack
 variables or constraints [cnstrnts] are generated. It returns an updated state and
 a set of generated variable equalities. *)

let rec install s (seqs, cnstrnts) veqs = 
  match seqs, cnstrnts with
    | [], [] -> 
	(s, veqs)
    | (a,b) :: seqs', _ ->
	let (s', veqs', cnstrnts') = propagate s (a,b) in
	install s' (seqs', cnstrnts') (Veqs.union veqs' veqs)
    | _, (a,c) :: cnstrnts' ->
	let (s', seqs') = refine s (a,c) in
	install s' (seqs' @ seqs, cnstrnts') veqs

let install_equality s (a, b) =
  Trace.msg 5 "Internal(a)" (a,b) pp_equal;
  install s ([(a,b)], []) Veqs.empty
    
let install_cnstrnt s (a, c) =
  Trace.msg 5 "Cnstrnt(a)" (a, c) pp_in;
  install s ([], [(a,c)]) Veqs.empty


(*s Adding a new equality between variables and infer new facts. *)

let rec merge e s =
  Trace.call 6 "Merge(a)" e Veq.pp;
  let (s', veqs') = merge1 e s in
  Trace.exit 6 "Merge(a)" veqs' Veqs.pp;
  (s', Veqs.remove e veqs')

and merge1 e s =
  let (x,y) = Veq.destruct e in
  if eq x y || not (A.occurs s.a x) then
    (s, Veqs.empty)
  else 
    install_equality s (A.norm s.a x,  A.norm s.a y)
   


(*s Adding a constraint. *)

let rec add (x,c) s =
  assert(is_var x && is_external x);
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
	       install_cnstrnt s (x,cd))
    | None ->
	install_cnstrnt s (x,c)
