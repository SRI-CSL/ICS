
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


(*s Slack variables *)

let slacks = ref Term.Set.empty
let _ = Tools.add_at_reset (fun () -> slacks := Term.Set.empty)

let is_slack x = Term.Set.mem x !slacks

let mk_slack =
  fun () -> 
    let k = Term.mk_fresh_param "k" None in
    Trace.msg 10 "Slack" k Term.pp;
    slacks := Term.Set.add k !slacks;
    k

(*s Constraint context. *)

type t = {
  cnstrnt : (Cnstrnt.t * Set.t) Map.t;
  use : Use.t
}

let empty = {
  cnstrnt = Map.empty;
  use = Use.empty
}

let is_empty s = (s.cnstrnt == Map.empty)

let mem x s = Map.mem x s.cnstrnt

let use s = Use.find s.use

(*s Compute constraint for an arithmetic term. All
 undeclared variables are implicitly assumed to be real-valued. *)

let rec cnstrnt s a = 
  cnstrnt_of_term s a

and cnstrnt_of_term s a =
  match Arith.d_interp a with
    | Some(Sym.Num(q), []) -> 
	Cnstrnt.mk_singleton q
    | Some(Sym.Mult, l) -> 
	Cnstrnt.multl (List.map (cnstrnt_of_term s) l)
    | Some(Sym.Add, l) -> 
	Cnstrnt.addl (List.map (cnstrnt_of_term s) l)
    | Some(Sym.Expt(n), [x]) -> 
	Cnstrnt.expt n (cnstrnt_of_term s x)
    | _ ->
	cnstrnt_of_var s a

and cnstrnt_of_var s x =
  try
    let (c, ts) = Term.Map.find x s.cnstrnt in
    cnstrnt_of_terms s ts c
  with
      Not_found ->
	Cnstrnt.mk_real

and cnstrnt_of_terms s =
  Term.Set.fold
    (fun t c ->
       let d = cnstrnt_of_term s t in
       Cnstrnt.inter c d)


(*s Constraints as a list. *)

let to_list s =
  Term.Map.fold (fun x _ acc -> (x, cnstrnt s x) :: acc) s.cnstrnt []


(*s Pretty-printing. *)

let rec pp fmt s = 
  Pretty.list Term.pp_in fmt (to_list s)


(*s Use structure manipulations. *)

let remove_use x a = Use.remove Arith.fold x a
let add_use x a = Use.add Arith.fold x a


(*s Extend domain of constraint map. *)

let extend c s =
  if Cnstrnt.is_empty c then
    raise Exc.Inconsistent
  else 
    let k = mk_slack () in
    Trace.msg 6 "Extend(c)" (k,c) Term.pp_in;
    let s' = {s with cnstrnt = Map.add k (c, Set.empty) s.cnstrnt} in
    (k, s')


(*s Restricting domain. *)

let restrict k s =
  try
    let (_, ts) = Map.find k s.cnstrnt in
    let cnstrnt' = Map.remove k s.cnstrnt in
    let use' = Set.fold (remove_use k) ts s.use in
    {s with cnstrnt = cnstrnt'; use = use'}
  with
      Not_found -> s

(*s Adding an equality (k,b) to [s]. *)

let rec merge (k,b) s =
  Trace.msg 6 "Merge(c)" (k,b) pp_equal;
  let d = cnstrnt s b in
  try
    let (c, bs) = Term.Map.find k s.cnstrnt in
    match Cnstrnt.cmp c d with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | Binrel.Singleton(q) ->
	  if Set.is_empty bs then   (* not propagated yet. *)
	    (restrict k s, [(k, Arith.mk_num q)])
	  else 
	    let c' = Cnstrnt.mk_singleton q in
	    let s' = {s with cnstrnt = Term.Map.add k (c', bs) s.cnstrnt} in
	    (s', [])
      | (Binrel.Same | Binrel.Sub) ->
	  let s' = update k c (b, bs) s in
	  (s', [])
      | Binrel.Super ->
	  let s' = update k d (b, bs) s in
	  (s', [])
      | Binrel.Overlap(cd) ->
	  let s' = update k cd (b, bs) s in
	  (s', [])
  with
      Not_found ->
	let s' = update k d (b, Set.empty) s in
	(s', [])
	  
and update k c (b, bs) s =        (* add [k |-> (c, add(t,ts))], where *)
  let bs' = Set.add b bs in       (* [t] is a new equality term. *)
  {s with 
     cnstrnt = Term.Map.add k (c, bs') s.cnstrnt;
     use = add_use k b s.use}



(*

(*s [remove x s] physically removes [x] from the [find] map and
 updates the [use] lists accordingly. *)

let remove x s =
  try
    let c = apply s x in
    {s with find = Term.Map.remove x s.find; use = remove_use x c s.use}
  with
      Not_found -> s


(*s [update x c s] installs the new constraint [x in c] by overwriting the
 [find] constraint and updating the use lists accordingly. *)

let update x c s =
  let find' = Term.Map.add x c s.find in
  let use' = 
    try
      let d = Term.Map.find x s.find in
      add_use x c (remove_use x d s.use)
    with
	Not_found ->
	  add_use x c s.use
  in
  {s with find = find'; use = use'}


(*s Instantiate [x] in symbolic constraints with [b] in [s]. *)

let rec instantiate (x, b) (s, eqs) =
  Term.Set.fold
    (fun y (s, eqs) ->
       try
	 let c = apply s y in
	 let c' = Symbolic.replace x b c in
	 match Symbolic.status c' with
	   | Status.Empty ->
	       raise Exc.Inconsistent
	   | Status.Singleton(q) ->
	       let n = Arith.mk_num q in
	       let (s', eqs') = restrict (y, n) (s, eqs) in
	       (s', (y, n) :: eqs')
	   | _ ->
	       (update y c' s, eqs)
       with
	   Not_found -> (s, eqs))
    (use s x)
    (s, eqs)


(*s Remove [x] from domain of cnstrnt assignments and replace every [x]
 with [b] in the symbolic constraints in which [x] occurs. *)

and restrict (x,b) (s, eqs) =
  let s' = remove x s in
  instantiate (x,b) (s', eqs)


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
	     let n = Arith.mk_num q in
	     restrict (y,n) (s, (y, n) :: eqs)
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
	let n = Arith.mk_num q in
	restrict (a,n) (s, [(a,n)])
    | None ->
	(match Arith.decompose a with     
	   | Arith.Const(q) ->              (* Case: [a = q] *)
	       if Cnstrnt.mem q c then 
		 (s, []) 
	       else 
		 raise Exc.Inconsistent
	   | Arith.One(q, p, x) ->          (* Case: [q + p * x in c] *)
	       let c' = normalize q p c in  (* iff [x in 1/p * (c - q)] *)  
	       Trace.msg 6 "Explicit(c)" (a,c) pp_in;
	       add_explicit x c' (s, [])
	   | Arith.Many(q, p, x, y) ->      (* Case: [q + p * x + y in c] *)
	       let c' = normalize q p c in  (* iff [x in ((1/p * (c - q)) - cnstrnt(1/p * y)] *)
	       let y' = Arith.mk_multq (Q.inv p) y in  
	       Trace.msg 0 "Symbolic(c)" (a,c) pp_in;
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
	  let n = Arith.mk_num q in
	  restrict (x,n) (s, (x, n) :: eqs)
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
 

(*s Merging an equality between constraints. *)

let propagate (k, b) s =
  (s, [])
(*
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
    | _ ->
	(s, [])
*)

*)
