
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

 module Cod = struct

   type t = {                             (* Constraints are composed of explicit constraints. *)
     cnstrnt: Cnstrnt.t;                  (* and a list of pairs [(c,a)] which represents the *)
     diffs: (Cnstrnt.t * Term.t) list     (* constraint [c] minus the constraint of term [a]. *)
   }

   let make (e, dl) = { cnstrnt = e; diffs = dl}

   let destruct { cnstrnt = e; diffs = dl} = (e, dl)

   let of_cnstrnt c = { cnstrnt = c; diffs = [] }

   let mk_real = of_cnstrnt Cnstrnt.mk_real

   let rec pp fmt s = 
     Cnstrnt.pp fmt s.cnstrnt;
     if s.diffs <> [] then
       begin
	 Pretty.string fmt " inter ";
	 pp_diffs fmt s.diffs
       end

   and pp_diffs fmt =
     Pretty.infixl
       (fun fmt (c,a) ->
	  Cnstrnt.pp fmt c;
	  Pretty.string fmt " - Cnstrnt(";
	  Term.pp fmt a;
	  Pretty.string fmt ")")
       " inter "
       fmt

   let status s =
     if Cnstrnt.is_empty s.cnstrnt then
       Status.Empty
     else
       match Cnstrnt.d_singleton s.cnstrnt with
	 | Some(q) -> 
	     Status.Singleton(q)
	 | None ->
	     if Cnstrnt.is_full s.cnstrnt && s.diffs = [] then
	       Status.Full
	     else
	       Status.Other

         (*s Meaning of a symbolic constraint is obtained by
           instantiating all symbolic constraints with explicit constraints. *)
	
   let rec meaning cnstrnt s = 
     let rec symbolic1 (c, a) = 
       Cnstrnt.subtract c (cnstrnt a)
     and symbolic l = 
       match l with
	 | [] -> Cnstrnt.mk_real
	 | [(c,a)] -> symbolic1 (c,a)
	 | (c,a) :: xl -> Cnstrnt.inter (symbolic1 (c,a)) (symbolic xl)
     in
     if s.diffs = [] then
       s.cnstrnt
     else 
       Cnstrnt.inter s.cnstrnt (symbolic s.diffs)

    
	       (*s [occurs x s] holds if [x] occurs uninterpreted in s. *)

   let occurs x s =
     List.exists (fun (_,a) -> Term.subterm x a) s.diffs

       (*s Replace [x] by [b] in constraint and take care of symbolic constraints
	 which become explicit in the process. *)

   let replace x b s =
     if not(occurs x s) then
       s
     else 
       let repl a = Arith.replace x b a in
       let (e', sl') = 
	 List.fold_right
	   (fun (c,a) ((e, sl) as acc) ->
	      let a' = repl a in
	      match Arith.d_num a' with
		| Some(q) ->
		    let e' = Cnstrnt.subtract e (Cnstrnt.mk_singleton q) in
		    (Cnstrnt.inter c e, sl)
		| None ->
		    (e, (c, a') :: sl))
	   s.diffs
	   (s.cnstrnt, [])
       in
       make (e', sl')

 end



(*s Constraint context as a map of term variables to constraints. *)

type t = {
  find : Cod.t Term.Map.t;
  use : Use.t;
}

let apply s x = Term.Map.find x s.find

let find s x = 
  try Term.Map.find x s.find with Not_found -> Cod.mk_real

let use s = Use.find s.use

(*s Check if [x] is in the domain. *)

let dom x s = Term.Map.mem x s.find


(*s Check if [x] is either in the domain or a subterm of an implicit constraint. *)

let mem x s =
  dom x s ||
  not(Term.Set.is_empty (use s x))



(*s [cnstrnt s a] computes best constraint for [a] in [s]. *)

let cnstrnt s a = 
  let rec of_term a =
    match Arith.d_interp a with
      | Some(Sym.Num(q), []) -> Cnstrnt.mk_singleton q
      | Some(Sym.Mult, l) -> Cnstrnt.multl (List.map of_term l)
      | Some(Sym.Add, l) -> Cnstrnt.addl (List.map of_term l)
      | Some(Sym.Expt(n), [x]) -> Cnstrnt.expt n (of_term x)
      | _ -> (apply s a).Cod.cnstrnt
  in
  try of_term a with Not_found -> Cnstrnt.mk_real
  
(*s [meaning s c] instantiates the symbolic constraint [c] in context [s]. *)

let meaning s = Cod.meaning (cnstrnt s)


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


(*s Use structure manipulations. *)

let remove_use x c = 
  List.fold_right (fun (_,a) -> Use.remove Arith.fold x a) c.Cod.diffs

let add_use x c =
  List.fold_right (fun (_,a) -> Use.add Arith.fold x a) c.Cod.diffs


(*s Extend domain of constraint map. *)

let extend c s =
  if Cnstrnt.is_empty c then
    raise Exc.Inconsistent
  else 
    let k = mk_slack () in
    Trace.msg 3 "Extend(c)" (k,c) Term.pp_in;
    let s' = {s with find = Map.add k (Cod.of_cnstrnt c) s.find} in
    (k, s')


(*s [update k c s] installs the new constraint [k in c] by overwriting 
 the [find] constraint *)

let update k c (s, eqs) =
  Trace.msg 3 "Update(c)" (k,c) Term.pp_in;
  let diffs' = (find s k).Cod.diffs in
  let find' = Term.Map.add k (Cod.make (c, diffs')) s.find in
  ({s with find = find'}, eqs)


(*s [remove x s] physically removes [x] from the [find] map and
 updates the [use] lists accordingly. *)

let remove x s =
  try
    let c = apply s x in  
    Trace.msg 3 "Restrict(c)" x Term.pp;
    {s with 
       find = Term.Map.remove x s.find; 
       use = remove_use x c s.use}
  with
      Not_found -> s


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
 

(*s Propagate an equality ['k = a'] between a slack
 parameter [k] and a combination [a] of slacks. *)

let rec propagate (k, a) s =
  try
    let c = (apply s k).Cod.cnstrnt in
    match Arith.decompose a with     
      | Arith.Const(q) ->              (* Case: [k = q] *)
	  if Cnstrnt.mem q c then 
	    (s, []) 
	  else 
	    raise Exc.Inconsistent
      | Arith.One(q, p, x) ->               (* Case: [q + p * x in c] *)
	  add k (normalize q p c) (s, [])   (* iff [x in 1/p * (c - q)] *)  
      | Arith.Many(q, p, x, y) ->      (* Case: [q + p * x + y in c] *)
	  let c' = normalize q p c in  (* iff [x in ((1/p*(c-q)) - C(1/p*y)] *)
	  let y' = Arith.mk_multq (Q.inv p) y in  
	  diff x (c', y') (s, [])
  with
      Not_found ->
	(s, [])

(*s Propagate an equality [k = q], where [q] is a rational number. *)

and const k q (s, eqs) = 
  try
    let c = (apply s k).Cod.cnstrnt in
    if Cnstrnt.mem q c then 
      let n = Arith.mk_num q in
      restrict (k, n) (s, (k, n) :: eqs) 
    else 
      raise Exc.Inconsistent
  with
      Not_found -> (s, eqs)

(*s Add a constraint [k in c] to [s]. *)

and add k c (s, eqs) = 
   try
     let d = (apply s k).Cod.cnstrnt in
     match Cnstrnt.cmp c d with
       | Binrel.Disjoint ->
	   raise Exc.Inconsistent
       | (Binrel.Super | Binrel.Same) ->
	   (s, eqs)
       | Binrel.Sub ->
	   update k c (s, eqs)
       | Binrel.Singleton(q) ->
	   let n = Arith.mk_num q in
	   restrict (k, n) (s, (k, n) :: eqs)
       | Binrel.Overlap(cd) ->
	   update k cd (s, eqs)
   with
       Not_found -> 
	 update k c (s, eqs)

and diff k (c,y) (s, eqs) =
  Trace.msg 0 "Diff(c)" "to do" Pretty.string;
  (s, eqs)

 
(*s Remove [x] from domain of cnstrnt assignments and replace every [x]
 with [b] in the symbolic constraints in which [x] occurs. *)

and restrict (x,b) (s, eqs) =
  let s' = remove x s in
  instantiate (x,b) (s', eqs)


(*s Instantiate [x] in symbolic constraints with [b] in [s]. *)

and instantiate (x, b) (s, eqs) =
  (s, eqs)
(*
  Term.Set.fold
    (fun y (s, eqs) ->
       try
	 let c = apply s y in
	 let c' = Cod.replace x b c in
	 match Cod.status c' with
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
*)
























(*

(*s [update x c s] installs the new constraint [x in c] by overwriting the
 [find] constraint and updating the use lists accordingly. *)

let update x c s =
  failwith "to do"

(*
  Trace.msg 3 "Update(c)" (x,c) Term.pp_in;
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
 
*)

(*s Instantiate [x] in symbolic constraints with [b] in [s]. *)

let rec instantiate (x, b) (s, eqs) =
  Term.Set.fold
    (fun y (s, eqs) ->
       try
	 let c = apply s y in
	 let c' = Cod.replace x b c in
	 match Cod.status c' with
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




(*s Updating constraint information. *)

let rec update_explicit x c (s, eqs) =
  let sc' = 
      try
	let sc = apply s x in
	Cod.make (c, sc.Cod.diffs)
      with
	  Not_found -> Cod.of_cnstrnt c
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
      let (e,s) = Cod.destruct (apply s x) in
      Cod.make (e, (c,a) :: s)
    with
	Not_found ->
	  Cod.make (Cnstrnt.mk_real, [(c,a)])
  in
  let find' = Map.add x sc' s.find in
  let use' = Use.add Arith.fold x a s.use in
  {find = find'; use = use'}
  


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
	       Trace.msg 0 "Cnstrnt(c)" (a,c) pp_in;
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
  failwith "to do"

(*

  assert(is_slack k);
  let ck = apply s k in
  let cb = cnstrnt s b in
    match Cnstrnt.cmp ck cb with
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
