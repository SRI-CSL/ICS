
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

type t = Cnstrnt.t Term.Map.t


(*s Check if [x] is in the domain. *)

let mem = Term.Map.mem


(*s Constraints as a list. *)

let to_list s =
  Term.Map.fold (fun x c acc -> (x,c) :: acc) s []


(*s Pretty-printing. *)

let rec pp fmt s = 
  Pretty.list Term.pp_in fmt (to_list s)


(*s Accessors. *)


let rec cnstrnt s a =
  let rec loop a =
  match Arith.d_interp a with
    | Some(Sym.Num(q), []) -> Cnstrnt.mk_singleton q
    | Some(Sym.Mult, l) -> Cnstrnt.multl (List.map loop l)
    | Some(Sym.Add, l) -> Cnstrnt.addl (List.map loop l)
    | Some(Sym.Expt(n), [x]) -> Cnstrnt.expt n (loop x)
    | _ ->
	Term.Map.find a s  (* raise [Not_found] if [a] not in domain. *)
  in 
  try
    Some(loop a)
  with
      Not_found -> None


(*s The empty constraint map. *)

let empty = Term.Map.empty


(*s Test for emptyness. *)

let is_empty s = (s == empty)


(*s Extend domain. *)

let extend (x,d) s =
  assert(not(mem x s));
  Trace.msg 6 "Extend(c)" (x,d) Term.pp_in;
  if Cnstrnt.is_empty d then
    raise Exc.Inconsistent
  else
    Term.Map.add x d s


(*s Restrict domain. *)

let restrict x s =
  if Term.Map.mem x s then
    Trace.msg 6 "Restrict(c)" x Term.pp;
  Term.Map.remove x s


(*s Normalize constraint such as ['2 * x + 5' in 'c'] 
 to ['x' in '1/2 ** (c -- 5)'], where ['**'], ['--']
 are abstract interval operations for linear multiplication and subtraction. *)

let normalize (a, c) =
  match Arith.decompose a with
    | None -> 
	(a, c)
    | Some(q, p, a') ->
	let c' = Cnstrnt.multq (Mpa.Q.inv p) (Cnstrnt.addq (Mpa.Q.minus q) c) in
	(a', c')

(*s Adding a constraint. *)
    
let rec add (a,c) s =
  let (a, c) = normalize (a, c) in
  Trace.msg 6 "Add(c)" (a,c) Term.pp_in;
  if is_var a then
    add_var (a,c) (s, [])
  else 
    add_nonvar (a,c) (s, [])

and add_var (x,c) (s, eqs) =
  (* assert(is_var x); *)
  try
    let d = Term.Map.find x s in
    match Cnstrnt.cmp c d with
      | Binrel.Disjoint ->
	  raise Exc.Inconsistent
      | (Binrel.Super | Binrel.Same) ->
	  (s, eqs)
      | Binrel.Sub ->
	  (Term.Map.add x c s, eqs)
      | Binrel.Singleton(q) ->
	  (Term.Map.remove x s, (x, Arith.mk_num q) :: eqs)
      | Binrel.Overlap(cd) ->
	  (Term.Map.add x cd s, eqs)
  with
      Not_found -> 
	if Cnstrnt.is_empty c then
	  raise Exc.Inconsistent
	else match Cnstrnt.d_singleton c with
	  | Some(q) ->
	      (Term.Map.remove x s, (x, Arith.mk_num q) :: eqs)
	  | None ->
	      (Term.Map.add x c s, eqs)

and add_nonvar (a,c) (s, eqs) =
  Trace.msg 1 "Possible Incompleteness" (a,c) Term.pp_in;
  match Arith.monomials a with
    | [] ->
	if Cnstrnt.mem Mpa.Q.zero c then
	  (s, eqs)
	else 
	  raise Exc.Inconsistent
    | [m] ->
	let (q,k) = Arith.mono_of m in
	add_var (k, (Cnstrnt.multq (Mpa.Q.inv q) c)) (s, eqs)
    | m :: ml ->
	let (q,k) = Arith.mono_of m in  (* [a = q * k + b] *)
	let b = Arith.mk_multl ml in
	match cnstrnt s b with   
	  | Some(d) ->  (* [q*k+b in c], [b in d] implies [k in 1/q * c - 1/q * d] *)
	      let qinv = Mpa.Q.inv q in
	      let c' = Cnstrnt.multq qinv c in
	      let d' = Cnstrnt.multq (Mpa.Q.minus qinv) d in
	      add_var (k, Cnstrnt.add c' d') (s, eqs)
	  | None ->
	       add_var (a, c) (s,eqs)
	      
	      
	
 
