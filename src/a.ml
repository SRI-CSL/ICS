
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
    let k = Term.mk_fresh slackname in
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
      Pretty.string fmt "\n";
    end;
  if not(C.is_empty s.c) then
    begin
      Pretty.string fmt "c:"; 
      C.pp fmt s.c;
      Pretty.string fmt "\n"
    end
  



(*s Accessors. *)

let solution_of s = A.solution s.a
let cnstrnt_of s = C.cnstrnts s.c

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
	(match C.cnstrnt_of a s.c with
	   | Some(c) -> c
	   | None -> loop (apply s a))      (* might raise [Not_found] *)
  in
  try
    Some(loop a) 
  with 
      Not_found -> None


(*s Adding a new equality between variables and infer new facts. *)

let merge1 e s =
  let (x,y) = Veq.destruct e in
  if A.occurs s.a x then
    let sl = Arith.solve (A.norm s.a x, A.norm s.a y) in
    let (a', es', is') = A.compose s.a sl in
    let (es'', c'') = C.merge is' s.c in
    ({s with a = a'; c = c''}, Veqs.union es' es'')
  else
    (s, Veqs.empty)

let rec mergel es s acc =
  if Veqs.is_empty es then
    (s, acc)
  else 
    let (e,es') = Veqs.destruct es in
    let (s',new') = merge1 e s in
    let acc' = Veqs.union acc new' in
    mergel es' s' acc'

let merge e s =
  Trace.call 6 "Merge(a)" e Veq.pp;
  let (s', es') = merge1 e s in
  Trace.exit 6 "Merge(a)" es' Veqs.pp;
  (s', es')


(*s Extend the domain of equality map. *)

let extend b s =
  Trace.msg 6 "Extend(a)" b Term.pp;
  let (x,a') = A.extend b s.a in
  (x, {s with a = a'})


(*s Adding a constraint. *)

let add (x,d) s =
  assert(is_external x);
  Trace.msg 6 "Add(a)" (x,d) (Pretty.infix Term.pp " in " Cnstrnt.pp);
  if A.mem s.a x then
    let a = apply s x in
    if is_slack a then
      let (veqs',c') = C.add (a, d) s.c in
      mergel veqs' {s with c = c'} veqs'
    else 
      failwith "add: to do"
  else     
    let k = mk_slack () in
    let s' = {s with a = A.union x k s.a ;c = C.extend (k,d) s.c} in
    (s', Veqs.empty)





