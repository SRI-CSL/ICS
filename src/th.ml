
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
 * Author: Harald Ruess, N. Shankar
 i*)

(*i*)
open Term
open Interp
(*i*)

(*s Shostak theories. *)

module T = Sth.Make(
  struct
    let map = Tuple.map
    let solve = Tuple.solve
  end)

module Bv = Sth.Make(
  struct
    let map = Bitvector.map
    let solve = Bitvector.solve
  end)

(*s Context for interpreted equalities. *)

type t = {
  a : A.t;
  t : T.t;
  bv : Bv.t
}

(*s Empty context. *)

let empty = {
  a = A.empty;
  t = T.empty;
  bv = Bv.empty
}

(*s Solutions. *)

let solutions i s =
  match i with
    | A -> A.solutions s.a
    | T -> T.solutions s.t
    | BV -> Bv.solutions s.bv


(*s Pretty-printing *)

let rec pp fmt s =
  pp_solution (Interp.name_of A) fmt (A.solutions s.a);
  pp_solution (Interp.name_of T) fmt (T.solutions s.t);
  pp_solution (Interp.name_of BV) fmt (Bv.solutions s.bv);
  pp_cnstrnt fmt (A.cnstrnts s.a)


and pp_solution name fmt sols =
  if not(Solution.is_empty sols) then
    begin
      Format.fprintf fmt "\n%s:" name;
      Solution.pp fmt sols
    end

and pp_cnstrnt fmt c =
  let l = Map.fold (fun x i acc -> (x, i) :: acc) c [] in
  if l <> [] then
    begin
      Format.fprintf fmt "\nc:";
      Pretty.map Term.pp Cnstrnt.pp fmt l
    end


(*s Canonizer for purely interpreted terms. *)

let sigma f l = 
  match Sym.destruct f with
    | Sym.Interp(Sym.Arith(op)) -> Arith.sigma op l
    | Sym.Interp(Sym.Tuple(op)) -> Tuple.sigma op l
    | Sym.Interp(Sym.Bv(op)) -> Bitvector.sigma op l
    | _ -> App.sigma f l


type solvedform =
  | Solved of (Term.t * Term.t) list
  | Unsat
  | Unsolved

let solve i e =
  try
    Solved(
      match i with
	| A ->
	    (match Arith.solve (fun _ -> Cnstrnt.mk_real) e with 
	     | None -> [] 
	     | Some(sl, _) -> sl)
	| T -> Tuple.solve e
	| BV -> Bitvector.solve e)
  with
    | Exc.Unsolved -> Unsolved
    | Exc.Inconsistent -> Unsat
      
	      
let find i s =
  match i with
   | A -> A.find s.a     
   | T -> T.find s.t
   | BV -> Bv.find s.bv

let inv i s =
  match i with
    | A -> A.inv s.a
    | T -> T.inv s.t
    | BV -> Bv.inv s.bv

let use i s = 
  match i with
    | A -> A.use s.a
    | T -> T.use s.t
    | BV -> Bv.use s.bv


(*s Extending the state. *)

let extend i b s =
  let (x',s') = 
   match i with
     | A -> let (x', a') = A.extend b s.a in (x', {s with a = a'})
     | T -> let (x', t') = T.extend b s.t in (x', {s with t = t'})
     | BV -> let (x', bv') = Bv.extend b s.bv in (x', {s with bv = bv'})
  in
  (x', s')


(*s Add a constraint. *)

let add c (v, d, s) = 
  let (v', d', a', focus') = A.add c (v, d, s.a) in
  let s' = {s with a = a'} in
  (v', d', s', focus')

let close (v, d, s, focus) =
  let (v1, d', a1, focus1) = A.close (v, d, s.a, focus) in
  let (v2, t2, vfocus2) = T.close (v1, s.t, Focus.v focus) in
  let (v', bv3, vfocus3) = Bv.close (v2, s.bv, Focus.v focus)
  in
  let focus' = Focus.add_v vfocus2 (Focus.add_v vfocus3 focus1) in
  let s' = {s with a = a1; t = t2; bv = bv3} in
  (v', d', s', focus')

(*s Constraint of a term. *)

let cnstrnt (v,s) = A.cnstrnt (v, s.a)


(*s List all constraints with finite extension. *)

let split s = 
  List.map (
    fun (x, i) -> 
      Atom.mk_in i x) 
    (A.split s.a)


(*s Instantiation. *)

let inst v s =
  {s with 
     a = A.inst v s.a;
     t = T.inst v s.t;
     bv = Bv.inst v s.bv}




