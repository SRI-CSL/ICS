
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


(*s Context for interpreted equalities. *)

type t = {
  a : A.t;
  t : T.t;
  b : B.t;
  bv : Bv.t
}

(*s Empty context. *)

let empty = {
  a = A.empty;
  t = T.empty;
  b = B.empty;
  bv = Bv.empty
}

(*s Solutions. *)

let solution i s =
  match i with
    | A -> A.solution_of s.a
    | T -> T.solution_of s.t
    | B -> B.solution_of s.b
    | BV -> Bv.solution_of s.bv


(*s Pretty-printing *)

let pp fmt s =
  A.pp fmt s.a;
  T.pp fmt s.t;
  B.pp fmt s.b;
  Bv.pp fmt s.bv


(*s Canonizer for purely interpreted terms. *)

let sigma f l = 
  match Sym.destruct f with
    | Sym.Interp(Sym.Arith(op)) -> Arith.sigma op l
    | Sym.Interp(Sym.Tuple(op)) -> Tuple.sigma op l
    | Sym.Interp(Sym.Boolean(op)) -> Boolean.sigma op l
    | Sym.Interp(Sym.Bv(op)) -> Bitvector.sigma op l
    | _ -> App.sigma f l

let solve i e =
  match i with
    | A ->
	let p x = not (A.is_slack x) in
	(match Arith.solve p e with 
	   | None -> [] 
	   | Some(x,b) -> [(x,b)])
    | T -> Tuple.solve e
    | B -> Boolean.solve e
    | BV -> Bitvector.solve e

let find i s =
  match i with
   | A -> A.find s.a     
   | T -> T.find s.t
   | B -> B.find s.b
   | BV -> Bv.find s.bv

let inv i s =
  match i with
    | A -> A.inv s.a
    | T -> T.inv s.t
    | B -> B.inv s.b
    | BV -> Bv.inv s.bv

let use i s = 
  match i with
    | A -> A.use s.a
    | T -> T.use s.t
    | B -> B.use s.b
    | BV -> Bv.use s.bv


(*s Tracing. *)

let call funname i = 
  Trace.call 5 (funname ^ "(" ^ (name_of i) ^ ")")

let exit funname i = 
  Trace.exit 5 (funname ^ "(" ^ (name_of i) ^ ")")


(*s Extending the state. *)

let extend i b s =
  call "Extend" i b Term.pp;
  let (x,s') = 
   match i with
     | A -> let (x,a') = A.extend b s.a in (x, {s with a = a'})
     | T -> let (x,t') = T.extend b s.t in (x, {s with t = t'})
     | B -> let (x,b') = B.extend b s.b in (x, {s with b = b'})
     | BV -> let (x,bv') = Bv.extend b s.bv in (x, {s with bv = bv'})
  in
  exit "Extend" i x Term.pp;
  (x, s')


(*s Merging variables. *)

let rec mergel es s acc =
  if Veqs.is_empty es then
    (s, acc)
  else
    let (e',es') = Veqs.destruct es in
    let (s', acc') = merge1 e' s in
    mergel (Veqs.union es' acc') s' (Veqs.union acc acc')

and merge1 e s =
  let (a', el1) = A.merge e s.a
  and (t', el2) = T.merge e s.t
  and (b', el3) = B.merge e s.b 
  and (bv', el4) = Bv.merge e s.bv in
  ({s with a = a'; t = t'; b = b'; bv = bv'},
   Veqs.union el1 (Veqs.union el2 (Veqs.union el3 el4)))


let merge e s =
  let (s',es') = merge1 e s in
  (s',es')


(*s Add a constraint. *)

let add (x,c) s = 
  let (a',es') = A.add (x,c) s.a in
  mergel es' {s with a = a'} es'

let cnstrnt s = A.cnstrnt s.a
