
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
open Dp
open Hashcons
(*i*)

(*s Canonization of terms *)

let rec term s a = 
  let f,l = Term.destruct a in
  if l = [] then         (* Both interpreted and uninterpreted constants. *)
    U.can s.u a          (* are in the [u] structure. *)
  else if f === Sym.mk_unsigned && List.length l = 1 then
    unsigned s (List.hd l)
  else 
    match Sym.destruct f with
      | Sym.Uninterp(x,sgn) ->
	  uninterp s (x,sgn) l
      | Sym.Interp(f) -> 
	  interp s f l 

and unsigned s a =
  let a' = term s a in
  try
    term s (Builtin.mk_unsigned (Th.BV.apply s.bv a'))
  with
      Not_found -> U.can s.u (Builtin.mk_unsigned a')
  

and uninterp s f l =
  let l' = Term.mapl (term s) l in
  let a' = Uninterp.mk_uninterp f l' in 
  U.can s.u a'
   
and interp s f =
  match f with
    | Sym.Arith(op) -> arith s op
    | Sym.Tuple(op) -> tuple s op
    | Sym.Bv(op) -> bv s op
    | Sym.Nonlin(op) -> nla s op 
    | Sym.Enum _ | Sym.Bool _ -> assert false

and arith s op l =
  let f x = Th.A.find s.a (term s x) in
  let a = Linarith.sigma op (Term.mapl f l) in
  try U.can s.u (Th.A.inv s.a a) with Not_found -> a

and tuple s op l =
  let f x = Th.T.find s.t (term s x) in
  let a = Tuple.sigma op (Term.mapl f l) in
  try U.can s.u (Th.T.inv s.t a) with Not_found -> a

and bv s op l =
  let f x = Th.BV.find s.bv (term s x) in
  let a = Bv.sigma op (Term.mapl f l) in
  try U.can s.u (Th.BV.inv s.bv a) with Not_found -> a

and  nla s op l =
  let f x = Th.NLA.find s.nla (term s x) in
  let a = Nonlin.sigma op (Term.mapl f l) in
  try U.can s.u (Th.NLA.inv s.nla a) with Not_found -> a


(*s Canonization of Atoms. *)

let rec atom s a =
  match a with
    | Atom.True -> Atom.mk_true
    | Atom.Equal(x,y) -> equal s (term s x) (term s y)
    | Atom.Diseq(x,y) -> diseq s (term s x) (term s y)
    | Atom.In(c,x) -> inn s c (term s x)
    | Atom.False -> Atom.mk_false

and equal s a b =
  if a === b then
    Atom.mk_true
  else if Type.is_disjoint (Dp.cnstrnt s a) (Dp.cnstrnt s b) then
    Atom.mk_false
  else if Dp.is_diseq s a b then
    Atom.mk_false
  else
    Atom.mk_equal a b

and inn s c a =
  match Type.destruct (Dp.cnstrnt s a) with  
    | Type.Top -> 
	Atom.mk_in c a
    | Type.Number(d) ->
	(match Number.cmp c d with
	   | (Binrel.Same | Binrel.Super) -> 
	       Atom.mk_true
	   | Binrel.Disjoint -> 
	       Atom.mk_false
	   | Binrel.Sub -> 
	       Atom.mk_in c a
	   | Binrel.Overlap ->
	       Atom.mk_in (Number.inter c d) a)
    | _ ->
	Atom.mk_false

and diseq s a b =
  if a === b then 
    Atom.mk_false
  else if Dp.is_diseq s a b then
    Atom.mk_true
  else if Type.is_disjoint (Dp.cnstrnt s a) (Dp.cnstrnt s b) then
    Atom.mk_true
  else
    Atom.mk_diseq a b


(*s Canonization of Propostions. *)

let rec prop s a =
  match Prop.destruct a with
    | Prop.True ->
	Prop.mk_tt
    | Prop.False ->
	Prop.mk_ff
    | Prop.Ite(x,p,n) ->
	Prop.mk_ite (atom s x) (prop s p) (prop s n)
