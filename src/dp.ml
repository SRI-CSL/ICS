
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
open Sym
open Term
open Hashcons
open Binrel
open Th
(*i*)

(*s [is_dom a] holds for uninterpreted terms and interpreted constants.
 Terms in equalities that are exchanged between theories satisfy this
 constraint. *)

let is_dom a =             
  let f, l = Term.destruct a in
  Sym.is_uninterp f || l = []


(*s Decision procedure state. *)

type t = {
  mutable ctxt: Atom.Set.t;     (* current context. *)
  mutable a : A.t;              (* Arithmetic *)
  mutable t : T.t;              (* Tuples. *)
  mutable bv : BV.t;            (* Bitvectors. *)
  mutable u: U.t;               (* congruence closure data structure. *)
  mutable c : C.t;              (* constraints. *)
  mutable d : D.t;              (* Disequalities. *)
  mutable nla : NLA.t;          (* nonlinear arithmetic terms. *)
  mutable p : Prop.t            (* propositional cases. *)
}

let ctxt s = s.ctxt

let a_of s = s.a
let t_of s = s.t
let bv_of s = s.bv
let nla_of s = s.nla
let u_of s = s.u
let d_of s = s.d
let p_of s = s.p
let c_of s = s.c

let empty = {
  ctxt = Atom.Set.empty;
  a = A.empty;
  t = T.empty;
  bv = BV.empty;
  u = U.empty;
  d = D.empty;
  nla = NLA.empty;
  c = C.empty;
  p = Prop.mk_tt
}

let copy s = {
  ctxt = s.ctxt;
  a = s.a;
  t = s.t;
  bv = s.bv;
  u = s.u;
  d = s.d;
  nla = s.nla;
  p = s.p;
  c = s.c
}

let eq s s' =
  s.ctxt == s'.ctxt &&
  s.a == s'.a &&
  s.t  == s'.t &&
  s.bv == s'.bv &&
  s.u == s'.u &&
  s.d == s'.d &&
  s.nla == s'.nla &&
  s.c == s'.c &&
  Prop.eq s.p s'.p
  
let pp fmt s =
  Pretty.list Pretty.atom fmt (Atom.Set.elements s.ctxt)

(*s Compute best constraints. As long as the dynamic type table
 is empty, one can simply use [of_term0] which computes types
 from static information only. This is more efficient, since
 [of_term0] is memoized. *)

let rec cnstrnt s a =
  if Ptmap.is_empty (C.cnstrnt_of s.c) then
    Cnstrnt.of_term0 a 
  else 
    let ctxt x = C.apply s.c (U.can s.u x) in
    Cnstrnt.of_term ctxt a          (* may throw exception [Not_found]. *)

let is_diseq s a b =
  Term.is_diseq a b || D.is_diseq s.d a b


(*s Extending the domain of the context [th] with a binding [x |-> a]. *)

let extend th s (x,a) =
  match th with  
    | Sym.A ->                (* Avoids slack variables in domain whenever possible *)
        s.a <- A.process (x,a) s.a
    | Sym.NLA -> 
	s.nla <- NLA.extend (x,a) s.nla 
    | Sym.T -> 
	s.t <- T.extend (x,a) s.t
    | Sym.BV -> 
	s.bv <- BV.extend (a,x) s.bv
    | _ -> 
	s.u <- U.extend (x,a) s.u



(*s Incomplete test for inclusion. *)

let rec sub f s t =
  if is_sub f s t then
    Three.Yes
  else if is_sub f t s then
    Three.No
  else
    Three.X

and is_sub f s t =
  Atom.Set.for_all 
    (fun x -> 
       let x' = f s x in 
       Atom.Set.exists 
	 (fun y -> 
	    let y' = f t y in
	    Atom.eq x' y')
	 t.ctxt)
    s.ctxt
