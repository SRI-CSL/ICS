
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
(*i*)

type prop =
  | True
  | False
  | Ite of Atom.t * t * t

and t = prop Hashcons.hashed

let eq = (===)


(*s Hashconsing Propositions. *)

module HashProp = Hashcons.Make(    
  struct 
    type t = prop

    let equal b1 b2 =
      match b1, b2 with
	| True, True -> true
	| False, False -> true
	| Ite(x,p1,n1), Ite(y,p2,n2) ->
	    Atom.eq x y && p1 === p2 && n1 === n2
	| _ -> false
     
    let hash = function
      | True -> 0
      | False -> 1
      | Ite(x,p,n) -> ((p.tag + n.tag) land 0x3FFFFFFF)
  end)

let make = 
  let ht = HashProp.create 17 in
  let _ = Tools.add_at_reset (fun () -> HashProp.clear ht) in
  HashProp.hashcons ht

let destruct a = a.node

(*s Boolean function symbols, constructors, and recognizers. *)

let mk_tt = make(True)
let mk_ff = make(False)
let mk_poslit a = 
  match a with
    | Atom.True -> mk_tt
    | Atom.False -> mk_ff
    | _ -> make(Ite(a,mk_tt,mk_ff))

let mk_neglit a = 
  match a with
    | Atom.True -> mk_ff
    | Atom.False -> mk_tt
    | _ -> make(Ite(a,mk_ff,mk_tt))


(*s Recognizers. *)

let is_tt a = (a === mk_tt)
let is_ff a = (a === mk_ff)

type p = t  (* avoid name clash *)

module P3 = Hashtbl.Make(
  struct
    type t = p * p * p
    let equal (a1,a2,a3) (b1,b2,b3) =
      a1 === b1 && a2 === b2 && a3 === b3
    let hash = Hashtbl.hash
  end)

let applyhash = P3.create 17

let rec mk_apply p3 =
  try
    P3.find applyhash p3
  with Not_found ->
    let p = mk_apply1 p3 in
    P3.add applyhash p3 p; p

and mk_apply1 (a,b,c) =
  if b === c then
    b
  else if b === mk_tt && c === mk_ff then 
    a
  else 
    match a.node with
      | True -> b
      | False -> c
      | Ite(x,_,_) ->
	  let y = topvar x b c in
	  let (pa,na) = cofactors y a in
	  let (pb,nb) = cofactors y b in
	  let (pc,nc) = cofactors y c in
	  let p = mk_apply (pa,pb,pc) in
	  let n = mk_apply (na,nb,nc) in
	  if p === n then p else make(Ite(y,p,n))

and cofactors x a =
  match a.node with
    | Ite(y,p,n) when Atom.eq x y -> (p,n)
    | _ -> (a,a)
   
and topvar x a2 a3 =
  match a2.node, a3.node with
    | Ite(y,_,_), Ite(z,_,_) -> maxvar x (maxvar y z)
    | Ite(y,_,_), _ -> maxvar x y
    | _, Ite(z,_,_) -> maxvar x z
    | _ -> x
    
and maxvar x y =
  if Atom.(<<<) x y then y else x

(*s Derived constructors. *)

let mk_ite x p n = 
  if eq p n then p else
    match x with
      | Atom.True -> p
      | Atom.False -> n
      | _ -> mk_apply (mk_poslit x, p, n)

let mk_neg a = mk_apply (a, mk_ff, mk_tt)
let mk_conj a1 a2 = mk_apply (a1, a2, mk_ff)
let mk_disj a1 a2 = mk_apply (a1, mk_tt, a2)
let mk_xor a1 a2 = mk_apply (a1, (mk_neg a2), a2)
let mk_imp a1 a2 = mk_apply (a1, a2, mk_tt)
let mk_iff a1 a2 = mk_apply (a1, a2, (mk_neg a2))

(*s Collect all literals from a
 propositional formula. *)

let rec literals_of a =
  match a.node with
    | (True | False) ->
	Atom.Set.empty
    | Ite(x,p,n) ->
        let pl = literals_of p in
        let nl = literals_of n in
	Atom.Set.add x (Atom.Set.union pl nl)


(*s Destructuring a proposition into a list of
 atoms, interpreted as a conjunction, and the remaining proposition. *)

let rec d_conj a =
  match a.node with
    | (True | False) -> None
    | Ite(x,{node=True},{node=False}) ->
	Some([x],mk_tt)
    | Ite(x,{node=False},{node=True}) ->
	Some([Atom.mk_neg x],mk_tt)
    | Ite(a,p,{node=False}) ->
	(match d_conj p with
	   | None -> Some([a], p)
	   | Some(l,q) -> Some(a :: l , q))
    | _ ->
	None
