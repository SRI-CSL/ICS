
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
open Hashcons
open Mpa
open Format
(*i*)


type term = 
  | App of Sym.t * t list

and t = term hashed

and set = term Ptset.t

and 'a map = (term,'a) Ptmap.t


(*s Equalities *)

let eql l m =
  try List.for_all2 (===) l m with Invalid_argument _ -> false

let rec eq a b =
  match a, b with
    | App(f,l), App(g,m)  -> Sym.eq f g && eql l m

(*s Hashconsing Terms. *)

module HashTrm = Hashcons.Make(           (*s Hashconsing of terms. *)
  struct 
    type t = term

    let equal = eq
     
    let hash = function
      | App(f,[]) -> Sym.tag f
      | App(f,l) -> (Sym.tag f + (List.fold_left (fun h a -> h + a.tag) 1 l)) land 0x3FFFFFFF
  end)

let ht = HashTrm.create 107
  let _ = Tools.add_at_reset (fun () -> HashTrm.clear ht)

let make (f,l) = HashTrm.hashcons ht (App(f,l))


let destruct a =
  match a.node with
    | App(f,l) -> (f,l)

let sym_of a = match a.node with App(f,_) -> f
let args_of a = match a.node with App(_,l) -> l


(*s Equality of terms. *)

let eq = (===)

(*s Efficient comparisons using hashconsing. Notice, this
    function depends on the memory layout, and is therefore
    session-specific *)

let fast_cmp a b = a.tag - b.tag		       
	
(*s Structural comparison. *)

let rec cmp a b =
  let f,l = destruct a and g,m = destruct b in
  match l,m with
    | [], [] ->
	(match Sym.is_interpreted_const f, Sym.is_interpreted_const g with
	   | true, false -> -1
	   | false, true -> 1
	   | _ -> Sym.cmp f g)
    | [], _::_ -> -1
    | _::_, [] -> 1
    | _ ->
	let c1 = Sym.cmp f g in
	if c1 != 0 then c1 else cmpl l m

and cmpl l m =
  let rec loop c l m =
    match l, m with
      | [], [] -> c
      | [], _  -> -1
      | _,  [] -> 1
      | x::xl, y::yl -> 
	  if c != 0 then loop c xl yl else loop (cmp x y) xl yl
  in
  loop 0 l m


let (<<<) a b = (cmp a b <= 0)

(*s [cmp] forces function applications to be smaller than constants.  
  Thus, [cmp] makes sure that e.g. [f(x) = c] is added in  this order.
  Otherwise, the term ordering is arbitrary and we use [Term.cmp] for
  ordering all the other cases. *)

let order a b =
  if a <<< b then (b,a) else (a,b)

(*s Some recognizers. *)

let is_const t =
  match t.node with
    | App(f,[]) -> Sym.is_interpreted_const f
    | _ -> false

let is_interp_const a =
  match destruct a with
    | f, [] -> Sym.is_interpreted_const f
    | _ -> false

let is_interp a =
  match a.node with
    | App(f,_) -> Sym.is_interp f

let is_uninterpreted a =
  match a.node with
    | App(f,_) -> not(Sym.is_interp f)


(*s Test is arguments are known to be disequal. *)

let is_diseq a b =
  (a =/= b) && 
  match destruct a, destruct b with
    | (f, []), (g, []) ->
	Sym.is_interpreted_const f && Sym.is_interpreted_const g
    | _ ->
	false


(*s Mapping over list of terms. Avoids unnecessary consing. *)

let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	if a' === a && l1 == l1' then l else a' :: l1'
	

(*s Homomorphisms on terms. [f] is applied to arguments [bi],
    if [bi] equals [f(bi)] for all [i], then the original term [a]
    is returned, otherwise a new term is constructed using [op]. *)

let hom1 a op f b =
  let b' = f b in
  if b' === b then a else op b'

let hom2 a op f (b1,b2) =
  let b1' = f b1 and b2' = f b2 in
  if b1' === b1 && b2' === b2 then a else op(b1',b2')

let hom3 a op f (b1,b2,b3) =
  let b1' = f b1 and b2' = f b2 and b3' = f b3 in
  if b1' === b1 && b2' === b2 && b3' === b3 then a else op(b1',b2',b3')

let homl a op f l =
  let l' = mapl f l in
  if eql l l' then a else op l'

(*s Association lists for terms. *)
    
let assq = List.assq

(*s Get theory of top-level function symbol. *)

let theory_of a =
  Sym.classify (sym_of a)

(*s Fold operator over terms. *)

let rec fold f a acc =
  match a.node with
    | App(_,l) -> f a (List.fold_right f l acc)

(*s Is [a] a subterm of [b]. *)

let is_subterm a b =
  let rec occ b =
    a === b || 
       match b.node with 
	 | App(_,l) -> List.exists occ l
  in 
  occ b


(*s Iteration over terms. *)

let rec iter f a  =
  f a;
  match a.node with
    | App(_,l) -> List.iter (iter f) l


let rec for_all p a  =
  p a && List.for_all (for_all p) (args_of a)


(*s Printer. *)

let rec pp fmt a =
  let f,l = destruct a in
  Format.fprintf fmt "@["; 
  Sym.pp false fmt f; 
  Tools.ppl ("(", ", ", ")") pp fmt l; 
  Format.fprintf fmt "@]"

let ppeqn fmt (a,b) =
  Format.fprintf fmt "@[("; 
  pp fmt a;
  Format.fprintf fmt " = ";
  pp fmt b;
  Format.fprintf fmt ")@]"
