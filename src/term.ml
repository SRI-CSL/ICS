
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
open Format
(*i*)

type t = {
  sym : Sym.t; 
  args : t list
}

(*s Equalities *)

let rec eq a b = 
  (Sym.eq a.sym b.sym) &&
  (try 
     List.for_all2 eq a.args b.args
   with 
       Invalid_argument _ -> false)

(*s Sets and maps of terms. *)

type trm = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = trm
    let compare = Pervasives.compare
  end)

module Map = Map.Make(
  struct
    type t = trm
    let compare = Pervasives.compare
  end)


let mk_const f = { sym = f; args = [] }

let mk_var x = { sym = Sym.mk_uninterp x; args = [] }

let mk_app f l = { sym = f; args = l }

let destruct a = (a.sym, a.args)

let sym_of a = a.sym
let args_of a = a.args
	       
	
(*s Structural comparison. *)

let rec cmp a b =
  let c1 = Sym.cmp a.sym b.sym in
  if c1 != 0 then c1 else cmpl a.args b.args

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


(*s Boolean constants. *)

let ttsym = Sym.mk_uninterp (Name.of_string "true")
let ffsym = Sym.mk_uninterp (Name.of_string "false")

let mk_tt = mk_app ttsym [] 
let mk_ff = mk_app ffsym []

let is_tt a = a.args = [] && Sym.eq a.sym ttsym
let is_ff a = a.args = [] && Sym.eq a.sym ffsym

(*s Some recognizers. *)

let is_const a =
  a.args = []

let is_var a =
  a.args = [] && 
  (Sym.is_uninterp a.sym || Sym.is_internal a.sym)


let is_label a =
  (a.args = []) &&
  (match Sym.destruct a.sym with
     | Sym.Uninterp(Sym.Internal(Sym.Label _)) -> true
     | _ -> false)

let is_slack a =
  (a.args = []) &&
  (match Sym.destruct a.sym with
     | Sym.Uninterp(Sym.Internal(Sym.Slack _)) -> true
     | _ -> false)

let d_slack a = 
  assert(a.args = []);
  match Sym.destruct a.sym with
    | Sym.Uninterp(Sym.Internal(Sym.Slack(_,c))) -> Some(c)
    | _ -> None

let is_interp_const a =
  a.args = [] && Sym.is_interpreted_const a.sym
   
let is_interp a =
  Sym.is_interp a.sym

let is_uninterpreted a =
  not(Sym.is_interp a.sym)


(*s Test is arguments are known to be disequal. *)

let is_diseq a b =
  match destruct a, destruct b with
    | (f, []), (g, []) ->
	not(Sym.eq f g) &&
	Sym.is_interpreted_const f && Sym.is_interpreted_const g
    | _ ->
	false


(*s Mapping over list of terms. Avoids unnecessary consing. *)

let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	if eq a' a && l1 == l1' then l else a' :: l1'


(*s Association lists for terms. *)
    
let rec assq a = function
  | [] -> raise Not_found
  | (x,y) :: xl -> if eq a x then y else assq a xl

(*s Is [a] a subterm of [b]. *)

let is_subterm a b =
  let rec occ b =
    eq a b || List.exists occ b.args
  in 
  occ b

(*s Iteration over terms. *)

let rec fold f a acc =
  f a (List.fold_right (fold f) a.args acc)

let rec iter f a  =
  f a; List.iter (iter f) a.args

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


