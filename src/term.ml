(*
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
 *)

open Mpa
open Format
open Sym


type t =
  | Var of Var.t
  | App of Sym.t * t list
 
let rec eq a b = 
  match a, b with
    | Var(x), Var(y) -> 
	Var.eq x y
    | App(f,l), App(g,m) -> 
	Sym.eq f g && eql l m
    | _ ->
	false

and eql al bl =
  try List.for_all2 eq al bl with Invalid_argument _ -> false


let mk_var x d = Var(Var.mk_var x d)

let mk_const f = App(f,[])
let mk_app f l = App(f,l)

module Hashq = Hashtbl.Make(
  struct
    type t = Mpa.Q.t
    let equal = Mpa.Q.equal
    let hash = Mpa.Q.hash
  end)

let mk_num =
  let table = Hashq.create 17 in
  let _ =  Tools.add_at_reset (fun () -> Hashq.clear table) in
    fun q ->
      try
	Hashq.find table q 
      with
	  Not_found ->
	    let n = App(Arith(Num(q)), []) in
	      Hashq.add table q n; n

let mk_rename x k d = Var(Var.mk_rename x k d)

let mk_fresh x k d = Var(Var.mk_fresh x k d)

let mk_slack k alpha d = Var(Var.mk_slack k alpha d)

let is_rename = function
  | Var(x) -> Var.is_rename x
  | _ -> false

let is_fresh = function
  | Var(x) -> Var.is_fresh x
  | _ -> false

let is_internal = function
  | Var(x) -> Var.is_rename x || Var.is_slack x 
  | _ -> false

let is_slack = function Var(x) -> Var.is_slack x | _ -> false

let is_var = function Var _ -> true | _ -> false
let is_app = function App _ -> true | _ -> false
let is_const = function App(_,[]) -> true | _ -> false

let is_intvar = function 
  | Var(x) -> 
      (match Var.dom_of x with
	 | Some(d) -> Dom.eq d Dom.Int
	 | _ -> false)
  | _ -> false

let is_realvar = function 
  | Var(x) -> 
      (match Var.dom_of x with
	 | Some(d) -> Dom.eq d Dom.Real
	 | _ -> false)
  | _ -> false

let to_var = function
  | Var(x) -> x
  | _ -> assert false

let name_of a =
  assert(is_var a);
  match a with Var(x) -> Var.name_of x | _ -> assert false

let destruct a =
  assert(is_app a);
  match a with App(f,l) -> (f,l) | _ -> assert false

let sym_of a = 
  assert(is_app a);
  match a with App(f,_) -> f | _ -> assert false

let args_of a = 
  assert(is_app a);
  match a with App(_,l) -> l | _ -> assert false

let poly_of a =
  match a with
    | App(Arith(op), l) ->
	(match op, l with
	   | Num(q), [] -> (q, [])
	   | Multq _, _ -> (Q.zero, [a])
	   | Add, ((x :: xl') as xl) ->
	       (match x with
		  | App(Arith(Num(q)), []) -> (q, xl')
		  | _ -> (Q.zero, xl))
	   | _ -> assert false)
    | _ -> 
	(Q.zero, [a])
	

let rec hash = function
  | Var(x) ->
      Var.hash x
  | App(f, l) ->
      (Sym.hash f + (List.fold_left (fun h a -> h + hash a) 1 l)) land 0x3FFFFFFF

let rec cmp a b =
  match a, b with
    | Var _, App _ -> 1
    | App _, Var _ -> -1
    | Var(x), Var(y) -> Var.cmp x y
    | App(f, l), App(g, m) ->
	let c1 = Sym.cmp f g in
	if c1 != 0 then c1 else cmpl l m
 
and cmpl l m =
  let rec loop c l m =
    match l, m with
      | [], [] -> c
      | [], _  -> -1
      | _,  [] -> 1
      | x:: xl, y:: yl -> 
	  if c != 0 then loop c xl yl else loop (cmp x y) xl yl
  in
  loop 0 l m


let (<<<) a b = (cmp a b <= 0)


let orient ((a, b) as e) =
  if cmp a b >= 0 then e else (b, a)

let min a b =
  if a <<< b then a else b

let max a b = 
  if a <<< b then b else a


(** Some recognizers. *)

let is_interp_const = function
  | App((Arith _ | Bv _ ), []) -> true
  | _ -> false
   
let is_interp = function
  | App((Arith _ | Bv _), _) -> true
  | _ -> false

let is_uninterpreted = function
  | App(Uninterp _, _) -> true
  | _ -> false


let is_equal a b =
  if eq a b then
    Three.Yes
  else match a, b with                                 (* constants from within a theory are *)
    | App((Arith _ as c), []), App((Arith _ as d), []) (* assumed to interpreted differently *)
	when not(Sym.eq c d) -> Three.No
    | App((Bv _ as c), []), App((Bv _ as d), [])
	when not(Sym.eq c d) -> Three.No
    | _ ->
	Three.X


(** Mapping over list of terms. Avoids unnecessary consing. *)
let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	if eq a' a && l1 == l1' then l else a' :: l1'


(** Association lists for terms. *)   
let rec assq a = function
  | [] -> raise Not_found
  | (x,y) :: xl -> if eq a x then y else assq a xl


(** Iteration over terms. *)
let rec fold f a acc =
  match a with
    | Var _ -> f a acc
    | App(_, l) -> f a (List.fold_right (fold f) l acc)

let rec iter f a  =
  f a; 
  if is_app a then
    List.iter (iter f) (args_of a)

let rec for_all p a  =
  p a && 
  match a with
    | Var _ -> true
    | App(_, l) -> List.for_all (for_all p) l


let rec subterm a b  =
  eq a b ||
  match b with
    | Var _ -> false
    | App(_, l) -> List.exists (subterm a) (args_of b)

let occurs x b = subterm x b


(** {6 Pretty-Printing} *)

let pretty = ref true  (* Infix/Mixfix output when [pretty] is true. *)

let rec pp fmt a =
  let str = Pretty.string fmt in
  let term = pp fmt in
  let args =  Pretty.tuple pp fmt in
  let app f l = Sym.pp fmt f; Pretty.tuple pp fmt l in
  let infixl x = Pretty.infixl pp x fmt in
  match a with
    | Var(x) -> Var.pp fmt x
    | App(f, l) when not(!pretty) -> app f l
    | App(f, l) ->
	(match f, l with
	   | Arith(Num q), [] -> 
	       Mpa.Q.pp fmt q
	   | Arith(Add), _ -> 
	       infixl " + " l
	   | Arith(Multq(q)) , [x] -> 
	       Pretty.infix Mpa.Q.pp "*" pp fmt (q, x)  
	   | Pair(Car), [App(Coproduct(OutR), [x])] ->
	       str "hd"; str "("; term x; str ")"
	   | Pair(Cdr), [App(Coproduct(OutR), [x])] ->
	       str "tl"; str "("; term x; str ")"
	   | Pp(Mult), [] ->
	       str "1"
	   | Pp(Mult), xl ->
	       infixl "*" xl
	   | Pp(Expt _), [x] ->
	       term x; Sym.pp fmt f
	   | Bv(Const(b)), [] -> 
	       str ("0b" ^ Bitv.to_string b)
	   | Bv(Conc _), l -> 
	       infixl " ++ " l
	   | Bv(Sub(_,i,j)), [x] ->
	       term x; Format.fprintf fmt "[%d:%d]" i j
	   | Coproduct(InL), [App(Pair(Cons), [x; xl])] ->
	       Pretty.infix pp "::" pp fmt (x, xl)
	   | Coproduct(InR), [App(Pair(Cons), [])] ->
	       str "[]"
	   | Arrays(Update), [x;y;z] ->
	       term x; str "["; term y; str " := "; term z; str "]"
	   | Arrays(Select), [x; y] ->
	       term x; str "["; term y; str "]"
	   | _ -> 
	       app f l)

let to_string = 
  Pretty.to_string pp


(** Pretty-printing of equalities/disequalities/constraints. *)

let pp_equal fmt (x,y) = 
  Pretty.infix pp "=" pp fmt (x,y)

let pp_diseq fmt (x,y) = 
  Pretty.infix pp "<>" pp fmt (x,y)


(** {6 Sets and maps of terms.} *)

type trm = t  (* avoid type-check error below *)

module Set = Set.Make(
  struct
    type t = trm
    let compare = cmp
  end)

module Map = Map.Make(
  struct
    type t = trm
    let compare = cmp
  end)

(** Apply a term map by instantiating [x] with [b] in [a] 
  if [m] contains the binding [x |-> b]. *)

let apply m =
  let rec app a =
    try
      Map.find a m
    with
	Not_found -> 
	  (match a with
	     | Var _ -> a
	     | App(f, al) ->
		 let al' = mapl app al in
		   if al == al' then a else App(f, al'))
  in
    app


(** Set of variables. *)
let rec vars_of a = 
  match a with
    | Var _ -> 
	Set.singleton a
    | App(_, al) ->
	List.fold_left 
	  (fun acc b ->
	     Set.union (vars_of b) acc)
	  Set.empty
	al

