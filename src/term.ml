
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
open Sym
(*i*)

(*s Terms. *)

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

(*s Constructors. *)

let mk_var x = Var(Var.mk_var x)

let mk_const f = App(f,[])
let mk_app f l = App(f,l)

let mk_fresh_var x k = Var(Var.mk_fresh x k)

let is_fresh_var = function
  | Var(x) -> Var.is_fresh x
  | _ -> false

(*s Recognizers. *)

let is_var = function Var _ -> true | _ -> false
let is_app = function App _ -> true | _ -> false
let is_const = function App(_,[]) -> true | _ -> false


(*s Destructors. *)

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

	
(*s Structural comparison. *)

let rec cmp a b =
  match a, b with
    | Var _, App _ -> -1
    | App _, Var _ -> 1
    | Var(x), Var(y) -> Var.cmp x y
    | App(f,l), App(g,m) ->
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


(*s [cmp] forces function applications to be smaller than constants.  
  Thus, [cmp] makes sure that e.g. [f(x) = c] is added in  this order.
  Otherwise, the term ordering is arbitrary and we use [Term.cmp] for
  ordering all the other cases. *)

let orient (a,b) =
  if a <<< b then (b,a) else (a,b)

let min a b =
  if a <<< b then a else b

let max a b = 
  if a <<< b then b else a


(*s Some recognizers. *)

let is_interp_const = function
  | App((Arith _ | Bv _ | Tuple _), []) -> true
  | _ -> false
   
let is_interp = function
  | App((Arith _ | Bv _ | Tuple _), _) -> true
  | _ -> false

let is_uninterpreted = function
  | App(Uninterp _, _) -> true
  | _ -> false



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

(*s Iteration over terms. *)

let rec fold f a acc =
  if is_var a then
    f a acc
  else 
    f a (List.fold_right (fold f) (args_of a) acc)

let rec iter f a  =
  f a; 
  if is_app a then
    List.iter (iter f) (args_of a)

let rec for_all p a  =
  p a && (is_var a || List.for_all (for_all p) (args_of a))


let rec subterm a b  =
  eq a b ||
  (not(is_var b) &&
   List.exists (subterm a) (args_of b))

let occurs x b = subterm x b


(*s Printer. *)

let pretty = ref true 

let set_pretty_print pp =
  pretty := pp

let rec pp fmt a =
  match a with
    | Var(x) -> 
	Var.pp fmt x
    | App(f, l) ->
	(match f, l with
	   | Arith(op), l -> 
	       pp_arith fmt op l
	   | Tuple(Proj(0,2)), [x] -> 
	       Pretty.string fmt "car("; 
	       pp fmt x;
	       Pretty.string fmt ")"
	   | Tuple(Proj(1,2)), [x] -> 
	       Pretty.string fmt "cdr("; 
	       pp fmt x;
	       Pretty.string fmt ")"
	  | Tuple(Product), [x; y] -> 
	      Pretty.string fmt "cons("; 
	      pp fmt x; 
	      Pretty.string fmt ", ";
	      pp fmt y;
	      Pretty.string fmt ")"
	  | Bv(Const(b)), [] -> 
	      Format.fprintf fmt "0b%s" (Bitv.to_string b)
	  | Bv(Conc _), l -> 
	      Pretty.infixl pp " ++ " fmt l
	  | Bv(Sub(_,i,j)), [x] -> 
	      (pp fmt x; Format.fprintf fmt "[%d:%d]" i j)
	  | Uninterp(f), [x;y;z] when Name.eq f Name.update ->
	      pp fmt x; 
	      Pretty.string fmt "[";
	      pp fmt y; 
	      Pretty.string fmt " := ";
	      pp fmt z;
	      Pretty.string fmt "]"
	  | Uninterp(f), [x; y] when Name.eq f Name.select ->
	      pp fmt x;
	      Pretty.string fmt "[";
	      pp fmt y;
	      Pretty.string fmt "]"
	  | Uninterp(f), [x; y] when Name.eq f Name.div ->
	      pp fmt x; Pretty.string fmt " / "; pp fmt y
	  | Uninterp(f), xl when Name.eq f Name.mult ->
	      Pretty.infixl pp " * " fmt xl
	  | _ -> 
	      Sym.pp fmt f; 
	      Tools.ppl ("(", ", ", ")") pp fmt l)

and pp_arith fmt op l = 
  match op, l with
    | Num q, [] -> 
	Mpa.Q.pp fmt q
    | Add, _ -> 
	Pretty.infixl pp " + " fmt l
    | Multq(q), [x] ->
	Pretty.infix Mpa.Q.pp "*" pp fmt (q, x)
    | _ ->
	assert false
   

let to_string = 
  Pretty.to_string pp


(*s Pretty-printing of equalities/disequalities/constraints. *)

let pp_equal fmt (x,y) = 
  Pretty.infix pp "=" pp fmt (x,y)

let pp_diseq fmt (x,y) = 
  Pretty.infix pp "<>" pp fmt (x,y)

let pp_in fmt (x,c) = 
  Pretty.infix pp "in" Cnstrnt.pp fmt (x,c)


(*s Sets and maps of terms. *)

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
