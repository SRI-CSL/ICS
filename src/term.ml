
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

(*s Terms. *)

type t =
  | Var of Var.t
  | App of Sym.t * t list
 
let rec eq a b = 
  match a, b with
    | Var(x), Var(y) -> 
	Var.eq x y
    | App(f,l), App(g,m) -> 
	Sym.eq f g &&
	(try List.for_all2 eq l m with Invalid_argument _ -> false)
    | _ ->
	false

(*s Constructors. *)

let mk_var x = Var(Var.mk_var x)

let mk_const f = App(f,[])
let mk_app f l = App(f,l)

let mk_fresh_var x k = Var(Var.mk_fresh x k)

let mk_fresh_param x k = App(Sym.mk_fresh x k, [])


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
      | x::xl, y::yl -> 
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
  | App(f,[]) -> Sym.is_interp f
  | _ -> false
   
let is_interp = function
  | App(f, _) -> Sym.is_interp f
  | _ -> false

let is_uninterpreted = function
  | App(f, _) -> not(Sym.is_interp f)
  | _ -> false

(*s Test is arguments are known to be disequal. *)

let is_diseq a b =
  match a, b with
    | App(f,[]), App(g,[]) ->
	not(Sym.eq f g) &&
	Sym.is_interpreted_const f && 
	Sym.is_interpreted_const g
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
  (not(is_var b) ||
   List.exists (subterm a) (args_of b))


(*s Printer. *)

let rec pp fmt a =
  match a with
    | Var(x) -> 
	Var.pp fmt x
    | App(f,l) ->
	(match Sym.destruct f, l with
	  | Sym.Interp(Sym.Arith(Sym.Num q)), [] -> 
	      Mpa.Q.pp fmt q
	  | Sym.Interp(Sym.Arith(Sym.Add)), _ -> 
	      Pretty.infixl pp " + " fmt l
	  | Sym.Interp(Sym.Arith(Sym.Mult)), _ -> 
	      Pretty.infixl pp " * " fmt l
	  | Sym.Interp(Sym.Arith(Sym.Expt(n))), [x] 
	      when is_var x || not(Sym.is_arith (sym_of x)) ->
		Pretty.infix pp "^" Pretty.number fmt (x,n)
	  | Sym.Interp(Sym.Tuple(Sym.Proj(0,2))), [x] -> 
	      Pretty.string fmt "car("; 
	      pp fmt x;
	      Pretty.string fmt ")"
	  | Sym.Interp(Sym.Tuple(Sym.Proj(1,2))), [x] -> 
	      Pretty.string fmt "cdr("; 
	      pp fmt x;
	      Pretty.string fmt ")"
	  | Sym.Interp(Sym.Tuple(Sym.Product)), [x;y] -> 
	      Pretty.string fmt "cons("; 
	      pp fmt x; 
	      Pretty.string fmt ", ";
	      pp fmt y;
	      Pretty.string fmt ")"
	  | Sym.Interp(Sym.Boolean(Sym.True)), [] -> 
	      Pretty.string fmt "true"
	  | Sym.Interp(Sym.Boolean(Sym.False)), [] -> 
	      Pretty.string fmt "false"
	  | Sym.Interp(Sym.Bv(Sym.Const(b))), [] -> 
	      Format.fprintf fmt "0b%s" (Bitv.to_string b)
	  | Sym.Interp(Sym.Bv(Sym.Conc _)), l -> 
	      Pretty.infixl pp " ++ " fmt l
	  | Sym.Interp(Sym.Bv(Sym.Sub(_,i,j))), [x] -> 
	      (pp fmt x; Format.fprintf fmt "[%d:%d]" i j)
	  | _, [x;y;z] when Sym.eq f Sym.mk_update ->
	      pp fmt x; 
	      Pretty.string fmt "[";
	      pp fmt y; 
	      Pretty.string fmt " := ";
	      pp fmt z;
	      Pretty.string fmt "]"
	  | _, [x;y] when Sym.eq f Sym.mk_select ->
	      pp fmt x;
	      Pretty.string fmt "[";
	      pp fmt y;
	      Pretty.string fmt "]"
	  | _, [x;y] when Sym.eq f Sym.mk_div ->
	      pp fmt x; Pretty.string fmt " / "; pp fmt y
	  | _ ->
	      Sym.pp fmt f; 
	      Tools.ppl ("(", ", ", ")") pp fmt l)

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
