
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*i*)
open Hashcons
open Mpa
open Format
open Term
(*i*)

type 'a  printer = Format.formatter -> 'a -> unit

let print_connectives = ref false

let rec list f fmt = function
    | [] -> ()
    | [x] -> f fmt x
    | x :: l -> f fmt x; Format.fprintf fmt ", " ; list f fmt l
	
let rec list_sep sep f = function
    | [] -> ()
    | [x] -> f x
    | x :: l -> f x; sep (); list_sep sep f l

	

		 (*s Pretty-printing constraints. *)
	
let rec pp_cnstrnt prec fmt c =
  match c with
    | Top -> 
	Format.fprintf fmt "top"
    | BooleanCnstrnt -> 
	Format.fprintf fmt "bool"
    | ArithCnstrnt x -> 
	Interval.pp fmt x
    | TupleCnstrnt ->
	Format.fprintf fmt "tuple"
    | Bot ->
	Format.fprintf fmt "bot"
   

(*i For the printing function, we take into account the usual
    precedence rules (i.e. * binds tighter than +) to avoid
    printing unnecessary parentheses. To this end, we maintain
    the current operator precedence and print parentheses
    around an operator only if its precedence is less than
    the current precedence.

    Operator precedences:
      0  '|', '#', '=>'
      1  '&' '<=>'
      2  '=' '<>' '<' '>' '<=' '>='
      3  'union'
      4  'inter'
      5  'diff' 'symdiff'
      6  '-' '+'
      7  '*' '/'
      8 '~' 'compl'
  i*)

let pp fmt t =
  let pr = fprintf fmt in
  let lpar prec op_prec = if prec > op_prec then pr "(" in 
  let rpar prec op_prec = if prec > op_prec then pr ")" in
  let rec pp_term prec t =
    match t.node with
      | Var(x,_) -> 
	  fprintf fmt "%s" x
      | App(f,l) ->  
	  pp_app prec f l

  and pp_app prec f l =
    match f.node with
      | Uninterp(x,_,_) ->
	  pr "@["; pp_term prec x; pr "("; pp_terml l; pr ")@]"
      | Interp(x) ->
	  pp_interp prec x l
      | Pred(x) ->
	  pp_pred prec x l
      | Builtin(x) ->
	  pp_builtin prec x l

  and pp_pred prec p l =
    match p, l with
      | Equal, [x;y] ->
	  pp_binary_pred prec "=" (x,y)
      | Cnstrnt(c), [x] ->
	  pp_cnstrnt_pred prec c "in" x
      | _ ->
	  assert false

  and pp_binary_pred prec str (x,y) =
    Format.fprintf fmt "";
    pp_term prec x;
    Format.fprintf fmt " %s " str;
    pp_term prec y;
    Format.fprintf fmt ""

  and pp_cnstrnt_pred prec c str x =
    Format.fprintf fmt "@[";
    pp_term prec x;
    Format.fprintf fmt " %s " str;
    pp_cnstrnt prec fmt c;
    Format.fprintf fmt "@]"

  and pp_builtin prec f l = 
    match f,l with
      | Update, [x;y;z] -> 
	  pr "@["; pp_term prec x; pr "["; pp_term prec y; pr " := "; pp_term prec z; pr "]@]" 
      | _ ->
	  assert false

  and pp_interp prec f l =
    match f with
      | Arith(op) ->
	  pp_arith prec op l
      | Tuple(op) ->
	  pp_tuple prec op l
      | Bool(op) -> 
	  pp_bool prec op l

  and pp_arith prec op l =
    match op, l with
      | Num(q),[] ->
	  Mpa.Q.pp fmt q
      | Multq(q), [x] ->
	  fprintf fmt "@[";
          Mpa.Q.pp fmt q;
	  fprintf fmt "*";
	  pp_term 2 x;
	  fprintf fmt "@]"
      | Add, _ :: _ :: _ ->
	  list_sep (fun () -> pr " + ") (pp_term prec) l  
      | Mult, _::_::_ ->
	  pr "@[("; list_sep (fun () -> pr " * ") (pp_term prec) l; pr ")@]"
      | Div, [x;y] ->
	  pr "@["; pp_term prec x; pr " / "; pp_term prec y; pr "@]" 
      | _ ->
	  assert false

  and pp_tuple prec op l =
    match op, l with
      | Product, _::_::_ ->
	  pr "@[("; pp_terml l; pr ")@]"
      | Proj(i,n),[x] ->
	  fprintf fmt "@[proj[%d,%d](" i n; pp_term 0 x; pr ")@]"
      | _ ->
	  assert false

  and pp_bool prec op l =
    match op, l with
      | True, [] ->
	  fprintf fmt "true"
      | False, [] ->
	  fprintf fmt "false"
      | Ite, [x;y;z] ->
	  pp_ite "if" (pp_term 0) (x,y,z)
      | _ ->
	  assert false

  and pp_ite str pp (x,y,z) =
    pr "@["; fprintf fmt "%s " str;
    pp x;
    pr "@ then@ ";
    pp y;
    pr "@ else@ ";
    pp z;
    pr "@ end@]"
			    
  and pp_terml = function
    | [t]  -> pp_term 0 t
    | t::l -> pp_term 0 t; pr ","; pp_terml l
    | []   -> ()
	  
  in
  pp_term 0 t
    
let pp_eqn fmt (a,b) =
  Format.fprintf fmt "@[";
  pp fmt a;
  Format.fprintf fmt " = ";
  pp fmt b;
  Format.fprintf fmt "@]"
    
let pp_diseq fmt (a,b) =
  Format.fprintf fmt "@[";
  pp fmt a;
  Format.fprintf fmt " <> ";
  pp fmt b;
  Format.fprintf fmt "@]"
    
let term = pp
     
let eqn = pp_eqn

let diseq = pp_diseq

let cnstrnt = (pp_cnstrnt 0)
	    
let tset fmt s = 
  let rec loop = function
    | [] -> ()
    | [a] -> pp fmt a
    | a :: l -> pp fmt a; Format.fprintf fmt "@ ,@ "; loop l
  in
  Format.fprintf fmt "@[{"; loop (Term.Set.to_list s); Format.fprintf fmt "}@]"

let tmap p fmt m =
  let pp_assign (x,a) =
    Format.fprintf fmt "@["; pp fmt x; Format.fprintf fmt "@ |->@ "; p fmt a
  in
  let rec pp_assigns = function
    | [] -> ()
    | [a] -> pp_assign a
    | a :: l -> pp_assign a; Format.fprintf fmt "@ ,@ "; pp_assigns l
  in
    Format.fprintf fmt "@[["; pp_assigns (Map.to_list m); Format.fprintf fmt "]@]"
