
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
open Term
open Hashcons
(*i*)


type t = Term.t Term.Map.t
 
let empty = Term.Map.empty

let add (x,t) rho =
  if x === t then
    rho
  else Term.Map.add x t rho

let mem s x =
  Term.Map.mem x s

let apply s a =
  Term.Map.find a s
    
let find s a =
  try
    Term.Map.find a s
  with
      Not_found -> a

let of_list l =
  List.fold_right add l empty

let to_list l =
  let cons x a acc =
    (x,a) :: acc
  in
  Map.fold cons l [] 

let iter f =
  Map.iter (fun x y -> f(x,y))

let fold f =
  let fu x y = f(x,y) in
  Map.fold fu
    
let dom s =
  let add x _ acc =
    Term.Set.add x acc
  in
  Map.fold add s Term.Set.empty
    
let pp fmt rho =
  let pp_binding (x,a) =
    Format.fprintf fmt "@[";
    Pretty.term fmt x;
    Format.fprintf fmt "@ |->@ ";
    Pretty.term fmt a;
    Format.fprintf fmt "@]";
  in
  let rec pp_bindings = function
    | [] -> ()
    | [b] -> pp_binding b
    | b :: l -> pp_binding b; Format.fprintf fmt ",@."; pp_bindings l
  in
  Format.fprintf fmt "@[";
  pp_bindings (to_list rho);
  Format.fprintf fmt "@]"
 

(*s {\bf Norm.} This operation, written \replace{S}{a}, applies [S]
    under the interpreted symbols of [a]. It is defined by
    \begin{displaymath}\begin{array}{rcl}
    \replace{S}{f(a_1,\dots,a_n)} & = & 
        f(\replace{S}{a_1},\dots,\replace{S}{a_n}), 
        \mbox{ if $f$ is interpreted} \\
    \replace{S}{a} & = & S(a), \mbox{ otherwise}
    \end{array}\end{displaymath}
    We use the [map] function over terms to apply [replace] recursively under
    interpreted terms.
*)

let norm s a =
  let rec repl t =
    (match t.node with
      | Var _ ->
	  find s t
      | App({node=Set(Cnstrnt(c))},[x]) ->
	  hom1 t (Cnstrnt.app c) repl x
      | App({node=Update(x,y,z)} as f,l) ->
	  let f' = hom3 f App.update repl (x,y,z) in
	  homl t (App.app f') repl l
      | App _ ->
	  find s t
      | Update(x,y,z) ->
	  hom3 t App.update repl (x,y,z)
      | Cond(x,y,z) ->
	  hom3 t Bool.cond repl (x,y,z)
      | Set s ->
	  (match s with
	     | Full _ | Empty _ | Cnstrnt _ ->
		 t
	     | Finite s ->
		 homs t Sets.finite repl s 
	     | SetIte(tg,x,y,z) ->
		 hom3 t (Sets.ite tg) repl (x,y,z))
      | Bool b ->
	  (match b with
	     | True | False ->
		 t
	     | Equal(x,y) ->
		 hom2 t Bool.equal repl (x,y)
	     | Ite(x,y,z) ->
		 hom3 t Bool.ite repl (x,y,z)
	     | _ ->
		 assert false)
      | Arith a ->
	  (match a with
	     | Num _ ->
		 t
	     | Add l ->
		 homl t Arith.add repl l
	     | Multq(q,x) ->
		 hom1 t (Arith.multq q) repl x
	     | Mult l ->
		 homl t Arith.mult repl l
	     | Div(x,y) ->
		 hom2 t Arith.div2 repl (x,y))
      | Bv b ->
	  (match b with
	     | Const _ ->
		 t
	     | Extr((n,x),i,j) ->
		 hom1 t (Bv.sub n i j) repl x
	     | BvToNat x ->
		 hom1 t (Bv.bv2nat 42) repl x
	     | Conc l ->
		 Bv.conc (List.map (fun (n,t) -> (n, repl t)) l)
	     | BvIte((n,x),(_,y),(_,z)) ->
		 hom3 t (Bv.ite n) repl (x,y,z))
      | Tuple tp ->
	  (match tp with
	     | Proj(i,j,x) ->
		 hom1 t (Tuple.proj i j) repl x
	     | Tup l ->
		 homl t Tuple.tuple repl l))
  in
  repl a







