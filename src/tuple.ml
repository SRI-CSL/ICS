
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
open Term
(*i*)

(*s Smart constructors for tuples and projections. *)

let tuple =
  Bool.nary_lift_ite
    (function
       | [x] -> x
       | [] -> assert false
       | l' -> hc(Tuple(Tup l')))

let rec proj i n a =
  match a.node with
    | Tuple(Tup l) ->
	List.nth l i
    | Bool(Ite(x,y,z)) ->
	Bool.ite x (proj i n y) (proj i n z)
    | _ ->
	hc(Tuple(Proj(i,n,a)))     


(*s Solving tuples. *) 

let add ((a,b) as e) el =
  if a === b then el
  else match b.node with
    | Tuple(Tup l) when List.exists (fun y -> a === y) l ->
	raise (Exc.Inconsistent "Tuple solver")
    | _ -> e :: el

(*s [solve (s, (t0,...,tn)) = \list{(proj s 0, t0),...,(proj s n, tn)}] *)

let tuple_solve s l =
  let n = List.length l in
  let (eqs, _) = 
    List.fold_right
      (fun t (acc, i) -> (add (proj i n s, t) acc, i + 1)) l ([], 0)
  in
  eqs

(*s [solve ((s0,...,sn), (t0,...,tn)) = [(s0,t0),...(sn,tn)] *)  

let tuple_tuple_solve al bl = 
   List.fold_right2 (fun a b acc -> add (a, b) acc) al bl []

(*s [solve (proj i n s, t) = (s, \list{c0,...,t,...cn-1})]
     where [ci] are fresh, [s] at [i]-th position. *)

let proj_solve i n s t =
  let rec args j acc =
    if j = -1 then acc
    else
      let a = if i = j then t else Var.fresh "p" [] in
      args (j - 1) (a :: acc)
  in
  add (s, tuple (args (n - 1) [])) []

let solve ((a,b) as e) =
  try
    let l = match a.node,b.node with
      | Tuple(Tup al), Tuple(Tup bl) -> tuple_tuple_solve al bl
      | Tuple(Proj (i,n,a)), _ -> proj_solve i n a b
      | _, Tuple(Proj(i,n,a)) ->  proj_solve i n a b
      | Tuple(Tup al), _ -> tuple_solve b al
      | _, Tuple(Tup al) -> tuple_solve b al
      | _ -> assert false
    in
    Some l
  with
      Exc.Inconsistent _ -> None
