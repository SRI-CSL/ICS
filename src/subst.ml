
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

let update (x,t) =
  Term.Map.update x t

let remove x rho =
  Term.Map.remove x rho

let is_empty =
  Term.Map.is_empty

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

let fold2 f s e =
  fold 
    (fun (x1,y1) acc1 ->
       (fold (fun (x2,y2) acc2 ->
		if x1 === x2 then acc2 else
		  f (x1,y1) (x2,y2) acc2)
	  s acc1))
    s e

let every f s =
  fold (fun b acc -> f(b) && acc) s true

let map f s =
  Map.fold (fun x y acc  -> add (f(x,y)) acc) s empty
    
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
 




