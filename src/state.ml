
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
open Subst
(*i*)

type t = {
  mutable ctxt: Term.eqn list;
  mutable find: Subst.t;
  mutable inv: (Term.terms * Interval.t) Term.Map.t;    (* extension and constraint for each find. *)
  mutable use: Term.terms Term.Map.t;
  mutable uninterp: Term.terms Funsym.Map.t
}  

let empty = {
  ctxt = [];
  find = Subst.empty;
  inv = Term.Map.empty;
  use = Term.Map.empty;
  uninterp = Funsym.Map.empty
}

let copy s = {
  ctxt = s.ctxt;
  find = s.find;
  inv = s.inv;
  use = s.use;
  uninterp = s.uninterp
}

let to_subst s = s.find

(*s Accessors. *)

let ctxt_of s = s.ctxt
let find_of s = s.find 
let ext_of s = Term.Map.fold (fun x (l,_) -> Term.Map.add x l) s.inv Term.Map.empty
let cnstrnt_of s = Term.Map.fold (fun x (_,c) -> Term.Map.add x c) s.inv Term.Map.empty
let use_of s = s.use
let uninterp_of s = s.uninterp

		      
(*s Pretty-printing *)

let pp_ctxt fmt s =
  Pretty.list Pretty.eqn fmt s.ctxt
    
let pp_find fmt s =
  Subst.pp fmt s.find
		      
let pp_ext fmt s =
  Pretty.tmap Pretty.tset fmt (ext_of s)

		     
let pp_use fmt s =
  Pretty.tmap Pretty.tset fmt s.use
		 
let pp_uninterp fmt s =
  Funsym.Map.iter (fun f ts ->
		 Funsym.pp fmt f;
		 Format.fprintf fmt " |-> ";
		 Pretty.tset fmt ts;
		 Format.fprintf fmt " \n")
    s.uninterp

 
(*s Use structure *)

let mem s =
  Subst.mem s.find

let apply s a =
  Subst.apply s.find a
    
let find s a =
  Subst.find s.find a

let inv s a =
  assert (find s a === a);
  try
    Term.Map.find a s.inv
  with
      Not_found -> (Term.Set.empty,Interval.top)
    
let ext s a =
  try
    let (el,_) = Term.Map.find a s.inv in
    el
  with
      Not_found -> Term.Set.empty

let cnstrnt s a =
  let from_ctxt x =
    let (_,c) = Term.Map.find (find s x) s.inv in c
  in
  Cnstrnt.cnstrnt from_ctxt a  
      
let use s a =
  try
    Term.Map.find a s.use
  with
      Not_found -> Term.Set.empty

let uninterp s f =
  try
    Funsym.Map.find f s.uninterp
  with
      Not_found -> Term.Set.empty

let dom s =
  Subst.fold (fun (x,_) acc -> x :: acc) s.find []

    
(*s Updating structures *)

let add_ctxt s e =
  s.ctxt <- e :: s.ctxt
 
let add_cnstrnt s c a =
 (* assert(a === find s a); *)
  let ext =
    try
      fst(Term.Map.find a s.inv)
    with
	Not_found -> Term.Set.empty
  in
  s.inv <- Term.Map.add a (ext,c) s.inv

let rec add_eqn s c (a,b) =
  update_old s c a;
  let extb = ext s b in
  let extb' = Term.Set.add a extb in
  if not(extb == extb') then
    s.inv <- Term.Map.add b (extb', c) s.inv;
  s.find <- Subst.add (a,b) s.find;
  add_use s b;
  add_funsym s a;
  add_funsyms s b        (* Add uninterpreted subterms of b which occur interpreted in b. *)

and update_old s c a =
  try
    let a' = Subst.apply s.find a in
    let ea' = ext s a' in
    let ea'' = Term.Set.remove a ea' in
    if Term.Set.is_empty ea'' then                     (* Not a find anymore. Delete from db. *)
      begin
	del_use s a';
	s.inv <- Term.Map.remove a' s.inv;
	del_funsym s a
      end
    else                                              (* Update extension and constraint. *)
      begin
	s.inv <- Term.Map.add a' (ea'',c) s.inv
      end
  with
      Not_found -> ()

and add_use s a =
  use_iter_uninterpreted
    (fun t ->
       let uset = use s t in
       let uset' = Term.Set.add a uset in
       if not(uset == uset') then
	 s.use <- Term.Map.add t uset' s.use)
    a

and del_use s a' =
  use_iter_uninterpreted
    (fun t ->
       let uset = use s t in
       let uset' = Term.Set.remove a' uset in
       if Term.Set.is_empty uset' then
	 s.use <- Term.Map.remove t s.use
       else if not(uset == uset') then
	 s.use <- Term.Map.add t uset' s.use)
    a'

and add_funsym s a =
  match Funsym.of_term a with
    | Some(f) ->
	let unf = uninterp s f in
	let unf' = Term.Set.add a unf in
	if not(unf == unf') then
	  s.uninterp <- Funsym.Map.add f unf' s.uninterp
    | None -> ()

and add_funsyms s b =
  funsym_iter_uninterpreted
    (fun f t ->
       let unf = uninterp s f in
       let unf' = Term.Set.add t unf in
       if not(unf == unf') then
	 s.uninterp <- Funsym.Map.add f unf' s.uninterp)
    b

and del_funsym s a =
  match Funsym.of_term a with
    | Some(f) ->
	let unf = uninterp s f in
	let unf' = Term.Set.remove a unf in
	if not(unf == unf') then
	  s.uninterp <- Funsym.Map.add f unf' s.uninterp
    | None -> ()

and use_iter_uninterpreted f a =
   let rec loop t =
    match t.node with
      | Var _ | App _ | Set(Cnstrnt _) ->
	  f t
      | Update(x,y,z) -> loop x; loop y; loop z
      | Cond(x,y,z) -> loop x; loop y; loop z
      | Arith(Mult l) -> List.iter loop l
      | Arith(Div(x,y))  -> loop x; loop y
      | Bv(BvToNat x) -> loop x;
      | Bool(Equal(x,y)) -> loop x; loop y
      | Set s ->
	  (match s with
	     | SetIte(_,x,y,z) -> loop x; loop y; loop z
	     | Finite(s) -> Set.iter loop s
	     | _ -> ())
      | Bool b ->
	  (match b with
	     | Ite(x,y,z) -> loop x; loop y; loop z
	     | _ -> ())
      | Arith a ->
	  (match a with
	     | Add l -> List.iter loop l
	     | Multq(_,x) -> loop x
             | Num _ -> ()
	     | _ -> assert false)
      | Bv b ->
	  (match b with
	     | Const _ -> ()
	     | Extr((_,x),_,_) -> loop x
	     | Conc l -> List.iter (fun (_,x) -> loop x) l
	     | BvIte((n,x),(_,y),(_,z)) -> loop x; loop y; loop z
	     | _ -> assert false)
      | Tuple tp ->
	  (match tp with
	     | Proj(_,_,x) -> loop x
	     | Tup l -> List.iter loop l)
   in   loop a

and funsym_iter_uninterpreted f a =
   let rec loop t =
    match t.node with
      | Var _ -> ()
      | App(x,l) ->
	  f (Funsym.uninterp x) t;
	  List.iter loop l;
      | Update(x,y,z) ->
	  f (Funsym.update()) t;
	  loop x; loop y; loop z;
      | Cond(x,y,z) ->
	  f (Funsym.update()) t;
	  loop x; loop y; loop z;  
      | Arith(Mult(l)) ->
	  f (Funsym.mult()) t;
	  List.iter loop l
      | Arith(Div(x,y)) ->
	  f (Funsym.div()) t;
	  loop x; loop y
      | Bool(Equal(x,y)) ->
	  f (Funsym.equal()) t;
	  loop x; loop y
      | Set s ->
	  (match s with
	     | SetIte(_,x,y,z) -> loop x; loop y; loop z
	     | Finite(s) -> failwith "funsym:to do"
	     | _ -> ())
      | Bool b ->
	  (match b with
	     | Ite(x,y,z) -> loop x; loop y; loop z
	     | _ -> ())
      | Arith a ->
	  (match a with
	     | Add l -> List.iter loop l
	     | Multq(_,x) -> loop x
             | Num _ -> ()
	     | _ -> assert false)
      | Bv b ->
	  (match b with
	     | Const _ -> ()
	     | Extr((_,x),_,_) -> loop x
	     | Conc l -> List.iter (fun (_,x) -> loop x) l
	     | BvIte((n,x),(_,y),(_,z)) -> loop x; loop y; loop z
	     | BvToNat _ ->
	         failwith "funsym:to do")
      | Tuple tp ->
	  (match tp with
	     | Proj(_,_,x) -> loop x
	     | Tup l -> List.iter loop l)
   in   loop a  





