
(*i*)
open Hashcons
open Term
(*i*)

(*s The following data-structure implements functional sets of equalities, 
    as an abstract datatype [t].
    If [S] is such a functional set of equalities, it is represented as 
    map from terms to terms (field [map]). 
    Another map represents $S^{-1}$, as a map from terms to sets
    of terms (field [pam]). Atomic terms are not considered in the reverse
    map. *)

type t = { 
  mutable find: term Tmap.t;
  mutable use : Tset.t Tmap.t     (* map from terms to set of terms *)
}
 
(*s [empty] is the empty set. [add_id a s] adds the mapping [a=a] in [s] 
    if [a] is not already in $dom(s)$. [add a b s] adds the mapping [a=b]
    in [s], overwriting any previous mapping for [a]. [mem a s] tests
    wether $a\in dom(s)$. *)   

let empty = {
  find = Tmap.empty;
  use = Tmap.empty
}

let copy s = {
  find = s.find;
  use = s.use;
}

let mem s x =
  Tmap.mem x s.find

let apply s =
  let rec loop x =
    let y = Tmap.find x s.find in
    if x == y then x else loop y
  in
  loop
    
let find s =
  let rec loop x =
    try
      let y = Tmap.find x s.find  in
      if x == y then x else loop y
    with
	Not_found -> x
  in
  loop

(*s Use structure *)
    
let rec use s x =
  try Tmap.find x s.use with Not_found -> Tset.empty

let adduse s x y =
  s.use <- Tmap.add x (Tset.add y (use s x)) s.use

let subterm_close s t =
  let rec iter t =
    if not(Tmap.mem t s.find) && not(is_const t) then
      begin
        s.find <- Tmap.add t t s.find;
      match t.node with
      | Var _ -> ()
      | App(u,vl) ->
	  iter u; adduse s u t;
	  List.iter (fun v -> iter v; adduse s v t) vl
      | Atom(Integer(u)) ->
	  iter u; adduse s u t
      | Update(u,v,w) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t;
	  iter w; adduse s w t
      | Tuple y ->
	  (match y with
	     | Tup tl -> List.iter (fun z -> iter z; adduse s z t) tl
	     | Proj(_,_,z) -> iter z; adduse s z t)
      | Arith a ->
	  (match a with
	     | Num _ -> ()
	     | Plus l -> List.iter (fun z -> iter z; adduse s z t) l
	     | Times l -> List.iter (fun z -> iter z; adduse s z t) l)
      | Bool b ->
	  (match b with
	     | True | False -> ()
	     | Ite (u,v,w) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t
	     | _ -> assert false)
      | Bv b ->
	  (match b with
	     | Const _ -> ()
	     | Extr ((_,u),_,_) ->
		 iter u; adduse s u t
	     | Conc l ->
		 List.iter (fun (_,u) -> iter u; adduse s u t) l
	     | BvIte((_,u), (_,v), (_,w)) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t)
      | Set a ->
	  (match a with
	     | Full _ | Empty _ -> ()
	     | SetIte (_,u,v,w) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t)
      | Atom(Equal(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t
      | Atom(Le(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t
      | Atom(Lt(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t    
      end
  in
  iter t

let subterm_close1 s t =
  let rec iter t =
    if not(Tmap.mem t s.find) && not(is_const t) then
      begin
      s.find <- Tmap.add t t s.find;
      if is_uninterpreted t then ()
      else match t.node with
      | Update(u,v,w) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t;
	  iter w; adduse s w t
      | Tuple y ->
	  (match y with
	     | Tup tl -> List.iter (fun z -> iter z; adduse s z t) tl
	     | Proj(_,_,z) -> iter z; adduse s z t)
      | Arith a ->
	  (match a with
	     | Num _ -> ()
	     | Plus l -> List.iter (fun z -> iter z; adduse s z t) l
	     | Times l -> List.iter (fun z -> iter z; adduse s z t) l)
      | Bool b ->
	  (match b with
	     | True | False -> ()
	     | Ite (u,v,w) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t
	     | _ -> assert false)
      | Bv b ->
	  (match b with
	     | Const _ -> ()
	     | Extr ((_,u),_,_) ->
		 iter u; adduse s u t
	     | Conc l ->
		 List.iter (fun (_,u) -> iter u; adduse s u t) l
	     | BvIte((_,u), (_,v), (_,w)) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t)
      | Set a ->
	  (match a with
	     | Full _ | Empty _ -> ()
	     | SetIte (_,u,v,w) ->
		 iter u; adduse s u t;
		 iter v; adduse s v t;
		 iter w; adduse s w t)
      | Atom(Equal(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t
      | Atom(Le(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t
      | Atom(Lt(u,v)) ->
	  iter u; adduse s u t;
	  iter v; adduse s v t    
      | _ -> assert false
      end
  in
  iter t

let update s x y =
  assert (not (x == y)); (* && not(is_const x)); *)
  let y' = find s x in
  if not(y == y') then
    begin
      s.find <- Tmap.add x y s.find;     
      s.use <- Tmap.add y (Tset.union (use s x) (use s y)) s.use
    end
  
(*s Pretty-printing *)

let pp_find s = Pretty.tmap Pretty.term s.find
let pp_use s = Pretty.tmap Pretty.tset s.use
let pp_universe s = Pretty.tmap Pretty.term s.find




