
(*i*)
open Hashcons
open Term
(*i*)

 
(*s Fold operator over terms. *)

let fold f t v0 =
  let rec fold_tuple x acc =
    match x with
      | Tup l -> List.fold_right fold_term l acc
      | Proj (_,_,x) -> fold_term x acc
  and fold_prop x acc =
    match x with
      | True | False -> acc
      | Ite (p,q,r)  -> fold_term p (fold_term q (fold_term r acc))
      | Forall _ | Exists _ -> assert false
  and fold_arith x acc =
    match x with
      | Num _ -> acc
      | Times l -> fold_term_list l acc
      | Plus l -> fold_term_list l acc
  and fold_set x acc =
    match x with
      | Empty _ | Full _ -> acc
      | SetIte (_,a,b,c) -> fold_term a (fold_term b (fold_term c acc))
  and fold_bv x acc =
    match x with
      | Const _ -> acc
      | Extr (b,_,_) -> fold_fixed b acc
      | Conc l -> fold_fixed_list l acc
      | BvIte (b1,b2,b3) -> fold_fixed b1 (fold_fixed b2 (fold_fixed b3 acc))
  and fold_fixed (_,x) acc =
    fold_term x acc
  and fold_fixed_list l acc =
    List.fold_right fold_fixed l acc   
  and fold_term_list l acc =
    List.fold_right fold_term l acc
  and fold_term t acc =
    match t.node with
      | Var _     -> f t acc
      | App (x,l) -> f t (fold_term x (fold_term_list l acc))
      | Update(x,y,z) -> f t (fold_term x (fold_term y (fold_term z acc)))
      | Equal (x,y)   -> f t (fold_term x (fold_term y acc))
      | Cnstrnt(_,x)   -> f t (fold_term x acc)
      | Tuple x   -> f t (fold_tuple x acc)
      | Bool x    -> f t (fold_prop x acc)
      | Set x     -> f t (fold_set x acc)
      | Bv l      -> f t (fold_bv l acc)
      | Arith x -> f t (fold_arith x acc)
  in
  fold_term t v0

let iter f t = fold (fun s _ -> f s) t ()

(*s Size of a term. *)

let size t = fold (fun _ n -> succ n) t 1

	       

(*s [is_pure a] tests if [a] contains neither a variable n
    nor an application of an uninterpreted function symbol. *)

exception Impure

let is_pure =
  Term.cache 1007
    (fun t ->
       try
         iter (fun x -> if is_uninterpreted x then raise Impure) t;
         true
       with
         Impure -> false)

(*s Mapping over terms *)
 
let rec map f t =
  match t.node with
    | Var _ ->
	f t
    | App (x,yl) ->
	Arrays.app (f x) (List.map (map f) yl)
    | Update (x,y,z) ->
	Arrays.update (map f x) (map f y) (map f z)
    | Tuple x ->
	(match x with
	   | Tup l ->
	       Tuple.tuple (List.map (map f) l)
	   | Proj (i,n,x) ->
	       Tuple.proj i n (map f x))
    | Bool x ->
	(match x with
	   | True | False ->
	       f t
	   | Ite(x,y,z) ->
	       Bool.ite (map f x) (map f y) (map f z)
	   | _ -> assert false)
    | Set x ->
	(match x with
	   | Empty _ | Full _ ->
	       f t
	   | SetIte(tg,x,y,z) ->
	       Sets.ite tg (map f x) (map f y) (map f z))
    | Bv x ->
	(match x with
	   | Const _ ->
	       f t
	   | Extr((n,x),i,j) ->
	       Bv.sub n i j (map f x)
	   | Conc l ->
	       Bv.conc (List.map (fun (n,t) -> (n, map f t)) l)
	   | BvIte((n,x),(_,y),(_,z)) ->
	       Bv.ite n (map f x) (map f y) (map f z))
    | Arith x ->
	(match x with
	   | Num _ -> f t
	   | Times l ->
	       Arith.mult (List.map (map f) l)
	   | Plus l ->
	       Arith.add (List.map (map f) l))
    | Equal (x,y) ->
	Atom.eq (map f x, map f y)
    | Cnstrnt(c,x) ->
	Atom.cnstrnt (c, map f x)

(*s [replace t x s] replaces occurrences of term [x] in term [t] with term [s] *)

let replace t x s =
  let rec repl t =
    if t == x then s
    else match t.node with
      | Var _ -> t
      | App (x,yl) ->
	  Arrays.app (repl x) (List.map repl yl)
      | Update (x,y,z) ->
	  Arrays.update (repl x) (repl y) (repl z)
      | Tuple x ->
	  (match x with
	     | Tup xl ->
		 Tuple.tuple (List.map repl xl)
	     | Proj (i,n,x) ->
		 Tuple.proj i n (repl x))
      | Bool x ->
	  (match x with
	     | True | False -> t
	     | Ite(x,y,z) ->
		 Bool.ite (repl x) (repl y) (repl z)
	     | _ -> assert false)
      | Set x ->
	  (match x with
	     | Empty _ | Full _ -> t
	     | SetIte(tg,x,y,z) ->
		 Sets.ite tg (repl x) (repl y) (repl z))
      | Bv x ->
	  (match x with
	     | Const _ -> t
	     | Extr((n,x),i,j) ->
		 Bv.sub n i j (repl x)
	     | Conc l ->
		 Bv.conc (List.map (fun (n,t) -> (n, repl t)) l)
	     | BvIte((n,x),(_,y),(_,z)) ->
		 Bv.ite n (repl x) (repl y) (repl z))
      | Arith x ->
	  (match x with
	     | Num _ -> t
	     | Times xl ->
		 Arith.mult (List.map repl xl)
	     | Plus xl ->
		 Arith.add (List.map repl xl)) 
      | Equal (x,y) ->
	  Atom.eq (repl x, repl y)    
      | Cnstrnt(c, x) ->
	  Atom.cnstrnt (c, repl x)
  in
  repl t
	


