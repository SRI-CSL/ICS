
(*i*)
open Hashcons
open Mpa
open Bitv
open Format
(*i*)

(*s Constraints. *)


type nonreal =
  | Boolean
  | Predicate
  | Cartesian
  | Bitvector
  | Other

module Nonreals =
  Set.Make(
    struct
      type t = nonreal
      let compare = compare
    end)

type cnstrnt =
  | Top
  | Sub of Nonreals.t * Interval.t
  | Bot

let cnstrnt_eq c1 c2 =
  match c1, c2 with
    | Sub(s1,i1), Sub(s2,i2) ->
	Nonreals.equal s1 s2 && Interval.eq i1 i2
    | _ ->
	c1 = c2

(*s Implementation of terms and functions over terms. \label{implterms}
    The datatypes for terms have been discussed in \fullrefsec{typeterms}. *)

 
type variable = string

type tag = int

type t = tnode hashed

and tnode =
  | Var of variable
  | App of t * t list
  | Update of t * t * t
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Bv of bv

and arith =
  | Num of Q.t
  | Add of t list
  | Multq of Q.t * t
  | Mult of t list
  | Div of t * t
  
and prop =
  | True
  | False
  | Equal of t * t  
  | Ite of t * t * t
  | Forall of variable list * t
  | Exists of variable list * t

and set =
  | Empty of tag
  | Full of tag
  | Finite of terms
  | Cnstrnt of cnstrnt
  | SetIte of tag * t * t * t
      
and tuple =
  | Tup of t list
  | Proj of int * int * t

and bv =
  | Const of Bitv.t
  | Conc of fixed list
  | BvToNat of t
  | Extr of fixed * int * int
  | BvIte of fixed * fixed * fixed
 
and fixed = int * t

and terms = tnode Ptset.t

	      
type term = t
type eqn = t * t
     
(*s {\bf Hash-consing.} In order to save space and to provide an efficient
    comparison of terms, we choose to perform full hash-consing over terms.
    It means that whenever [x] is structurally equal to [y] ([x=y])
    then it will be actually \emph{physically} equal to [y] ([x==y]). 
    Hash-consing is realized using hash-tables, where an already existing 
    and equal object is looked for each time we try to create a new object i.e.
    each time we apply a constructor. 
    We use our own version of hash tables, which provides a [stat] function
    to look at the distribution of values inside a given table. *)
    
(*s Hash-consing of terms. *)

module HashTerm = Hashcons.Make(
  struct 
    type t = tnode
    let equal t1 t2 =
      match t1, t2 with
	| Var(x1), Var(x2) -> 
	    x1 == x2
	| App(s1,l1), App(s2,l2)  ->
            s1 === s2 && (try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false)
	| Update(s1,t1,u1), Update(s2,t2,u2) ->
	    s1 === s2 && t1 === t2 && u1 === u2
	| Arith a1, Arith a2 ->
	    (match a1, a2 with
	       | Num(q1), Num(q2) ->
		   Q.equal q1 q2
	       | Multq(q1,x1), Multq(q2,x2) ->
		   Q.equal q1 q2 && x1 === x2
	       | Add l1, Add l2 ->
		   (try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false)
	       | Mult l1, Mult l2 ->
		   (try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false)
	       | Div(s1,t1), Div(s2,t2) ->
		   s1 === s2 && t1 === t2
	       | _ -> false)
	| Tuple t1, Tuple t2 ->
	    (match t1,t2 with
	       | Tup l1, Tup l2 ->
		   (try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false)
	       | Proj (i1,n1,t1), Proj (i2,n2,t2) ->
		   i1 = i2 && n1 = n2 && t1 === t2
	       | _ -> false)
	| Set t1, Set t2 ->
	    (match (t1,t2) with
	       | SetIte (t1,a1,b1,c1), SetIte (t2,a2,b2,c2) ->
		   a1 === a2 && b1 === b2 && c1 === c2 && t1 = t2		 
	       | Empty t1, Empty t2 ->
		   t1 = t2
	       | Full t1, Full t2 ->
		   t1 = t2
	       | Cnstrnt (c1), Cnstrnt (c2) ->
		   cnstrnt_eq c1 c2  
	       | Finite s1, Finite s2 ->
		   Ptset.equal s1 s2
	       | _ -> false)	    
	| Bool t1, Bool t2  ->
	    (match t1,t2 with
	       | True, True -> true
	       | False, False -> true
	       | Equal (s1, t1), Equal (s2,t2) ->
		   s1 === s2 && t1 === t2
	       | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
		   a1 === a2 && b1 === b2 && c1 === c2
	       | Forall (xl,p), Forall (yl,q) ->
		   p === q && (try List.for_all2 (=) xl yl with Invalid_argument _ -> false)
	       | Exists (xl,p), Exists (yl,q) ->
		   p === q && (try List.for_all2 (=) xl yl with Invalid_argument _ -> false) 
	       | _ -> false) 
	| Bv t1, Bv t2 ->
	    (match t1,t2 with
	       | Const c, Const d ->
		   compare c d = 0
	       | Extr((_,b1),l1,u1), Extr((_,b2),l2,u2)  ->
		   b1 === b2 && l1 = l2 && u1 = u2
	       | BvToNat(s1), BvToNat(s2) ->
		   s1 === s2				  
	       | Conc l1, Conc l2 ->
		   (try
		      List.for_all2 (fun (_,b1) (_,b2) -> b1 === b2) l1 l2
		    with
			Invalid_argument _ -> false)
	       | BvIte((_,u1),(_,v1),(_,w1)), BvIte((_,u2),(_,v2),(_,w2)) ->
		   u1 === u2 && v1 === v2 && w1 === w2
	       | _ -> false)
	| _ -> false
    let hash = function
      | App (s,l) ->
	  s.tag + (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
      | Update(x,y,z) ->
	  (x.tag + y.tag + z.tag) land 0x3FFFFFFF
      | Arith a ->
	  (match a with
	     | Num q -> Q.hash q
	     | Multq(q,x) -> (Q.hash q + x.tag) land 0x3FFFFFFF
	     | Mult l ->
		 (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
	     | Add l ->
		 (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
	     | Div(x,y) -> (x.tag + y.tag) land 0x3FFFFFFF)
      | Bool p ->
	  (match p with
	     | Equal(x,y) -> (x.tag + y.tag) land 0x3FFFFFFF
	     | Ite (a,b,c) -> (31 + a.hkey + b.hkey + c.hkey) land 0x3FFFFFFF
	     | _ -> Hashtbl.hash p)
      | Set s ->
	  (match s with
	     | (Empty _) as t ->  Hashtbl.hash t
	     | (Full _) as t -> Hashtbl.hash t
	     | Finite s -> Hashtbl.hash s
	     | Cnstrnt c -> Hashtbl.hash c
	     | SetIte (_,a,b,c) -> (a.tag + b.tag + c.tag ) land 0x3FFFFFFF)
      | Tuple t ->
	  (match t with
	     | Tup l -> (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
	     | Proj (i,n,t) -> (i + n + t.tag) land 0x3FFFFFFF)
      | Bv l ->
	  (match l with
	     | Const c -> Hashtbl.hash c 
	     | Extr((_,b),i,j) -> (i + j + b.tag) land 0x3FFFFFFF
	     | BvToNat(x) -> x.tag
	     | Conc l -> (List.fold_left (fun h (_,a) -> h+a.tag) 1 l) land 0x3FFFFFFF
	     | BvIte((_,a),(_,b),(_,c)) -> (a.tag + b.tag + c.tag) land 0x3FFFFFFF)
      | x ->
	  Hashtbl.hash x
  end)

let hc : tnode -> t =
  let ht = HashTerm.create 107 in
  Tools.add_at_exit (fun () -> print_string "Terms: "; HashTerm.stat ht);
  Tools.add_at_reset (fun () -> HashTerm.clear ht);
  fun t -> HashTerm.hashcons ht t
 
		       
(*s Caches for functions over terms. *)

module HCache =
  Hashtbl.Make(
    struct 
      type t = term
      let equal = (===) 
      let hash x = x.tag 
    end)
    
let cache n f =
  let ht = HCache.create n in
  fun x ->
    try
      HCache.find ht x
    with
	Not_found -> let y = f x in HCache.add ht x y; y

      
module HCache2 =
  Hashtbl.Make(
    struct 
      type t = term * term
      let equal (x1,y1) (x2,y2) = x1 === x2 && y1 === y2
      let hash (x,y) = (x.tag + y.tag) land 0x3FFFFFFF
  end)
    
let cache2 n f =
  let ht = HCache2.create n in
  fun x ->
    try
      HCache2.find ht x
    with
	Not_found -> let y = f x in HCache2.add ht x y; y

     
module HCachel =
  Hashtbl.Make(
    struct 
      type t = term list
      let equal l1 l2 = try List.for_all2 (===) l1 l2 with Invalid_argument _ -> false
      let hash l = (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
    end)
    
let cachel n f =
  let ht = HCachel.create n in
  fun x ->
    try
      HCachel.find ht x
    with
	Not_found ->
	  let y = f x in HCachel.add ht x y; y     
 
(*s Efficient comparisons using hash-consing. *)

let fast_cmp x y = x.tag - y.tag		       
	     
(*s Structural comparison. Contrary to the above functions, the result
    of the following comparison functions has to be
    session-independent.  Due to the presence of hash-consing tagging
    inside the term data-structures, we cannot use the
    [Pervasives.compare] function directly. We have to write our own
    structural comparison functions, with the help of [Tools.gen_compare]. *)

let rec cmp x y = Cmp.generic
  (function
     | Var s1, Var s2 ->
	 Pervasives.compare s1 s2
     | App (s1,l1), App (s2,l2) -> 
	 Cmp.lexico2 cmp s1 s2 (Cmp.list cmp) l1 l2
     | Update(x1,y1,z1), Update(x2,y2,z2) ->
	 Cmp.lexico3 cmp x1 x2 cmp y1 y2 cmp z1 z2
     | Arith a1, Arith a2 ->
	 cmp_arith a1 a2
     | Tuple t1, Tuple t2 ->
	 cmp_tuple_node t1 t2
     | Set t1, Set t2   ->
	 cmp_set_node t1 t2
     | Bool t1,  Bool t2  ->
	 cmp_prop_node t1 t2
     | Bv b1, Bv b2   ->
	 cmp_bv b1 b2
     | _ -> assert false) x.node y.node

and cmp_arith a1 a2 = Cmp.generic
   (function
      | Num q1, Num q2 ->
	  Q.compare q1 q2
      | Add l1, Add l2 ->
	  Cmp.list cmp l1 l2
      | Multq(q1,x1), Multq(q2,x2) ->
	  Cmp.lexico2 Q.compare q1 q2 cmp x1 x2
      | Mult l1, Mult l2 ->
	  Cmp.list cmp l1 l2
      | Div(x1,y1), Div(x2,y2) ->
	  Cmp.lexico2 cmp x1 x2 cmp y1 y2
      | _ -> assert false) a1 a2
	
and cmp_prop_node x y = Cmp.generic
 (function
    | Equal(x1,y1), Equal(x2,y2) ->
	Cmp.lexico2 cmp x1 x2 cmp y1 y2
    | Ite (a1,b1,c1), Ite (a2,b2,c2) -> 
	Cmp.lexico2
	  cmp a1 a2 
	  (Cmp.lexico2 cmp b1 b2 cmp) c1 c2
    | Forall (xl,p), Forall (yl,q) ->
	Cmp.lexico2
	  (Cmp.list Pervasives.compare) xl yl
	  cmp p q
    | _ -> assert false) x y

and cmp_prop x y = cmp_prop_node x.node y.node

and cmp_set_node x y = Cmp.generic
  (function 
     | SetIte (t1,a1,b1,c1), SetIte (t2,a2,b2,c2) ->
	 Cmp.lexico4 Pervasives.compare t1 t2 cmp a1 a2 cmp b1 b2 cmp c1 c2
     | Finite(s1), Finite(s2) ->
	 Pervasives.compare s1 s2
     | Cnstrnt(c1), Cnstrnt(c2) ->
	 Pervasives.compare c1 c2
     | _ -> assert false) x y

and cmp_set x y = cmp_set_node x.node y.node

and cmp_tuple_node x y = Cmp.generic
  (function
     | Tup l1, Tup l2 ->
	 Cmp.list cmp l1 l2
     | Proj (i1,n1,t1), Proj (i2,n2,t2) ->
	 Cmp.lexico3 Pervasives.compare i1 i2 Pervasives.compare n1 n2 cmp t1 t2
     | _ -> assert false) x y

and cmp_tuple x y = cmp_tuple_node x.node y.node

and cmp_bv_node x y = Cmp.generic
   (function 
      | Const b1, Const b2 ->
	  Pervasives.compare b1 b2
      | BvToNat(x1), BvToNat(x2) ->
	  cmp x1 x2
      | Extr(b1,l1,u1), Extr(b2,l2,u2) ->
	  Cmp.lexico3 cmp_fixed b1 b2 Pervasives.compare l1 l2 Pervasives.compare u1 u2
      | Conc l1, Conc l2 ->
	  Cmp.list cmp_fixed l1 l2
      | BvIte(u1,v1,w1), BvIte(u2,v2,w2) ->
	  Cmp.lexico3 cmp_fixed u1 u2 cmp_fixed v1 v2 cmp_fixed w1 w2
      | _ -> assert false) x y

and cmp_bv x y = cmp_bv_node x y

and cmp_fixed (n1,t1) (n2,t2) =
  Cmp.lexico2 Pervasives.compare n1 n2 cmp t1 t2
	
(*s Some recognizers. *)

let is_const t =
  match t.node with
    | Bool True
    | Bool False
    | Arith (Num _)
    | Set (Full _)
    | Set (Empty _)
    | Bv (Const _) -> true
    | _ -> false

(*s Sets of terms *)
	
exception SFound of term
exception Found

module Set = struct
  type elt = term
  type t = tnode Ptset.t  
  let empty = Ptset.empty
  let add = Ptset.add
  let singleton t = add t empty
  let is_empty = Ptset.is_empty
  let remove = Ptset.remove
  let mem = Ptset.mem
  let iter = Ptset.iter
  let iter2 f s = iter (fun x -> iter (fun y -> f x y) s) s
  let fold = Ptset.fold
  let map f s = Ptset.fold (fun x acc -> add (f x) acc) s empty       
  let union = Ptset.union
  let filter p s =
    fold (fun x acc -> if p x then add x acc else acc) s empty
  let inter s1 s2 = filter (fun x -> mem x s2) s1			     
  let to_list s = fold (fun x acc -> x :: acc) s []	  
  let choose p s =
    try
      iter (fun a -> if p a then raise (SFound a)) s;
      raise Not_found
    with
	SFound a -> a
  let exists p s =
    try
      iter (fun a -> if p a then raise Found) s;
      false
    with
	Found -> true
  let for_all p s =
    not(exists (fun x -> not(p x)) s)
  let sub s1 s2 =
    not (exists (fun x -> not (mem x s2)) s1)
  let destructure s =
    let x = choose (fun _ -> true) s in
    x, remove x s
end

(*s Finite maps with terms as domain *)

exception MFound of term * term

module Map = struct
  type 'a t = (tnode,'a) Ptmap.t
	      
  let empty = Ptmap.empty
  let is_empty m = Ptmap.fold (fun _ _ _ -> true) m false
  let add = Ptmap.add
  let mem = Ptmap.mem
  let find = Ptmap.find
  let remove = Ptmap.remove
  let map = Ptmap.map
  let iter = Ptmap.iter
  let fold = Ptmap.fold
	     
  let choose p m =
    try
      iter (fun a b -> if p a b then raise (MFound (a,b))) m;
      raise Not_found
    with
	MFound (a,b) -> (a,b)
		     
  let to_list m =
    fold (fun x y acc -> (x,y) :: acc) m []

	
end


(*s [occurs_interpreted a b] tests if term [a] occurs interpreted in [b]; in
    particular, this test fails if [a] is in the scope of an uninterpreted function symbol.
  *)

let occurs_interpreted a b =
  let rec occ x =
    a === x ||
       (match x.node with
	  | Var _ | App _ | Update _ -> false
	  | Tuple x ->
	      (match x with
		 | Tup xl -> List.exists occ xl
		 | Proj (_,_,x) -> occ x)
	  | Bool x ->
	      (match x with
		 | True | False | Equal _ -> false
		 | Ite(x,y,z) -> occ x || occ y || occ z
		 | _ -> assert false)
	  | Set x ->
	      (match x with
		 | Empty _ | Full _ | Finite _ | Cnstrnt _ -> false
		 | SetIte(tg,x,y,z) -> occ x || occ y || occ z)
	  | Bv x ->
	      (match x with
		 | Const _ -> false
		 | Extr((_,x),_,_) -> occ x
		 | BvToNat _ -> false
		 | Conc l -> List.exists (fun (_,x) -> occ x) l
		 | BvIte((n,x),(_,y),(_,z)) -> occ x || occ y || occ z)
	  | Arith x ->
	      (match x with
		 | Num _ | Div _ | Mult _ -> false
		 | Multq(_,x) -> occ x
		 | Add xl -> List.exists occ xl))
  in
  occ b


(*s [is_ground a] tests if [a] contains neither a variable n
    nor an application of an uninterpreted function symbol. *)

let rec is_ground a = 
    match a.node with
      | Var _ | App _ -> false
      | Update(x,y,z) -> is_ground x && is_ground y && is_ground z
      | Tuple x ->
	  (match x with
	     | Tup xl -> List.for_all is_ground xl
	     | Proj (_,_,x) -> is_ground x)
      | Bool x ->
	  (match x with
	     | True | False -> true
	     | Equal(x,y) -> is_ground x && is_ground y
	     | Ite(x,y,z) -> is_ground x && is_ground y && is_ground z
	     | _ -> assert false)
      | Set x ->
	  (match x with
	     | Empty _ | Full _ | Cnstrnt _ -> true
	     | Finite s -> Ptset.for_all is_ground s
	     | SetIte(tg,x,y,z) -> is_ground x && is_ground y && is_ground z)
      | Bv x ->
	  (match x with
	     | Const _ -> true
	     | Extr((_,x),_,_) -> is_ground x
	     | BvToNat(x) -> is_ground x
	     | Conc l -> List.exists (fun (_,x) -> is_ground x) l
	     | BvIte((n,x),(_,y),(_,z)) -> is_ground x && is_ground y && is_ground z)
      | Arith x ->
	  (match x with
	     | Num _ -> true
	     | Add xl -> List.for_all is_ground xl
	     | Multq(_,x) -> is_ground x
	     | Mult xl -> List.for_all is_ground xl
	     | Div(x,y) -> is_ground x && is_ground y)
	
let is_uninterpreted a =
  match a.node with
    | Var _ -> true
    | App _ -> true
    | Update _ -> true
    | Arith(Mult _) -> true
    | Arith(Div _) -> true
    | Bv(BvToNat _) -> true
    | Set(Finite _) -> true
    | Set(Cnstrnt _) -> true
    | _ -> false

let iter_uninterpreted f a =
   let rec loop t =
    match t.node with
      | Var _ | App _ | Set(Cnstrnt _) ->
	  f t
      | Update(x,y,z) -> loop x; loop y; loop z
      | Arith(Mult l) -> List.iter loop l
      | Arith(Div(x,y))  -> loop x; loop y
      | Bv(BvToNat x) -> loop x;
      | Bool(Equal(x,y)) -> loop x; loop y
      | Set s->
	  (match s with
	     | SetIte(_,x,y,z) -> loop x; loop y; loop z
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






