
(*i*)
open Hashcons
open Mpa
open Bitv
(*i*)

(*s Implementation of terms and functions over terms. \label{implterms}
    The datatypes for terms have been discussed in \fullrefsec{typeterms}. *)

type sort = Int | Real
  
type cnstrnt = Pos | Neg | Nonneg | Nonpos

type variable = string * sort option * cnstrnt option

type tag = int

type term_node =
  | Var of variable
  | App of term * term list
  | Update of term * term * term
  | Arith of arith
  | Tuple of tuple
  | Atom of atom
  | Bool of prop
  | Set of set
  | Bv of bv
        
and term = term_node hashed

and atom =
  | Equal of term * term
  | Le of term * term
  | Lt of term * term
  | Integer of term     

and arith =
  | Num of Q.t
  | Times of term list
  | Plus of term list

and prop =
  | True
  | False
  | Ite of term * term * term
  | Forall of variable list * term
  | Exists of variable list * term

and set =
  | Empty of tag
  | Full of tag
  | SetIte of tag * term * term * term
     
and tuple =
  | Tup of term list
  | Proj of int * int * term

and bv =
  | Const of Bitv.t
  | Conc of fixed list
  | Extr of fixed * int * int
  | BvIte of fixed * fixed * fixed

and fixed = int * term
     
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
    type t = term_node
    let equal t1 t2 =
      match t1, t2 with
	| Var (x1,typ1,sgn1), Var (x2,typ2,sgn2) -> 
	    x1 == x2 && typ1 = typ2 && sgn1 = sgn2
	| App (s1,l1), App (s2,l2) ->
	    s1 == s2 &&
            (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
	| Atom a1, Atom a2 ->
	    (match a1, a2 with
	       | Equal (s1,t1), Equal (s2,t2) ->
		   s1 == s2 && t1 == t2
	       | Le (s1,t1), Le (s2,t2) ->
		   s1 == s2 && t1 == t2
	       | Lt (s1,t1), Le (s2,t2) ->
		   s1 == s2 && t1 == t2
	       | Integer s1, Integer s2 ->
		   s1 == s2
	       | _ -> false)
	| Arith a1, Arith a2 ->
	    (match a1, a2 with
	       | Num(q1), Num(q2) ->
		   Q.equal q1 q2
	       | Times l1, Times l2 ->
		   (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
	       | Plus l1, Plus l2 ->
		   (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
	       | _ -> false)
	| Tuple t1, Tuple t2 ->
	    (match t1,t2 with
	       | Tup l1, Tup l2 ->
		   (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
	       | Proj (i1,n1,t1), Proj (i2,n2,t2) ->
		   i1 == i2 && n1 == n2 && t1 == t2
	       | _ -> false)
	| Set t1, Set t2 ->
	    (match (t1,t2) with
	       | SetIte (t1,a1,b1,c1), SetIte (t2,a2,b2,c2) ->
		   a1 == a2 && b1 == b2 && c1 == c2 && t1 = t2
	       | Empty t1, Empty t2 ->
		   t1 == t2
	       | Full t1, Full t2 ->
		   t1 == t2
	       | _ -> false)	    
	| Bool t1, Bool t2  ->
	    (match t1,t2 with
	       | True, True -> true
	       | False, False -> true
	       | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
		   a1 == a2 && b1 == b2 && c1 == c2
	       | Forall (xl,p), Forall (yl,q) ->
		   p == q && (try List.for_all2 (==) xl yl with Invalid_argument _ -> false)
	       | Exists (xl,p), Exists (yl,q) ->
		   p == q && (try List.for_all2 (==) xl yl with Invalid_argument _ -> false) 
	       | _ -> false) 
	| Bv t1, Bv t2 ->
	    (match t1,t2 with
	       | Const c, Const d ->
		   compare c d = 0
	       | Extr(b1,l1,u1), Extr(b2,l2,u2)  ->
		   b1 == b2 && l1 = l2 && u1 = u2
	       | Conc l1, Conc l2 ->
		   (try List.for_all2 (==) l1 l2  with Invalid_argument _ -> false)
	       | BvIte(u1,v1,w1), BvIte(u2,v2,w2) ->
		   u1 == u2 && v1 == v2 && w1 == w2
	       | _ -> false)
	| _ -> false
    let hash =
      function
      | App (s,l) ->
	  s.tag + (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
      | Arith a ->
	  (match a with
	     | Num q -> Q.hash q
	     | Times l ->
		 (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
	     | Plus l ->
		 (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF)
      | Bool p ->
	  (match p with
	     | Ite (a,b,c) -> (31 + a.hkey + b.hkey + c.hkey) land 0x3FFFFFFF
	     | _ -> Hashtbl.hash p)
      | Set s ->
	  (match s with
	     | (Empty _) as t ->  Hashtbl.hash t
	     | (Full _) as t -> Hashtbl.hash t 
	     | SetIte (_,a,b,c) -> (a.tag + b.tag + c.tag ) land 0x3FFFFFFF)
      | Tuple t ->
	  (match t with
	     | Tup l -> (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
	     | Proj (i,n,t) -> (i + n + t.tag) land 0x3FFFFFFF)
      | Bv l ->
	  (match l with
	     | Const c -> Hashtbl.hash c 
	     | Extr((_,b),i,j) -> (i + j + b.tag) land 0x3FFFFFFF
	     | Conc l -> (List.fold_left (fun h (_,a) -> h+a.tag) 1 l) land 0x3FFFFFFF
	     | BvIte((_,a),(_,b),(_,c)) -> (a.tag + b.tag + c.tag) land 0x3FFFFFFF)
      | Atom a ->
	  (match a with
	     | Integer(t) -> 5 + t.tag  + 1 land 0x3FFFFFFF
	     | Equal(s,t) -> (17 + s.tag + t.tag) land 0x3FFFFFFF
	     | Le(s,t) -> (19 + s.tag + t.tag) land 0x3FFFFFFF
	     | Lt(s,t) -> (23 + s.tag + t.tag) land 0x3FFFFFFF)   
      | x -> Hashtbl.hash x
  end)

let hc : term_node -> term =
  let ht = HashTerm.create 251 in
  Tools.add_at_exit (fun () -> print_string "Terms: "; HashTerm.stat ht);
  Tools.add_at_reset (fun () -> HashTerm.clear ht);
  fun t -> HashTerm.hashcons ht t
 

(*s Efficient comparisons using hash-consing. *)

let fast_cmp x y = x.tag - y.tag		       
			       
(*s Caches for functions over terms. *)

module HCache =
  Hashtbl.Make(
    struct 
      type t = term
      let equal = (==) 
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
      let equal (x1,y1) (x2,y2) = x1 == x2 && y1 == y2
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
      let equal l1 l2 = try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false
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
     | Update (s1,t1,u1), Update (s2,t2,u2) ->
	  Cmp.lexico3 cmp s1 s2 cmp t1 t2 cmp u1 u2
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
     | Atom(Integer t1), Atom(Integer t2) ->
	 cmp t1 t2
     | _ -> assert false) x.node y.node

and cmp_arith a1 a2 = Cmp.generic
   (function
      | Num q1, Num q2 ->
	  Q.compare q1 q2
      | Plus l1, Plus l2 ->
	  Cmp.list cmp l1 l2
      | Times l1, Times l2 ->
	  Cmp.list cmp l1 l2
      | _ -> assert false) a1 a2
	
and cmp_prop_node x y = Cmp.generic
 (function
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

let is_uninterpreted =
  let rec is_uninterp x =
    match x.node with
      | Var _
      | App _
      | Atom(Integer _) ->
	  true
      | Arith (Times (_::_::_ as l)) ->         (* nonlinear *)
	  List.for_all is_uninterp l
      | _ ->
	  false
  in
  cache 10007 is_uninterp

let is_const t =
  match t.node with
    | Bool True
    | Bool False
    | Arith (Num _)
    | Set (Full _)
    | Set (Empty _)
    | Bv (Const _) -> true
    | _ -> false
 
