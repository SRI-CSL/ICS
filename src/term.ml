
(*i*)
open Hashcons
open Tools
open Mpa
open Bitv
(*i*)

(*s Implementation of terms and functions over terms. \label{implterms}
    The datatypes for terms have been discussed in \fullrefsec{typeterms}. *)

type constraints = Int | All
  
type variable = string * constraints

type tag = int

type term_node =
  | Var of variable
  | App of term * term list
  | Update of term * term * term
  | Equal of term * term
  | Bv of bv
  | Arith of arith
  | Tuple of tuple
  | Bool of prop
  | Set of set
  | Mem of term * term
  | Integer of term
        
and term = term_node hashed

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

(*s Hash-consing of strings. Used for variables and uninterpreted symbols. *)

let string_ht = Hasht.create 251 

let _ = add_at_exit (fun () -> print_string "Str. : "; Hasht.stat string_ht)

let hc_string (s : string) = 
  try Hasht.find string_ht s with Not_found -> Hasht.add string_ht s s; s
      
(*s Hash-consing of terms. *)

module HashTerm = Hashcons.Make(
  struct 
    type t = term_node
    let equal t1 t2 =
      match (t1,t2) with
	| Var (s1,_), Var (s2,_) ->    (* ignore typing *)
	    s1 == s2
	| App (s1,l1), App (s2,l2) ->
	    s1 == s2 &&
            (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
	| Equal (s1,t1), Equal (s2,t2) ->
	    s1 == s2 && t1 == t2
	| Arith a1, Arith a2 ->
	    (match a1, a2 with
	       | Num(q1), Num(q2) -> Q.equal q1 q2
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
	       | Empty t1, Empty t2 -> t1 == t2
	       | Full t1, Full t2 -> t1 == t2
	       | _ -> false)	    
	| Bool t1, Bool t2  ->
	    (match t1,t2 with
	       | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
		   a1 == a2 && b1 == b2 && c1 == c2
	       | Forall (xl,p), Forall (yl,q) ->
		   p == q && (try List.for_all2 (==) xl yl
			      with Invalid_argument _ -> false)
	       | True, True -> true
	       | False, False -> true
	       | _ -> false) 
	| Bv t1, Bv t2 ->
	    (match t1,t2 with
	       | Const c, Const d -> compare c d = 0
	       | Extr(b1,l1,u1), Extr(b2,l2,u2)  ->
		   b1 == b2 && l1 = l2 && u1 = u2
	       | Conc l1, Conc l2 ->
		   (try List.for_all2 (==) l1 l2  with Invalid_argument _ -> false)
	       | BvIte(u1,v1,w1), BvIte(u2,v2,w2) ->
		   u1 == u2 && v1 == v2 && w1 == w2
	       | _ -> false)
	| Mem (x1,t1), Mem (x2,t2)  -> x1 == x2 && t1 == t2
	| Integer t1, Integer t2 -> t1 == t2
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
      | Mem (x,s) -> 3 + x.tag + s.tag land 0x3FFFFFFF
      | Integer(t) -> 5 + t.tag  + 1 land 0x3FFFFFFF
      | Equal(s,t) -> (17 + s.tag + t.tag) land 0x3FFFFFFF
      | x -> Hashtbl.hash x
  end)

let hc_term : term_node -> term =
  let ht = HashTerm.create 251 in
  add_at_exit (fun () -> print_string "Terms: "; HashTerm.stat ht);
  add_at_reset (fun () -> HashTerm.clear ht);
  fun t -> HashTerm.hashcons ht t
 

(*s Hashconsing constructors for terms. *)
  
let var (s,kind) = hc_term (Var (hc_string s,kind))
	      
let app f l = hc_term (App (f, l))
  
let update f x e = hc_term (Update (f,x,e))
    
let equal a b = hc_term (Equal (a,b))
    
let mem x s = hc_term (Mem (x, s))
	       
let integer t = hc_term (Integer t)
  
let num q  = hc_term (Arith (Num q))
	       
let mult = function
  | [] -> num Q.one
  | [t] -> t
  | l -> hc_term (Arith (Times l))
	       
let add = function
  | [] -> num Q.zero
  | [t] -> t
  | l -> hc_term (Arith (Plus l))

let ptrue () = hc_term (Bool True)
let pfalse () = hc_term (Bool False)
		  
let ite a b c = hc_term (Bool (Ite (a, b, c)))
    
let forall xl p = hc_term (Bool (Forall (xl, p)))
let exists xl p = hc_term (Bool (Exists (xl, p)))    
 
let tuple l = hc_term (Tuple (Tup l))
let proj i n t = hc_term (Tuple (Proj (i,n,t)))

let empty tg = hc_term (Set (Empty tg))
let full tg = hc_term (Set (Empty tg))
		
let setite tg a b c = hc_term (Set (SetIte (tg,a,b,c)))

let width_of b =
  match b.node with
    | Bv b ->
	(match b with
	   | Const c -> Bitv.length c
	   | Extr(_,i,j) -> j-i+1
	   | Conc l -> List.fold_left (fun a (n,_) -> a + n) 0 l
	   | BvIte((n,_),_,_) -> n)
    | _ -> assert false
			
let fixed n b =
  assert (n >= 0);
  (n,b)
		  
let const c = hc_term (Bv (Const c))
		
let conc bl =
(*  assert (List.for_all (fun {node=n,_} -> n > 0) bl); *)
  hc_term (Bv (Conc bl))
		
let extr b i j =
(*  assert (0 <= i && i <= j && j < let (n,_) = b.node in n); *)
  hc_term (Bv (Extr (b,i,j)))
		   
let bvite b1 b2 b3 =
 (* assert (let (n1,_) = b1.node and (n2,_) = b2.node and (n3,_) = b3.node in
	  n1 > 0 && n1 = n2 && n2 = n3); *)
  hc_term (Bv (BvIte (b1,b2,b3)))

    
(*s Efficient comparisons using hash-consing. *)

let eq_term  = (==) 

let fast_compare_term x y  = x.tag - y.tag
			       
			       
(*s Caches for functions over terms. *)

module HCache = Hashtbl.Make(
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

      
module HCache2 = Hashtbl.Make(
  struct 
    type t = term * term
    let equal (x1,y1) (x2,y2) =
      x1 == x2 && y1 == y2
    let hash (x,y) = (x.tag + y.tag) land 0x3FFFFFFF
  end)
    
let cache2 n f =
  let ht = HCache2.create n in
  fun x ->
    try
      HCache2.find ht x
    with
	Not_found -> let y = f x in HCache2.add ht x y; y

     
module HCachel = Hashtbl.Make(
  struct 
    type t = term list
    let equal l1 l2 =
      try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false
    let hash l =
      (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
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

let rec compare_term_node x y = gen_compare
  (function
     | Var (s1,_), Var (s2,_) -> Pervasives.compare s1 s2
     | App (s1,l1), App (s2,l2) -> 
	 lexico2
	   compare_term s1 s2
	   (compare_list compare_term) l1 l2
     | Update (s1,t1,u1), Update (s2,t2,u2) ->
	  lexico3
	    compare_term s1 s2
	    compare_term t1 t2
	    compare_term u1 u2
     | Arith a1, Arith a2 ->
	 compare_arith a1 a2
     | Tuple t1, Tuple t2 ->
	 compare_tuple_node t1 t2
     | Set t1, Set t2   ->
	 compare_set_node t1 t2
     | Bool t1,  Bool t2  ->
	 compare_prop_node t1 t2
     | Bv b1, Bv b2   ->
	 compare_bv b1 b2
     | Integer t1, Integer t2 ->
	 compare_term t1 t2
     | Mem(x1,t1), Mem(x2,t2) ->
	 lexico2
	   compare_term x1 x2
	   compare_term t1 t2
     | _ -> assert false) x y
				    
and compare_term x y =
  compare_term_node x.node y.node

and compare_arith a1 a2 = gen_compare
   (function
      | Num q1, Num q2 ->
	  Q.compare q1 q2
      | Plus l1, Plus l2 ->
	  compare_list compare_term l1 l2
      | Times l1, Times l2 ->
	  compare_list compare_term l1 l2
      | _ -> assert false) a1 a2
	
and compare_prop_node x y = gen_compare
 (function
    | Ite (a1,b1,c1), Ite (a2,b2,c2) -> 
	lexico2
	  compare_term a1 a2 
	  (lexico2 compare_term b1 b2 compare_term) c1 c2
    | Forall (xl,p), Forall (yl,q) ->
	lexico2
	  (compare_list Pervasives.compare) xl yl
	  compare_term p q
    | _ -> assert false) x y

and compare_prop x y = compare_prop_node x.node y.node

and compare_set_node x y = gen_compare
  (function 
     | SetIte (t1,a1,b1,c1), SetIte (t2,a2,b2,c2) ->
	 lexico4
	   Pervasives.compare t1 t2
	   compare_term a1 a2
	   compare_term b1 b2
	   compare_term c1 c2
     | _ -> assert false) x y

and compare_set x y = compare_set_node x.node y.node

and compare_tuple_node x y = gen_compare
  (function
     | Tup l1, Tup l2 -> compare_list compare_term l1 l2
     | Proj (i1,n1,t1), Proj (i2,n2,t2) ->
	 lexico3
	   Pervasives.compare i1 i2
	   Pervasives.compare n1 n2
	   compare_term t1 t2
     | _ -> assert false) x y

and compare_tuple x y = compare_tuple_node x.node y.node

and compare_bv_node x y = gen_compare
   (function 
      | Const b1, Const b2 ->
	  Pervasives.compare b1 b2
      | Extr(b1,l1,u1), Extr(b2,l2,u2) ->
	  lexico3
	    compare_fixed b1 b2
	    Pervasives.compare l1 l2
	    Pervasives.compare u1 u2
      | Conc l1, Conc l2 ->
	  compare_list compare_fixed l1 l2
      | BvIte(u1,v1,w1), BvIte(u2,v2,w2) ->
	  lexico3
	    compare_fixed u1 u2
	    compare_fixed v1 v2
	    compare_fixed w1 w2
      | _ -> assert false) x y

and compare_bv x y = compare_bv_node x y

and compare_fixed (n1,t1) (n2,t2) =
  lexico2 Pervasives.compare n1 n2 compare_term t1 t2
	
(*s Some recognizers. *)

let is_var x = match x.node with
  | Var _ -> true
  | _ -> false

let is_uninterpreted =
  let rec is_uninterp x =
    match x.node with
      | Var _ | App _ | Integer _ | Mem _ -> true
      | Arith (Times (_::_::_ as l)) ->         (* nonlinear *)
	  List.for_all is_uninterp l
      | _ -> false
  in
  cache 10007 is_uninterp

let is_num x = match x.node with
  | Arith (Num _) -> true
  | _ -> false

let num_of x = match x.node with
  | Arith (Num q) -> Some q
  | _ -> None

let val_of x = match x.node with
  | Arith (Num q) -> q
  | _ -> assert false

let is_const x = match x.node with
  | Bool True
  | Bool False
  | Arith (Num _)
  | Set (Full _)
  | Set (Empty _)
  | Bv (Const _) -> true
  | _ -> false

let is_arith x = match x.node with
  | Arith  _ -> true
  | _ -> false
	
let is_bv x = match x.node with
  | Bv _ -> true
  | _ -> false
			  
(*s Fresh variables. They are constants i.e. 0-ary uninterpreted symbols. 
    They are given named like \_c123 in order to prevent clashes with
    the user's symbols. *)

let fresh =
  let fresh_counter = ref 0 in
  add_at_reset (fun () -> fresh_counter := 0);
  fun s l k -> 
    incr fresh_counter;
    app (var ("_" ^ s ^ string_of_int !fresh_counter,k)) l

let new_var s k =
  let rec look i =
    let s' = s ^ string_of_int i in
    if Hasht.mem string_ht s' then look (succ i) else var (s',k)
  in
  look 0

(*s Test for atomicity. *)

let is_atomic t =
  match t.node with
    | Arith (Num _)
    | Var _ -> true
    | _ -> false
     
(*s The following exceptions are raised by any solver which detects an 
     nconsistent system or an obvious validity. *)
      
exception Inconsistent of string
 
exception Valid
