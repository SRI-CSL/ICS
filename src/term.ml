
(*i*)
open Hashcons
open Mpa
open Format
(*i*)

(*s Type of constraints. *)

type cnstrnt = 
  | Top
  | BooleanCnstrnt
  | ArithCnstrnt of Interval.t
  | TupleCnstrnt
  | Bot

let ceq c d =
  match c,d with
    | Top, Top -> true
    | BooleanCnstrnt, BooleanCnstrnt -> true
    | ArithCnstrnt(x), ArithCnstrnt(y) ->
	Interval.eq x y
    | TupleCnstrnt, TupleCnstrnt -> true
    | Bot, Bot -> true
    | _ -> false

(*s Name of variables. *)

type name = string

(*s Interpreted function symbols *)

type arith = 
  | Num of Q.t  
  | Multq of Q.t
  | Add 
  | Mult 
  | Div

and tuple = 
  | Product 
  | Proj of int * int

and boolean = 
  | True 
  | False 
  | Ite

and interp = 
  | Arith of arith
  | Tuple of tuple
  | Bool of boolean

and pred = 
  | Equal
  | Cnstrnt of cnstrnt

and builtin = 
  | Update 

and props = AC | A | C

and symnode = 
  | Uninterp of t * domain option * props option
  | Interp of interp
  | Pred of pred
  | Builtin of builtin

and sym = symnode hashed

and domain = 
  | RatDom
  | IntDom
  | BoolDom

and kind =
  | Ext of domain option    (* External variables; interpretation may be constrained to domain. *)
  | Rename of t             (* Invariant: [Var("x",EqVar(t)) = t] *)
  | Fresh of domain         (* Fresh variables introduce by solvers. *)

and tnode =
  | Var of name * kind
  | App of sym * (t list)

and  t = tnode hashed

type eqn = t * t
type diseq = t * t

	  (*s In order to save space and to provide an efficient
	    comparison of terms, we choose to perform full hash-consing over terms.
	    It means that whenever [x] is structurally equal to [y] ([x=y])
	    then it will be actually \emph{physically} equal to [y] ([x==y]). 
	    Hash-consing is realized using hash-tables, where an already existing 
	    and equal object is looked for each time we try to create a new object i.e.
	    each time we apply a constructor. 
	    We use our own version of hash tables, which provides a [stat] function
	    to look at the distribution of values inside a given table. *)

 
let string_ht = Hashtbl.create 107      (*s Hashconsing of strings. *)

let hc_string (s: string) = 
  try
    Hashtbl.find string_ht s
  with
      Not_found ->
	Hashtbl.add string_ht s s;
	s

module HashSym = Hashcons.Make(        (*s Hashconsing of symbols *)
  struct 
    type t = symnode
    let equal s1 s2 =
      match s1, s2 with
	| Interp(sym1), Interp(sym2) -> 
	    (match sym1, sym2 with
	       | Arith(op1), Arith(op2) ->
		   (match op1, op2 with
		      | Num(q1), Num(q2) -> Q.equal q1 q2
		      | Multq(q1), Multq(q2) -> Q.equal q1 q2
		      | Add, Add -> true
		      | Mult, Mult -> true
		      | Div, Div -> true
		      | _ -> false)
	       | Tuple(op1), Tuple(op2) ->
		   (match op1, op2 with
		      | Product, Product -> true 
		      | Proj(i,n), Proj(j,m) -> i = j && n = m
		      | _ -> false)
	       | Bool(op1), Bool(op2) ->
		   op1 = op2
	       | _ -> false)
	| Builtin(b1), Builtin(b2) ->
	    b1 = b2
	| Uninterp(f1,d1,p1), Uninterp(f2,d2,p2) ->
	    f1 === f2 && d1 = d2 && p1 = p2
	| Pred(p1), Pred(p2) ->
	    (match p1,p2 with
	       | Equal, Equal -> true
	       | Cnstrnt(c1), Cnstrnt(c2) -> ceq c1 c2
	       | _ -> false)
	| _ ->
	    false
    let hash = function
      | Uninterp(f,_,_) -> 
	  f.tag
      | Builtin(b) ->
	  Hashtbl.hash b
      | Interp(op) ->
	  Hashtbl.hash op
      | Pred(p) ->
	  Hashtbl.hash p
  end)

let hc_sym : symnode -> sym =
  let ht = HashSym.create 107 in
  Tools.add_at_reset (fun () -> HashSym.clear ht);
  HashSym.hashcons ht
  
let eql l1 l2 =
  try
    List.for_all2 (===) l1 l2
  with
      Invalid_argument _ -> false

module HashTrm = Hashcons.Make(           (*s Hashconsing of terms. *)
  struct 
    type t = tnode
    let equal t1 t2 =
      match t1, t2 with
	| Var(x1,k1), Var(x2,k2) ->         (*s Variables are equal if their names are equal. *)
	    x1 == x2 && k1 = k2
	| App(s1,l1), App(s2,l2)  ->
            s1 === s2 && eql l1 l2
	| _ ->
	    false
    let hash = function
      | App(f,l) ->
	  f.tag + (List.fold_left (fun h a -> h + a.tag) 1 l) land 0x3FFFFFFF
      | x ->
	  Hashtbl.hash x
  end)

let hc : tnode -> t =
  let ht = HashTrm.create 107 in
  Tools.add_at_reset (fun () -> HashTrm.clear ht);
  HashTrm.hashcons ht


	
	
	  (*s Efficient comparisons using hash-consing. *)

let fast_cmp x y = x.tag - y.tag		       
	     
(*s Structural comparison. Contrary to the above functions, the result
    of the following comparison functions has to be
    session-independent.  Due to the presence of hash-consing tagging
    inside the term data-structures, we cannot use the
    [Pervasives.compare] function directly. We have to write our own
    structural comparison functions, with the help of [Tools.gen_compare]. *)

let rec cmp x y =
  match x.node, y.node with
     | Var(s1,k1), Var(s2,k2) ->    
         cmp_var (s1,k1) (s2,k2)
     | Var(s1, k1), _ -> 
	 -1
     | _, Var(s2 ,k2) -> 
	 1
     | App(f1,l1), App(f2,l2) -> 
	 Cmp.lexico2 cmp_sym f1 f2 (Cmp.list cmp) l1 l2

and cmp_var (s1,k1) (s2,k2) =  (* fresh vars <<< rename vars <<< external vars. *)
  match k1, k2 with                       
    | Fresh _, (Rename _ | Ext _) -> -1
    | (Rename _ | Ext _), Fresh _ -> 1
    | Rename _, Ext _ -> -1
    | Ext _, Rename _ -> 1
    | _ -> Pervasives.compare s1 s2

and cmp_sym f1 f2 = 
  Pervasives.compare f1 f2

let (<<<) a b = cmp a b < 0
	

    (*s Some recognizers. *)

let is_const t =
  match t.node with
    | App({node=Interp(f)},_) ->
	(match f with
	   | Bool(True | False) -> true
	   | Arith(Num _) -> true
	   | _ -> false)
    | _ ->
	false

	  (* Variables. *)

let mk_var s = 
  hc(Var(hc_string s, Ext(None)))

let mk_intvar s =
  hc(Var(hc_string s, Ext(Some(IntDom))))

let mk_ratvar s =
  hc(Var(hc_string s, Ext(Some(RatDom))))

let mk_boolvar s =
  hc(Var(hc_string s, Ext(Some(BoolDom))))

let is_var a =
  match a.node with
    | Var _ -> true
    | _ -> false

let d_var a =
  assert(is_var a);
  match a.node with
    | Var(s,_) -> s
    | _ -> assert false

let is_intvar a =
  match a.node with
    | Var(_,Ext(Some(IntDom))) -> true
    | _ -> false

let is_ratvar a =
  match a.node with
    | Var(_,Ext(Some(RatDom))) -> true
    | _ -> false

let is_boolvar a =
  match a.node with
    | Var(_,Ext(Some(BoolDom))) -> true
    | _ -> false

let counter = ref 0

let _ = Tools.add_at_reset (fun () -> counter := 0)

let rename = Hashtbl.create 107

let _ = Tools.add_at_reset (fun () -> Hashtbl.clear rename)

let mk_rename_var = 
  (fun str a ->
     try
       Hashtbl.find rename a
     with
	 Not_found ->
	   incr counter;
	   let x = hc(Var(hc_string(str ^ string_of_int !counter), Rename(a))) in
	   Hashtbl.add rename a x;
	   x)

let name_of a =
  Hashtbl.find rename a

let is_rename_var a =
  match a.node with
    | Var(_, Rename _) -> true
    | _ -> false

let d_rename_var a =
  assert(is_rename_var a);
  match a.node with
    | Var(s, Rename(x)) -> (s,x)
    | _ -> assert false

let mk_fresh dom =
  incr counter;
  hc(Var(hc_string("_c" ^ string_of_int !counter), Fresh(dom)))

let is_fresh a =
  match a.node with
    | Var(_, Fresh _) -> true
    | _ -> false

let d_fresh a =
  assert(is_fresh a);
  match a.node with
    | Var(s, Fresh(dom)) -> (s,dom)
    | _ -> assert false

let is_internal_var a =
  match a.node with
    | Var(_, Ext _) -> false
    | _ -> true


	  (*s Applications. *)

let mk_app f l =
  hc(App(f,l))

let is_app a =
  match a.node with
    | App _ -> true
    | _ -> false

let d_app a =
  assert(is_app a);
  match a.node with
    | App(f,l) -> (f,l)
    | _ -> assert false

let mk_uninterp_sym dom knd a = 
  hc_sym(Uninterp(a,dom,knd))

let mk_uninterp dom knd a l =
  hc(App(mk_uninterp_sym dom knd a,l))

let is_uninterp a =
  match a.node with
    | App({node=Uninterp _},_) -> true
    | _ -> false

let d_uninterp a =
  assert(is_uninterp a);
  match a.node with
    | App({node=Uninterp(a,dom,knd)},l) -> (dom,knd,a,l)
    | _ -> assert false

let is_interp a =
  match a.node with
    | App({node=Interp _},_) -> true
    | _ -> false

let is_pred a =
  match a.node with
    | App({node=Pred _},_) -> true
    | _ -> false

let is_builtin a =
  match a.node with
    | App({node=Builtin _},_) -> true
    | _ -> false


  (*s Arithmetic function symbols, constructors, and recognizers. *)

let num_sym q = hc_sym(Interp(Arith(Num(q))))
let multq_sym q = hc_sym(Interp(Arith(Multq q)))
let add_sym = hc_sym(Interp(Arith(Add)))
let mult_sym = hc_sym(Interp(Arith(Mult)))
let div_sym = hc_sym(Interp(Arith(Div)))

let mk_num q = hc(App(num_sym q,[]))    (* Arithmetic constants *)
let mk_zero = mk_num(Q.zero)
let mk_one = mk_num(Q.one)

let is_num a =
  match a.node with
    | App({node=Interp(Arith(Num _))},[]) -> true
    | _ -> false

let is_zero a =
  match a.node with
    | App({node=Interp(Arith(Num(q)))},[]) -> Q.is_zero q
    | _ -> false

let is_one a =
  match a.node with
    | App({node=Interp(Arith(Num(q)))},[]) -> Q.is_one q
    | _ -> false

let d_num a =
  assert(is_num a);
  match a.node with
    | App({node=Interp(Arith(Num(q)))},[]) -> q
    | _ -> assert false


let mk_multq q x =                      (* Linear multiplication [q*x] *)
  if Q.is_zero q then 
    mk_num(Q.zero)
  else if Q.is_one q then
    x
  else 
    hc(App(multq_sym q,[x]))

let is_multq a =
  match a.node with
    | App({node=Interp(Arith(Multq _))},[_]) -> true
    | _ -> false

let d_multq a =
  assert(is_multq a);
  match a.node with
    | App({node=Interp(Arith(Multq(q)))},[x]) -> (q,x)
    | _ -> assert false


let mk_add = function                   (* [n]-ary addition *)
  | [] -> mk_num(Q.zero)
  | [x] -> x
  | l -> hc(App(add_sym, l))

let is_add a =
  match a.node with
    | App(f,_) -> f == add_sym
    | _ -> false

let d_add a =
  assert(is_add a);
  match a.node with
    | App(_, l) ->l
    | _ -> assert false


let mk_mult = function                      (* [n]-ary multiplication *)  
  | [] -> mk_num(Q.one)
  | [x] -> x
  | l -> hc(App(mult_sym,l))

let is_mult a =                      
  match a.node with
    | App(f,_) -> f == mult_sym
    | _ -> false

let d_mult a =
  assert(is_mult a);
  match a.node with
    | App(_, l) -> l
    | _ -> assert false

let mk_div (a,b) =
  hc(App(div_sym, [a;b]))

let is_div a =
  match a.node with
    | App(f,[_;_]) -> f === div_sym
    | _ -> false

let d_div a =
  assert(is_div a);
  match a.node with
    | App(_,[a;b]) -> (a,b)
    | _ -> assert false

          (*s Tuple and projection. *)

let tuple_sym = hc_sym(Interp(Tuple(Product)))
let proj_sym i n = hc_sym(Interp(Tuple(Proj(i,n))))

let mk_tuple = function
  | [x] -> x
  | [] -> assert false
  | l -> hc(App(tuple_sym,l))

let is_tuple a =
  match a.node with
    | App(f,_) -> f === tuple_sym
    | _ -> false

let d_tuple a =
  assert(is_tuple a);
  match a.node with
    | App(_,l) -> l
    | _ -> assert false

let mk_proj i n a =
  hc(App(proj_sym i n,[a]))

let is_proj a =
  match a.node with
    | App({node=Interp(Tuple(Proj(_,_)))},[_]) -> true
    | _ -> false

let d_proj a =
  assert(is_proj a);
  match a.node with
    | App({node=Interp(Tuple(Proj(i,n)))},[a]) -> (i,n,a)
    | _ -> assert false
  


	  (*s Boolean function symbols, constructors, and recognizers. *)

let tt_sym = hc_sym(Interp(Bool(True)))
let ff_sym = hc_sym(Interp(Bool(False)))
let ite_sym = hc_sym(Interp(Bool(Ite)))

let mk_tt() = hc(App(tt_sym,[]))
let mk_ff() = hc(App(ff_sym,[]))
let mk_ite (a,b,c) = hc(App(ite_sym,[a;b;c]))

let is_tt a =
  match a.node with
    | App(f,[]) -> f === tt_sym
    | _ -> false

let is_ff a =
  match a.node with
    | App(f,[]) -> f === ff_sym
    | _ -> false

let is_ite a =
  match a.node with
    | App(f,[_;_;_]) -> f === ite_sym
    | _ -> false

let d_ite a =
  match a.node with
    | App(f,[a;b;c]) when f === ite_sym -> (a,b,c)
    | _ -> assert false

(*s Predicates. *)

let eq_sym = hc_sym(Pred(Equal))
let cnstrnt_sym c = hc_sym(Pred(Cnstrnt(c)))

let mk_equal (a,b) =
  hc(App(eq_sym,[a;b]))

let is_equal a =
  match a.node with
    | App(f,[_;_]) -> f === eq_sym
    | _ -> false

let d_equal a =
  assert(is_equal a);
  match a.node with
    | App(_,[a;b]) -> (a,b)
    | _ -> assert false

let mk_diseq (a,b) =
  mk_ite (mk_equal (a,b), mk_ff(), mk_tt())

let is_diseq a =
  is_ite a &&
  let (x,y,z) = d_ite a in
  is_equal x && is_ff y && is_tt z

let d_diseq a =
  assert(is_diseq a);
  let (x,_,_) = d_ite a in
  d_equal x

let mk_cnstrnt c a =
  hc(App(cnstrnt_sym c, [a]))

let is_cnstrnt a =
  match a.node with
    | App({node=Pred(Cnstrnt _)},[_]) -> true
    | _ -> false

let d_cnstrnt a =
  assert(is_cnstrnt a);
  match a.node with
    | App({node=Pred(Cnstrnt(c))},[x]) -> (c,x)
    | _ -> assert false

(*s Builtin functions. *)

let update_sym = hc_sym(Builtin(Update))


let mk_update (a,b,c) =
  hc(App(update_sym, [a;b;c]))

let is_update a =
  match a.node with
    | App(f,[_;_;_]) -> f === update_sym
    | _ -> false

let d_update a =
  assert(is_update a);
  match a.node with
    | App(_,[a;b;c]) -> (a,b,c)
    | _ -> assert false

(*s Get domain of interpretation. *)

let domain_of a =
  match a.node with
    | Var(_,Ext(Some(d))) -> Some(d)
    | Var(_,Fresh(d)) -> Some(d)
    | App({node=Uninterp(_,Some(d),_)},_) -> Some(d)
    | _ -> None

(*s Classify function symbols. *)

type theories =
  | ArithTh
  | TupleTh
  | BooleanTh
  | EqTh

let theory_of f =
  match f.node with
    | Interp(x) ->
	(match x with
	  | Arith _ -> ArithTh
	  | Tuple _ -> TupleTh
	  | Bool _ -> BooleanTh)
    | _ ->
	EqTh



	       (*s [is_ground a] tests if [a] contains neither a variable n
		 nor an application of an uninterpreted function symbol. *)

let rec is_ground a = 
  match a.node with
    | Var _ -> 
	false
    | App(f,l) -> 
	(match f.node with
	   | Uninterp _ ->
	       false
	   | Builtin _ -> 
	       false
	   | Interp _ | Pred _ ->
	       List.for_all is_ground l)

let is_uninterpreted a =
  match a.node with
    | Var _ -> 
	true
    | App(f,l) ->
	(match f.node with
	   | Uninterp _ | Builtin _ | Pred _ -> 
	       true
	   | Interp _ ->
	       false)


(*s Mapping over list of terms. Avoids unnecessary consing. *)

let rec mapl f l =
  match l with
    | [] -> []
    | a :: l1 ->
	let a' = f a and l1' = mapl f l1 in
	if a' === a && l1 == l1' then l else a' :: l1'
	

(*s Homomorphisms on terms. [f] is applied to arguments [bi],
    if [bi] equals [f(bi)] for all [i], then the original term [a]
    is returned, otherwise a new term is constructed using [op]. *)

let hom1 a op f b =
  let b' = f b in
  if b' === b then a else op b'

let hom2 a op f (b1,b2) =
  let b1' = f b1 and b2' = f b2 in
  if b1' === b1 && b2' === b2 then a else op(b1',b2')

let hom3 a op f (b1,b2,b3) =
  let b1' = f b1 and b2' = f b2 and b3' = f b3 in
  if b1' === b1 && b2' === b2 && b3' === b3 then a else op(b1',b2',b3')

let homl a op f l =
  let l' = mapl f l in
  if eql l l' then a else op l'

(*s Association lists for terms. *)
    
let assq = List.assq

(*s Fold operator over terms. *)

let rec fold f a acc =
  match a.node with
    | Var _ ->
	f a acc
    | App(_,l) ->
	f a (List.fold_right f l acc)

(*s Is [a] a subterm of [b]. *)

let is_subterm a b =
  let rec occ b =
    a === b || 
       match b.node with 
	 | Var _ -> false 
	 | App(f,l) ->
	     (match f.node with
		| Uninterp(op,_,_) -> 
		    occ op || List.exists occ l
		| _ -> 
		    List.exists occ l)
  in 
  occ b

(*s Iteration over terms. *)

let rec iter f a  =
  f a;
  match a.node with
    | Var _ -> ()
    | App(_,l) ->
	List.iter (iter f) l

	(*s Sets of terms *)

type term = t
type ts = tnode Ptset.t
	
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

let rec vars_of a =
  match a.node with
    | Var _ -> 
	Set.singleton a
    | App(f,l) ->
	Set.union (vars_of_funsym f) (vars_of_args l)

and vars_of_funsym f =
  match f.node with
    | Uninterp(x,_,_) -> 
	vars_of x
    | _ ->
	Set.empty

and vars_of_args l =
  List.fold_right (fun x -> Set.union (vars_of x)) l Set.empty
  

let rec freshvars_of a =
  match a.node with
    | Var(_, Rename _) ->
	Set.singleton a
    | Var _ -> 
	Set.empty
    | App(f,l) ->
	Set.union (freshvars_of_funsym f) (freshvars_of_args l)

and freshvars_of_funsym f =
  match f.node with
    | Uninterp(x,_,_) -> 
	freshvars_of x
    | _ ->
	Set.empty

and freshvars_of_args l =
  List.fold_right (fun x -> Set.union (freshvars_of x)) l Set.empty


(*s Finite maps with terms as domain *)

exception MFound of term * term

module Map = struct
  type 'a t = (tnode,'a) Ptmap.t
	      
  let empty = Ptmap.empty
  let is_empty m = Ptmap.fold (fun _ _ _ -> false) m true
  let add = Ptmap.add
  let mem = Ptmap.mem
  let find = Ptmap.find
  let remove = Ptmap.remove
  let map = Ptmap.map
  let iter = Ptmap.iter
  let fold = Ptmap.fold

  let update x a m =
    Ptmap.add x a (Ptmap.remove x m)
	     
  let choose p m =
    try
      iter (fun a b -> if p a b then raise (MFound (a,b))) m;
      raise Not_found
    with
	MFound (a,b) -> (a,b)
		     
  let to_list m =
    fold (fun x y acc -> (x,y) :: acc) m []
	
end

