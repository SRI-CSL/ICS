
(*i*)
open Hashcons
(*i*)

(*s A concatenation normal form is simply an n-ary concatenation
    of basic bitvectors. Hereby, a basic bitvector is either a
    constant, an extraction, or a bitwise conditional operator.
  *)

type basic_node =
  | Const of Bitv.t
  | Sub of Term.term * int * int * int
  | Ite of basic * basic * basic

and basic = basic_node hashed

type t = basic list                   (* concatenation normal form *)

(*s Pretty printing *)

let rec pp_basic b = match b.node with
  | Const(c) ->
      Format.print_string (Bitv.to_string c)
  | Sub(x,n,i,j) ->
      Pretty.term x; Format.printf "[";
      Format.print_int n; Format.printf ","; 
      Format.print_int i; Format.printf ","; 
      Format.print_int j;
      Format.print_string "]"
  | Ite(b1,b2,b3) ->
     Format.printf "bvite ";
      pp_basic b1;
      Format.printf " then ";
      pp_basic b2;
      Format.printf " then ";
      pp_basic b3;
      Format.printf " end"
      
let pp =
  Pretty.list_sep (fun () -> Format.printf " ++ ") pp_basic
       	
(* Length of concatenation normal forms *)
		
let rec lengthb b = 
  match b.node with
    | Const c -> Bitv.length c
    | Sub(_,_,i,j) -> j-i+1
    | Ite(b1,_,_) -> lengthb b1
	
let length bl =
  List.fold_left (fun acc b -> lengthb b + acc) 0 bl

(* Hashconsing constructors *)

module HashBasic = Hashcons.Make(
  struct 
    type t = basic_node
    let equal t1 t2 =
      match t1, t2 with
	| Const c, Const d ->
	    compare c d = 0
	| Sub(t1,n1,l1,u1), Sub(t2,n2,l2,u2)  ->
	    t1 == t2 && n1 = n2 && l1 = l2 && u1 = u2			     
	| Ite(x1,y1,z1), Ite(x2,y2,z2) ->
	    x1 == x2 && y1 == y2 && z1 == z2
	| _ -> false
    let hash = Hashtbl.hash
  end)

let hc_basic : basic_node -> basic =
  let ht = HashBasic.create 251 in
  Tools.add_at_exit (fun () -> Format.print_string "Basic bv  : "; HashBasic.stat ht);
  Tools.add_at_reset (fun () -> HashBasic.clear ht);
  HashBasic.hashcons ht

let mk_const c = hc_basic (Const c)
let mk_eps () = mk_const (Bitv.from_string "")
let mk_sub x n i j =
  hc_basic (Sub (x,n,i,j))

let eq_basic = (==)
	
(* Test if a basic bitvector occurs in a concatenation normal form *)

let occursb y b =
  let rec occ b =
    match b.node with
      | Const _ -> false
      | Sub(x,_,_,_) -> y == x
      | Ite(b1,b2,b3) -> occ b1 || occ b2 || occ b3
  in
  occ b

let occurs y = List.exists (occursb y)
  	
(*s Bitvector Bdds are a BDD-like structure with basics as
    conditions in if-then-else structures
  *)  
	
module Bvbdd = Bdd.Make(
  struct
    type bdd_node = basic_node
    type bdd = basic
    type tag = int
    let compare = compare  
    let high n = hc_basic (Const (Bitv.create n true))
    let low n = hc_basic (Const (Bitv.create n false))
    let ite _ x y z = hc_basic (Ite (x,y,z))
    let is_high = function {node=Const b} -> Bitv.all_ones b | _ -> false 
    let is_low = function {node=Const b} -> Bitv.all_zeros b | _ -> false
    let is_ite = function {node=Ite _} -> true | _ -> false 
    let destructure_ite = function {node=Ite(x,y,z)} -> Some(x,y,z) | _ -> None 
    let fresh n = mk_sub (Var.fresh ("b",None,None) []) n 0 (n-1)
  end)

let mk_apply b1 b2 b3 =
  assert (lengthb b1 = lengthb b2 && lengthb b2 = lengthb b3);
  match b1.node, b2.node, b3.node with
    | Const c, Const d, Const e ->
        mk_const (Bitv.bw_or (Bitv.bw_and c d) (Bitv.bw_and (Bitv.bw_not c) e))
    | _ -> Bvbdd.build (lengthb b1) (b1,b2,b3)	 

	  
(*s Constants *)	  
		
let eps = []
let atom b = [b]
let const c = atom (mk_const c)
let zero n = const (Bitv.create n false)
let one n  = const (Bitv.create n true)
      
(*s A Bitvector variable [x] of length [n] is represented by an
    extraction [mk_sub(x,0,n-1)]. *)

let inj n x =
  atom (mk_sub x n 0 (n-1))

let fresh n =
  inj n (Var.fresh ("b",None,None) [])

  
			   
(*s Homomorphismus on concatenation normal form *)

let hom f g h bl =
  let rec homb b =
    match b.node with
      | Const c -> f c
      | Sub(x,n,i,j) -> g x n i j
      | Ite(b1,b2,b3) -> h (lengthb b1) (homb b1) (homb b2) (homb b3)
  in
  List.map homb bl
	  
(*s Concatenation. Builds right-associative concatenations, eliminates bitvectors
    of length [0], combines bitvector const_basicants and terms of the form [x[i,j]]
    and [x[j+1,k]], and the like... *)

let add b bl =
  if lengthb b = 0 then bl
  else match b.node, bl with
    | _, [b2] when lengthb b2 = 0 -> [b]
    | _, [] -> [b]
    | Const c, {node=Const d} :: bl' ->
	(mk_const (Bitv.append c d)) :: bl'
    | Sub(x,n,i,j), {node=Sub(x',n',i',j')} :: bl'
	when x == x' && i' = j+1 ->
	  assert (n = n');
	  (mk_sub x n i j') :: bl'
    | _ -> b :: bl

let (++) l1 l2 =
  let rec loop acc = function
    | [] -> acc 
    | b1 :: l1' -> loop (add b1 acc) l1'
  in
  loop l2 (List.rev l1)
	
let conc = List.fold_left (++) []
 
(*s Constructors for extraction *)
  

let subb b i j =
  assert (0 <= i && i <= j && j < lengthb b);
  let rec loop b i j =
    if i = 0 && j = pred(lengthb b) then b
    else match b.node with
      | Const c ->
	  mk_const (Bitv.sub c i (j-i+1))
      | Sub (x,n,k,_) ->
	  mk_sub x n (k+i) (k+j)
      | Ite (b1,b2,b3) ->
	  mk_apply (loop b1 i j) (loop b2 i j) (loop b3 i j)
  in
  loop b i j
  
  
let sub l i j =
  assert (0 <= i && i <= j && j < length l);
  let rec loop l i j =
  match l with
    | [] -> []
    | b :: bl ->
	let n = lengthb b in
	if j < n then
	  atom (subb b i j)
	else if i >= n then
	  loop bl (i-n) (j-n)
	else (* i <= n *)
	  add (subb b i (n-1)) (loop bl 0 (j-n))
  in
  loop l i j


(*s Constructor for bitwise operations on basic bitvectors *)

let rec ite l1 l2 l3 =
  assert (length l1 = length l2 && length l2 = length l3);
  match l1, l2, l3 with
    | [], [], [] -> []
    | b1 :: _, b2 :: _, b3 :: _ ->
	let m = min (lengthb b1) (min (lengthb b2) (lengthb b3)) in
	let (x1,l1') = cut m l1 in
	let (x2,l2') = cut m l2 in
	let (x3,l3') = cut m l3 in
	add (mk_apply x1 x2 x3) (ite l1' l2' l3')
    | _ -> assert false

and cut n = function
  | [] ->
      assert (n = 0);
      (mk_eps(),[])
  | b :: bl ->
      let (b1,b2) = cutb n b in
      (b1, add b2 bl)

and cutb n b =
  let m = lengthb b in
  assert (not (n>m));
  if n = m then
    (b, mk_eps())
  else (* n < m *)
    (subb b 0 (n-1), subb b n (m-1))

  
(* n-ary concatenation of some concatenation normal form *)

let rec iterate bl =         
  function 0 -> [] | n -> bl ++ iterate bl (n-1)
  
(*s Solving Bitvector equations. *)

let rec solve l1 l2 =
  assert (length l1 = length l2);
  match l1, l2 with
    | [], [] -> []
    | b1 :: l1', b2 :: l2' ->
	let n1 = lengthb b1 in
	let n2 = lengthb b2 in
	if n1 = n2 then
	  (solveb b1 b2) @ (solve l1' l2')
	else if n1 < n2 then
	  let (b21,b22) = cutb n1 b2 in
	  (solveb b1 b21) @ (solve l1' (add b22 l2'))
	else
	  let (b11,b12) = cutb n2 b1 in
	  (solveb b11 b2) @ (solve (add b12 l1') l2')
    | _ -> assert false

and solveb b1 b2 =
  assert (lengthb b1 = lengthb b2);
  if eq_basic b1 b2 then [] else
  match b1.node, b2.node with
    | Const c, Const d ->
	if compare c d = 0 then [] else raise (Exc.Inconsistent "Bitvector solver")
    | Sub(x,n,i,j), Sub(y,m,k,l) when x == y ->
	assert(n = m);
        if i < k then
           solve_extr x n i j k l
	else if i > k then
	  solve_extr x n k l i j
	else
	  assert false 
    | Sub(t,n,i,j), _ ->
	let a1 = fresh i in
	let a2 = fresh (n-j-1) in
	[t, a1 ++ atom b2 ++ a2]
    | _, Sub(t,n,i,j) ->
	let a1 = fresh i in
	let a2 = fresh (n-j-1) in
	[t, a1 ++ atom b1 ++ a2]
    | Ite _, _
    | _, Ite _ ->
	solve_ite b1 b2

and solve_extr x n i j k l =
  assert (i < k && j-i = l-k);
  let lhs = sub (inj n x) i l in
  let rhs =
    if (l-i+1) mod (k-i) = 0 then
      let a = fresh (k-i) in
      iterate a ((l-i+1)/(k-i))
    else
      let a = fresh ((l-i+1) mod (k-i)) in
      let b = fresh ((i-l-1) mod (k-i)) in
      a ++ iterate (b ++ a) ((l-i+1) / (k-i))
  in
  solve lhs rhs
     
and solve_ite b1 b2 =
  let n = lengthb b1 in
  let bbl = Bvbdd.solve n (Bvbdd.iff n b1 b2) in
  List.fold_right (fun (b1,b2) acc -> solveb b1 b2 @ acc) bbl []

