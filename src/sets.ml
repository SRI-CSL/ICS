(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
    val pp : Format.formatter -> t -> unit
  end

module type S =
  sig
    type elt
    type t
    val empty: unit -> t  
    val singleton: elt -> t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val copy: t -> t
    val add: elt -> t -> unit
    val union : t -> t -> unit
    val remove: elt -> t -> unit
    val replace : elt -> elt -> t -> unit
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val cardinal: t -> int
    val to_list: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val choose_if : (elt -> bool) -> t -> elt
    val pp : Format.formatter -> t -> unit
  end

let debug = true

module Balanced(Ord: OrderedType): (S with type elt = Ord.t) = struct
  type elt = Ord.t

  module S = Set.Make(Ord)

  type t = { mutable elts : S.t }

  let empty () = { elts = S.empty }
  let singleton i = { elts = S.singleton i } 
  let copy s = { elts = s.elts }
  let is_empty s = S.is_empty s.elts
  let mem i s = S.mem i s.elts
  let add i s = s.elts <- S.add i s.elts
  let remove i s = s.elts <- S.remove i s.elts
  let replace x y s = remove x s; add y s
  let union s t = t.elts <- S.union s.elts t.elts
  let compare s t = S.compare s.elts t.elts
  let subset s t = S.subset s.elts t.elts
  let iter f s = S.iter f s.elts
  let fold f s = S.fold f s.elts
  let for_all p s = S.for_all p s.elts
  let exists p s = S.exists p s.elts
  let cardinal s = S.cardinal s.elts
  let to_list s = S.elements s.elts
  let min_elt s = S.min_elt s.elts
  let max_elt s = S.max_elt s.elts
  let choose s = S.choose s.elts

  exception Found
  let choose_if p s =
    let elt = ref (Obj.magic 0) in
      try
	S.iter 
	  (fun x -> 
	     if p x then 
	       (elt := x; raise Found))
	  s.elts;
	raise Not_found
      with
	  Found -> !elt

  let pp fmt s =
    let pp_elt x =
      Ord.pp fmt x
    in
    let rec pp_list = function
      | [] ->()
      | [x] -> pp_elt x
      | x :: xl -> pp_elt x; Format.fprintf fmt ", "; pp_list xl
    in
      Format.fprintf fmt "@[{";
      pp_list (to_list s);
      Format.fprintf fmt "}@];"

end



(** Implementation of finite sets of totally ordered elements in terms
  of Splay trees. These are ordinary binary search trees, which 
  rearranges its nodes {i on-the-fly}. *)
module Splay(Ord: OrderedType) = struct

  module Tree = struct

    (** Data items will always be maintained inorder, that is;
      for any node [x], the elements occupying the left subtree
      of [x] are all less than [x], and thus occupying the right 
      subtree of [x] are all greater than [x]. *)
    type 'a t = 
      | Empty
      | Node of 'a node
	  
    and 'a node = {
      mutable left : 'a t; 
      mutable elt : Ord.t;
      mutable right : 'a t;
      mutable refcount : int;
    }
	
    let incr = function
      | Node(n) ->
	  assert(n.refcount >= 0);
	  n.refcount <- n.refcount + 1
      | Empty -> 
	  ()
	  
    let decr = function
      | Node(n) ->   
	  assert(n.refcount >= 1);
	  n.refcount <- n.refcount - 1
      | Empty -> 
	  ()
	  
    let finalise = function
      | Node(n) -> 
	  assert(n.refcount = 0);
	  decr n.left; decr n.right  
      | Empty -> 
	  ()
	  
    let create l k r =
      let n = Node{
	left = l; 
	elt = k; 
	right = r; 
	refcount = 0} 
      in
	incr l; incr r;
	Gc.finalise finalise n;
	n
	  
    let rec max_elt = function
      | Node(n) -> if n.right == Empty then n.elt else max_elt n.right
      | Empty -> raise Not_found
	  
    let rec min_elt = function
      | Node(n) -> if n.left == Empty then n.elt else min_elt n.left
      | Empty -> raise Not_found     
	  
    let iter f =
      let rec iterf = function
	| Empty -> ()
	| Node(n) -> iterf n.left; f n.elt; iterf n.right
      in
	iterf
	  
    let fold f t e =
      let acc = ref e in
      let g i v = acc := f i v !acc in
	iter g t; !acc
	  
    let for_all p =
      let rec for_all_p = function
	| Empty -> true
	| Node(x) -> p x.elt && for_all_p x.left && for_all_p x.right
      in
	for_all_p
	  
    let exists p = 
      let rec exists_p = function
	| Empty -> false
	| Node(n) -> p n.elt || exists_p n.left || exists_p n.right
      in
	exists_p

    let (<<) p q = 
      Ord.compare (max_elt p ) (min_elt q) < 0
      
	  
    let cardinal s =
      let acc = ref 0 in
      let rec count = function
	| Empty -> ()
	| Node(n) -> acc := !acc + 1; count n.left; count n.right
      in
	count s; 
	!acc
	  
    let is_ordered = function
      | Empty -> true
      | Node(n) -> 
	  let less i = Ord.compare i n.elt < 0
	  and greater j = Ord.compare n.elt j < 0 in
	    for_all less n.left && for_all greater n.right
	      
    let rec mem i = function
      | Empty -> false
      | Node(n) -> 
	  let cmp = Ord.compare i n.elt in
	    cmp == 0 || mem i (if cmp < 0 then n.left else n.right)
	  
    let to_list t =
      assert(is_ordered t);
      let rec elts acc = function
	| Empty -> acc
	| Node(s) -> elts (s.elt :: (elts acc s.right)) s.left
      in
	elts [] t
		  
    let rec pp fmt t = 
      if debug then pp_debug fmt t else pp_debug fmt t
	(* Pretty.set Ord.pp fmt (to_list t) *)
	  
    and pp_debug fmt = function
      | Empty -> 
	  Format.fprintf fmt "nil"
      | Node n ->
	  Format.fprintf fmt "@[(";
	  pp_debug fmt n.left;
	  Format.fprintf fmt ",@ ";
	  Ord.pp fmt n.elt;
	  Format.fprintf fmt ",";
	  pp_debug fmt n.right;
	  Format.fprintf fmt ")[%d]@]" n.refcount
	    
    (** Reorganize the splay tree [x] so that element [i] is at the root if [mem i x]. 
      - {i Empty}. The splay of [Empty] is Empty.   
      - {i Base}. If [x] has a parent [p] but no grandparent, we just [rotate x p]. *)
    let rec splay i = function
      | Empty -> ()
      | Node(p) -> splay_node i p 
	  
    and splay_node i p =
      let cmp = Ord.compare i p.elt in
	if cmp < 0 then
	  (splay i p.left; rrotate p)
	else if cmp > 0 then
	  (splay i p.right; lrotate p)
	  
    (** Right rotation: [p<l<a,j,v,b>,k,w,r>] ==> [p<a,j,v,n<b,k,w,r>>]. 
      Notice: ther is a possible optimization by canceling redundant incr 
      and decr of reference counters. *)      
    and rrotate p = 
      match p.left with 
	| Empty -> ()
	| Node(nl) -> 
	    let a = nl.left and j = nl.elt and b = nl.right in
	    let n = (* reuse node [l] if only used once. *)
	      if nl.refcount = 1 then 
		(recycle nl b p.elt p.right; p.left)
	      else 
		create b p.elt p.right
	    in
	      recycle p a j n
		
    and recycle n l k r =
      assert(n.refcount = 1);
      incr l;        (* increment first before decrementing. *)
      incr r;        (* to maintain invariants. *)
      decr n.left;
      decr n.right;
      n.left <- l;
      n.elt <- k;
      n.right <- r
	
    (** Left rotation: [p<l,k,r<b,j,c>]] ==> [p<n<l,k,b>,j,c>] *) 
    and lrotate p = 
      match p.right with
	| Empty -> ()
	| Node(nr) -> 
	    let b = nr.left and j = nr.elt and  c = nr.right in
	    let n =   (* reuse node [r] if only used once. *)
	      if nr.refcount = 1 then
		(recycle nr p.left p.elt b; p.right)
	      else 
		create p.left p.elt b
	    in
	      recycle p n j c
		
    let empty = Empty

    let is_empty = function
      | Empty -> true
      | _ -> false

    let add k n = 
      match n with
	| Empty -> 
	    create Empty k Empty
	| Node(nn) -> 
	    splay_node k nn;
	    let cmp = Ord.compare k nn.elt in
	      if cmp = 0 then 
		n
	      else if cmp > 0 then  (* [k > nn.elt] *)
		if is_empty nn.right then
		  create n k Empty
		else         (* to do: recycle node [n]. *)
		  let m = create nn.left nn.elt Empty in
		    create m k nn.right
	      else                  (* [k < nn.elt] *)
		let m = create nn.left k Empty in
		  create m nn.elt nn.left

    let join n m =
      match n, m with
	| Empty, Empty -> Empty
	| Empty, Node _ -> m
	| Node _, Empty -> n
	| Node _, Node(mm) ->  
	    assert(n << m);
	    splay_node (min_elt m) mm;
	    assert(is_empty mm.left);
	    if mm.refcount = 1 then
	      (mm.left <- n; incr n; m)
	    else 
	      create n mm.elt mm.right

    let union n m =
      if n << m then join n m else 
	if m << n then join m n else
	  (* fold add n m *)
	  failwith "To do"

    let remove i n =   (* refcounting not quite right yet.*)
      match n with
	| Empty -> n
	| Node(nn) ->
	    splay_node i nn;
	    if Ord.compare i nn.elt <> 0 then n else
	      join nn.left nn.right

    let replace i j n =
      if mem i n then add j (remove i n) else add j n
	    
  end

  type 'a t = { mutable root : 'a Tree.t }
    
  let mem i s = 
    Tree.mem i s.root
      
  let empty () =
    { root = Tree.Empty }
    
  let is_empty s = 
    (s.root == Tree.Empty)

  let finalise s =
    Tree.decr s.root
      
  let copy s = 
    assert(Tree.is_ordered s.root);
    Tree.incr s.root;
    let s' =  {root = s.root} in
      Gc.finalise finalise s';
      s'
    
  let add i s =
    assert(Tree.is_ordered s.root);
    if not(mem i s) then 
      let r' = Tree.add i s.root in
	Tree.incr r';
	Tree.decr s.root;
	s.root <- r'

  let union s t = 
    assert(Tree.is_ordered s.root);
      let r' = Tree.union s.root t.root in
	Tree.incr r';
	Tree.decr t.root;
	s.root <- r'

  let remove i s =
    assert(Tree.is_ordered s.root);
    if mem i s then
      let r' = Tree.remove i s.root in
	Tree.incr r';
	Tree.decr s.root;
	s.root <- r'

  let to_list s = Tree.to_list s.root
  let exists p s = Tree.exists p s.root
  let for_all p s = Tree.for_all p s.root
  let fold f s = Tree.fold f s.root
  let iter f s = Tree.iter f s.root
  let pp fmt s = Tree.pp fmt s.root

  exception False
  let for_all p s =
    let p' x a = if not(p x a) then raise False in
    try
      Tree.iter p' s.root;
      true
    with
	False -> false

  let cardinal s =
    let acc = ref 0 in
    let rec count = function
      | Tree.Empty -> ()
      | Tree.Node(x) -> 
	  incr acc; 
	  count x.Tree.left; 
	  count x.Tree.right
    in
      count s.root; !acc	

  let choose s =
    match s with
      | Tree.Empty -> raise Not_found
      | Tree.Node(n) -> n.Tree.elt

  exception Found
  let choose_if p s =
    let elt = ref (Obj.magic 0) in
      try
	Tree.iter 
	  (fun x -> 
	     if p x then 
	       (elt := x; raise Found))
	  s;
	raise Not_found
      with
	  Found -> !elt
	
end


module Make = Balanced

(*
(** test. *)
module Ints = Splay(
  struct type t = int
    let compare = Pervasives.compare
    let pp fmt i = Format.fprintf fmt "%d" i
  end);;

open Ints;;

let pps = pp Format.std_formatter;;
#install_printer pps;;
*)
