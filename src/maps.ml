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

module type OrderedType = sig
  type t
  val compare: t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type key
  type 'a t
  val empty: unit -> 'a t
  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a
  val set : key -> 'a -> 'a t -> unit
  val remove : key -> 'a t -> unit
  val copy : 'a t -> 'a t
  val mem :  key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b  
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val to_list : 'a t -> (key * 'a) list
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Balanced(Ord: OrderedType) = struct

  module M = Map.Make(Ord)

  type key = Ord.t

  type 'a t = { mutable root : 'a M.t }
   
  let empty () = { root = M.empty }

  let is_empty m = (m.root == M.empty)

  let find x m = M.find x m.root

  let set x a m =
    m.root <- M.add x a m.root

  let remove x m = 
    m.root <- M.remove x m.root

  let copy m = { root = m.root }

  let mem x m = M.mem x m.root

  let iter f m = M.iter f m.root

  let fold f m = M.fold f m.root

  exception False
  let for_all p m =
    let p' x a = if not(p x a) then raise False in
    try
      M.iter p' m.root;
      true
    with
	False -> false

  let to_list m =
    let cons x a l = (x, a) :: l in
      fold cons m []

  let pp p fmt m =
    let binding (x, a) = 
      Format.fprintf fmt "@["; 
      Ord.pp fmt x;
      Format.fprintf fmt " |-> ";
      p fmt a; 
      Format.fprintf fmt "@]"
    in  
    let rec bindings = function
      | [] -> ()
      | [bnd] -> binding bnd
      | bnd :: bnds -> binding bnd; Format.fprintf fmt ", "; bindings bnds
    in
      Format.fprintf fmt "[";
      bindings (to_list m);
      Format.fprintf fmt "]@;"

end


module Make = Balanced



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
      mutable key : Ord.t;
      mutable value : 'a;
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
	  
    let create l k v r =
      let n = Node{
	left = l; 
	key = k; 
	value = v; 
	right = r; 
	refcount = 0} 
      in
	incr l; incr r;
	Gc.finalise finalise n;
	n
	  
    let rec max_elt = function
      | Node(n) -> if n.right == Empty then n.key else max_elt n.right
      | Empty -> raise Not_found
	  
    let rec min_elt = function
      | Node(n) -> if n.left == Empty then n.key else min_elt n.left
      | Empty -> raise Not_found     
	  
    let iter f =
      let rec iterf = function
	| Empty -> ()
	| Node(n) -> iterf n.left; f n.key n.value; iterf n.right
      in
	iterf
	  
    let fold f t e =
      let acc = ref e in
      let g i v = acc := f i v !acc in
	iter g t; !acc
	  
    let for_all p =
      let rec for_all_p = function
	| Empty -> true
	| Node(x) -> p x.key && for_all_p x.left && for_all_p x.right
      in
	for_all_p
	  
    let exists p = 
      let rec exists_p = function
	| Empty -> false
	| Node(n) -> p n.key || exists_p n.left || exists_p n.right
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
	  let less i = Ord.compare i n.key < 0
	  and greater j = Ord.compare n.key j < 0 in
	    for_all less n.left && for_all greater n.right
	      
    let rec mem i = function
      | Empty -> false
      | Node(n) -> 
	  let cmp = Ord.compare i n.key in
	    cmp == 0 || mem i (if cmp < 0 then n.left else n.right)
	      
    let find i =
      let rec findi = function
	| Empty -> raise Not_found
	| Node(n) -> 
	    let cmp = Ord.compare i n.key in
	      if cmp == 0 then n.value 
	      else if cmp < 0 then findi n.left
	      else findi n.right
      in
	findi 
	  
    let to_list t =
      assert(is_ordered t);
      let rec elts acc = function
	| Empty -> acc
	| Node(s) -> elts ((s.key, s.value) :: (elts acc s.right)) s.left
      in
	elts [] t
	  
    let debug = true
		  
    let rec pp p fmt = function
      | Empty -> 
	  Format.fprintf fmt "nil"
      | Node n ->
	  Format.fprintf fmt "<";
	  pp p fmt n.left;
	  Format.fprintf fmt ",";
	  Ord.pp fmt n.key;
	  Format.fprintf fmt "|->";
	  p fmt n.value;
	  Format.fprintf fmt ",";
	  pp p fmt n.right;
	  Format.fprintf fmt ">";
	  if debug then Format.fprintf fmt "[%d]" n.refcount
	    
    (** Reorganize the splay tree [x] so that element [i] is at the root if [mem i x]. 
      - {i Empty}. The splay of [Empty] is Empty.   
      - {i Base}. If [x] has a parent [p] but no grandparent, we just [rotate x p]. *)
    let rec splay i = function
      | Empty -> ()
      | Node(p) -> splay_node i p 
	  
    and splay_node i p =
      let cmp = Ord.compare i p.key in
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
	    let a = nl.left and j = nl.key and v = nl.value and b = nl.right in
	    let n = (* reuse node [l] if only used once. *)
	      if nl.refcount = 1 then 
		(recycle nl b p.key p.value p.right; p.left)
	      else 
		create b p.key p.value p.right
	    in
	      recycle p a j v n
		
    and recycle n l k v r =
      assert(n.refcount = 1);
      incr l;        (* increment first before decrementing. *)
      incr r;        (* to maintain invariants. *)
      decr n.left;
      decr n.right;
      n.left <- l;
      n.key <- k;
      n.value <- v;
      n.right <- r
	
    (** Left rotation: [p<l,k,r<b,j,c>]] ==> [p<n<l,k,b>,j,c>] *) 
    and lrotate p = 
      match p.right with
	| Empty -> ()
	| Node(nr) -> 
	    let b = nr.left and j = nr.key and v = nr.value and  c = nr.right in
	    let n =   (* reuse node [r] if only used once. *)
	      if nr.refcount = 1 then
		(recycle nr p.left p.key p.value b; p.right)
	      else 
		create p.left p.key p.value b
	    in
	      recycle p n j v c
		
    let empty = Empty

    let is_empty = function
      | Empty -> true
      | _ -> false

    let add k v n = 
      match n with
	| Empty -> 
	    create Empty k v Empty
	| Node(nn) -> 
	    splay_node k nn;
	    let cmp = Ord.compare k nn.key in
	      if cmp = 0 then
		if nn.refcount = 1 then
		  (nn.value <- v; n)
		else 
		  create nn.left k v nn.right
	      else if cmp > 0 then  (* [k > nn.key] *)
		if is_empty nn.right then
		  create n k v Empty
		else         (* to do: recycle node [n]. *)
		  let m = create nn.left nn.key nn.value Empty in
		    create m k v nn.right
	      else                  (* [k < nn.key] *)
		let m = create nn.left k v Empty in
		  create m nn.key nn.value nn.left

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
	      create n mm.key mm.value mm.right

    let remove i n =   (* refcounting not quite right yet.*)
      match n with
	| Empty -> n
	| Node(nn) ->
	    splay_node i nn;
	    if Ord.compare i nn.key <> 0 then n else
	      join nn.left nn.right
	    
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

  let find i s = 
    Tree.find i s.root
    
  let set i v s =
    assert(Tree.is_ordered s.root);
    if not(mem i s) then 
      let r' = Tree.add i v s.root in
	Tree.incr r';
	Tree.decr s.root;
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
  let pp p fmt s = Tree.pp p fmt s.root

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
	
end


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

