(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val empty : unit -> t
  val singleton : elt -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val copy : t -> t
  val add : elt -> t -> unit
  val union : t -> t -> unit
  val remove : elt -> t -> unit
  val replace : elt -> elt -> t -> unit
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val cardinal : t -> int
  val to_list : t -> elt list
  val of_list : elt list -> t
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val choose_if : (elt -> bool) -> t -> elt
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Balanced (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  module S = Set.Make (Ord)

  type t = {mutable root: S.t}

  let empty () = {root= S.empty}
  let singleton k = {root= S.add k S.empty}
  let is_empty s = s.root == S.empty
  let mem k s = S.mem k s.root
  let copy s = {root= s.root}
  let add k s = s.root <- S.add k s.root
  let union s1 s2 = s2.root <- S.union s1.root s2.root
  let remove k s = s.root <- S.remove k s.root

  let replace k1 k2 s =
    if mem k1 s then s.root <- S.add k2 (S.remove k1 s.root)

  let subset s1 s2 = S.subset s1.root s2.root
  let iter f s = S.iter f s.root
  let fold f s = S.fold f s.root
  let for_all f s = S.for_all f s.root
  let exists f s = S.exists f s.root
  let cardinal s = S.cardinal s.root
  let to_list s = S.elements s.root

  let of_list l =
    let acc = empty () in
    let add1 k = add k acc in
    List.iter add1 l ;
    acc

  let min_elt s = S.min_elt s.root
  let max_elt s = S.max_elt s.root
  let choose s = S.choose s.root

  exception Witness

  let choose_if p s =
    let found = ref (Obj.magic 0) in
    let check k =
      if p k then (
        found := k ;
        raise Witness )
    in
    try
      iter check s ;
      raise Not_found
    with Witness -> !found

  let pp fmt s =
    let rec print = function
      | [] -> ()
      | [k] -> Ord.pp fmt k
      | k :: kl ->
          Ord.pp fmt k ;
          Format.fprintf fmt ", " ;
          print kl
    in
    Format.fprintf fmt "{" ;
    print (to_list s) ;
    Format.fprintf fmt "}"

  let equal s1 s2 = S.equal s1.root s2.root
  let debug = ref false
  let well_formed _ = true
end

(* silence unused warning *)
let () =
  let open Balanced (Int) in
  ()

module Splay (Ord : OrderedType) = struct
  type elt = Ord.t

  let debug = ref false
  let reuse = true (* reuse still buggy *)

  (** Implementation of finite sets of totally ordered elements in terms of
      Splay trees. These are ordinary binary search trees, which rearranges
      its nodes {i on-the-fly}. *)
  module Tree = struct
    (** A set is encoded by means of binary trees with [left] and [right]
        children. Data items [elt] are maintained inorder, that is; for any
        node [x], the elements occupying the left subtree of [x] are all
        less than [x], and thus occupying the right subtree of [x] are all
        greater than [x]. The {i reference counting} slot [refcount] keeps
        track of the number of objects pointing to the current node. Nodes
        with reference count [1] are updated destructively. *)
    type t =
      { mutable left: t
      ; mutable elt: elt
      ; mutable right: t
      ; mutable refcount: int }

    let empty : t = Obj.magic None
    let is_empty n = n == empty
    let is_node s = not (s == empty)

    let d_singleton n =
      if (not (is_empty n)) && is_empty n.left && is_empty n.right then
        n.elt
      else raise Not_found

    let refcount n = if is_empty n then -1 else n.refcount

    let refcount_eq n m =
      match (is_empty n, is_empty m) with
      | false, false -> n.refcount = m.refcount
      | _ -> true

    let to_list n =
      let rec elements acc n =
        if is_empty n then acc
        else elements (n.elt :: elements acc n.right) n.left
      in
      elements [] n

    let rec pp fmt s = if !debug then pp_debug fmt s else pp_set fmt s

    and pp_set fmt s =
      let sep () = Format.fprintf fmt ", " in
      let rec iterate = function
        | [] -> ()
        | [x] -> Ord.pp fmt x
        | x :: xl ->
            Ord.pp fmt x ;
            sep () ;
            iterate xl
      in
      Format.fprintf fmt "@[{" ;
      iterate (to_list s) ;
      Format.fprintf fmt "}@]@?"

    and pp_debug fmt n =
      if is_empty n then Format.fprintf fmt "nil"
      else (
        Format.fprintf fmt "@[(" ;
        pp fmt n.left ;
        Format.fprintf fmt ",@ " ;
        Ord.pp fmt n.elt ;
        Format.fprintf fmt ",@ " ;
        pp fmt n.right ;
        Format.fprintf fmt ")[%d]@]" n.refcount )

    let to_string n =
      pp Format.str_formatter n ;
      Format.flush_str_formatter ()

    let elt_to_string k =
      Ord.pp Format.str_formatter k ;
      Format.flush_str_formatter ()

    let for_all p =
      let rec every n =
        is_empty n || (p n.elt && every n.left && every n.right)
      in
      every

    let less n k =
      let lt j = Ord.compare j k < 0 in
      for_all lt n

    let greater n k =
      let gt j = Ord.compare j k > 0 in
      for_all gt n

    let rec is_ordered n =
      is_empty n
      ||
      let l = n.left and k = n.elt and r = n.right in
      less l k && greater r k && is_ordered l && is_ordered r

    let pos_refcounts n =
      let rec pos n =
        is_empty n || (n.refcount > 0 && pos n.left && pos n.right)
      in
      is_empty n || (pos n.left && pos n.right)

    let is_ordered n =
      let res = is_ordered n in
      if not res then
        Format.eprintf "\nIs_ordered %s failed.@?" (to_string n) ;
      res

    let pos_refcounts n =
      let res = pos_refcounts n in
      if not res then
        Format.eprintf "\nposRefcounts %s failed.@?" (to_string n) ;
      res

    let well_formed n = is_ordered n && pos_refcounts n

    let rec max_elt n =
      assert (well_formed n) ;
      if is_empty n then raise Not_found
      else if is_empty n.right then n.elt
      else max_elt n.right

    let rec min_elt n =
      assert (well_formed n) ;
      if is_empty n then raise Not_found
      else if is_empty n.left then n.elt
      else min_elt n.left

    let iter f n =
      assert (well_formed n) ;
      let rec iterf n =
        if is_empty n then ()
        else (
          iterf n.left ;
          f n.elt ;
          iterf n.right )
      in
      iterf n

    let fold f n e =
      assert (well_formed n) ;
      let acc = ref e in
      let g i = acc := f i !acc in
      iter g n ;
      !acc

    let exists p n =
      assert (well_formed n) ;
      let rec some n =
        if is_empty n then false else p n.elt || some n.left || some n.right
      in
      some n

    let ( << ) p q =
      assert (well_formed p) ;
      assert (well_formed q) ;
      is_empty p
      || is_empty q
      ||
      try Ord.compare (max_elt p) (min_elt q) < 0 with Not_found -> false

    let cardinal s =
      let acc = ref 0 in
      let rec count n =
        if is_empty n then ()
        else (
          acc := !acc + 1 ;
          count n.left ;
          count n.right )
      in
      count s ;
      !acc

    let rec mem i n =
      assert (is_ordered n) ;
      if is_empty n then false
      else
        let cmp = Ord.compare i n.elt in
        cmp == 0 || mem i (if cmp < 0 then n.left else n.right)

    let incr n = if is_node n then n.refcount <- n.refcount + 1

    let decr_pre n =
      let res = if is_node n then n.refcount >= 1 else true in
      if not res then
        Format.eprintf "\nPre(decr %s) violated@?" (to_string n) ;
      res

    let decr n =
      assert (decr_pre n) ;
      if is_node n then n.refcount <- n.refcount - 1

    let finalise =
      let action n =
        assert (is_node n)
        (* following commented because it turns out that sometimes the
           decr_pre assertion is violated, so somewhere the reference
           counting must be off. *)
        (* assert (well_formed n) ;
         * assert (n.refcount >= 0) ;
         * (* refcounts may be overapproximating. *)
         * assert (decr_pre n.left) ;
         * assert (decr_pre n.right) ;
         * decr n.left ;
         * decr n.right *)
      in
      let do_finalize = Gc.finalise action in
      fun n ->
        try do_finalize n
        with exc ->
          Format.eprintf "\nWarning(splay.set): %s@?"
            (Printexc.to_string exc)

    (** Create a node with reference count [0] and instruct garbage
        collector to decrement reference counters of the children upon
        collecting this node. *)
    let create l k r =
      assert (well_formed l) ;
      assert (well_formed r) ;
      let n = {left= l; elt= k; right= r; refcount= 0} in
      incr l ;
      incr r ;
      finalise n ;
      assert (well_formed n) ;
      assert (n.refcount = 0) ;
      n

    (** Reorganize the splay tree [x] so that element [i] is at the root if
        [mem i x].

        - {i Empty}. The splay of [Empty] is Empty.
        - {i Base}. If [x] has a parent [p] but no grandparent, we just
          [rotate x p]. *)
    let rec splay i p =
      assert (well_formed p) ;
      if not (is_empty p) then splay_node i p ;
      assert (well_formed p)

    and splay_node i p =
      assert (is_node p) ;
      assert (well_formed p) ;
      let cmp = Ord.compare i p.elt in
      if cmp < 0 then (
        splay i p.left ;
        rrotate p )
      else if cmp > 0 then (
        splay i p.right ;
        lrotate p ) ;
      assert (well_formed p) ;
      assert (mem i p = (Ord.compare i p.elt == 0))

    (** Right rotation: [p<l<a,j,b>,k,c>] ==> [p<a,j,n<b,k,c>>]. If the
        reference count of [l] is [1], then this node is reused for building
        node [n]. *)
    and rrotate p =
      assert (is_node p) ;
      assert (well_formed p) ;
      let l = p.left in
      if is_node l then (
        let k = p.elt and c = p.right in
        ( if l.refcount = 1 then (
          (* reuse [l]. *)
          p.left <- l.left ;
          p.elt <- l.elt ;
          p.right <- l ;
          l.left <- l.right ;
          l.elt <- k ;
          l.right <- c )
        else
          let n = create l.right k c in
          incr n ;
          decr l ;
          p.left <- l.left ;
          p.elt <- l.elt ;
          p.right <- n ) ;
        assert (refcount p.right = 1) ;
        assert (well_formed p) )

    (** Left rotation: [p<a,k,r<b,j,c>>] ==> [p<n<a,k,b>,j,c>] *)
    and lrotate p =
      assert (is_node p) ;
      assert (well_formed p) ;
      let r = p.right in
      if is_node r then (
        let a = p.left
        and k = p.elt
        and b = r.left
        and j = r.elt
        and c = r.right in
        ( if r.refcount = 1 then (
          p.left <- r ;
          p.elt <- j ;
          p.right <- c ;
          r.left <- a ;
          r.elt <- k ;
          r.right <- b )
        else
          let n = create a k b in
          incr n ;
          decr r ;
          p.left <- n ;
          p.elt <- j ;
          p.right <- c ) ;
        assert (refcount p.left = 1) ;
        assert (well_formed p) )

    (** Return a node [m] which contains [k] and all elements of [n]. If the
        refcount of [n] is [1], then [n] might be updated to contain a
        different set. *)
    let add k n =
      assert (is_node n) ;
      assert (well_formed n) ;
      assert (not (mem k n)) ;
      splay_node k n ;
      let l = n.left and j = n.elt and r = n.right in
      let cmp = Ord.compare j k in
      assert (cmp <> 0) ;
      let m =
        if cmp < 0 then (
          if (* Case [j < k]. *)
             n.refcount = 1 then (
            (* reuse [n]. *)
            n.right <- empty ;
            assert (well_formed n) ;
            create n k r )
          else
            let l' = create l j empty in
            incr l' ;
            assert (well_formed l') ;
            create l' k r )
        else if (* Case [j > k]. *)
                n.refcount = 1 then (
          (* reuse [n]. *)
          n.left <- empty ;
          assert (well_formed n) ;
          create l k n )
        else
          let r' = create empty j r in
          incr r' ;
          assert (well_formed r') ;
          create l k r'
      in
      assert (is_node m) ;
      assert (well_formed m) ;
      assert (m.refcount = 0) ;
      assert (mem k m) ;
      m

    (** Splay for the minimum element in (the element-wise larger) set [m]
        and add [n] as a left child. *)
    let join n m =
      assert (is_node n) ;
      assert (is_node m) ;
      assert (well_formed n) ;
      assert (well_formed m) ;
      assert (n << m) ;
      splay_node (min_elt m) m ;
      assert (is_empty m.left) ;
      let k =
        if reuse && m.refcount = 1 then (
          incr n ;
          decr m.left ;
          m.left <- n ;
          m.refcount <- 0 ;
          m )
        else
          let m' = create n m.elt m.right in
          m'
      in
      assert (well_formed k) ;
      assert (k.refcount = 0) ;
      k
  end

  type t = {mutable root: Tree.t}

  let well_formed s =
    Tree.well_formed s.root
    && (Tree.is_empty s.root || s.root.Tree.refcount > 0)

  module Balanced = Set.Make (Ord) (* Reference implementation. *)

  let to_balanced s =
    assert (well_formed s) ;
    Tree.fold Balanced.add s.root Balanced.empty

  let mem k s =
    assert (well_formed s) ;
    let res = Tree.mem k s.root in
    assert (res = Balanced.mem k (to_balanced s)) ;
    res

  let is_empty s =
    assert (well_formed s) ;
    Tree.is_empty s.root

  let d_singleton s = Tree.d_singleton s.root
  let to_list s = Tree.to_list s.root
  let exists p s = Tree.exists p s.root
  let for_all p s = Tree.for_all p s.root
  let fold f s = Tree.fold f s.root
  let iter f s = Tree.iter f s.root
  let pp fmt s = Tree.pp fmt s.root
  let to_string s = Tree.to_string s.root

  let cardinal s =
    assert (well_formed s) ;
    let n = Tree.cardinal s.root in
    assert (n = Balanced.cardinal (to_balanced s)) ;
    n

  let max_elt s =
    assert (well_formed s) ;
    let k = Tree.max_elt s.root in
    assert (Ord.compare k (Balanced.max_elt (to_balanced s)) = 0) ;
    k

  let min_elt s =
    assert (well_formed s) ;
    let k = Tree.min_elt s.root in
    assert (Ord.compare k (Balanced.min_elt (to_balanced s)) = 0) ;
    k

  let finalise =
    let action s =
      (* assert(well_formed s); *)
      assert (Tree.decr_pre s.root) ;
      Tree.decr s.root
    in
    fun s ->
      try Gc.finalise action s
      with exc ->
        Format.eprintf "\nWarning(splay.set): %s@?" (Printexc.to_string exc)

  let empty () =
    let s = {root= Tree.empty} in
    assert (well_formed s) ;
    assert (Balanced.equal (to_balanced s) Balanced.empty) ;
    finalise s ;
    s

  let singleton i =
    let r = Tree.create Tree.empty i Tree.empty in
    let s = {root= r} in
    Tree.incr r ;
    assert (well_formed s) ;
    assert (Balanced.equal (to_balanced s) (Balanced.singleton i)) ;
    finalise s ;
    s

  let copy s =
    assert (well_formed s) ;
    let s' = {root= s.root} in
    Tree.incr s.root ;
    finalise s' ;
    assert (well_formed s') ;
    assert (Balanced.equal (to_balanced s) (to_balanced s')) ;
    s'

  let b = ref (Obj.magic 0) (* For debugging. *)

  let b1 = ref !b
  let b2 = ref !b

  let add k s =
    assert (well_formed s) ;
    assert (
      b := to_balanced s ;
      true ) ;
    if is_empty s then (
      let r = Tree.create Tree.empty k Tree.empty in
      assert (Tree.well_formed r) ;
      Tree.incr r ;
      s.root <- r )
    else if not (mem k s) then (
      let n = s.root in
      assert (Tree.is_node n) ;
      let m = Tree.add k n in
      assert (Tree.well_formed m) ;
      Tree.incr m ;
      Tree.decr n ;
      s.root <- m ) ;
    assert (well_formed s) ;
    assert (Balanced.equal (to_balanced s) (Balanced.add k !b)) ;
    assert (mem k s)

  let remove k s =
    (* to do: remove seems to destroy other sets! *)
    assert (well_formed s) ;
    assert (
      b := to_balanced s ;
      true ) ;
    if mem k s then (
      let n = s.root in
      assert (Tree.is_node n) ;
      assert (Tree.mem k n) ;
      Tree.splay_node k n ;
      let l = n.Tree.left and r = n.Tree.right in
      assert (Ord.compare k n.Tree.elt = 0) ;
      assert (Tree.less l k) ;
      assert (Tree.greater r k) ;
      match (Tree.is_empty l, Tree.is_empty r) with
      | true, true ->
          Tree.decr n ;
          s.root <- Tree.empty
      | true, false ->
          Tree.incr r ;
          Tree.decr n ;
          s.root <- r
      | false, true ->
          Tree.incr l ;
          Tree.decr n ;
          s.root <- l
      | false, false ->
          assert (Tree.( << ) l r) ;
          Tree.splay_node (Tree.min_elt r) r ;
          assert (Tree.is_empty r.Tree.left) ;
          if reuse && r.Tree.refcount = 1 then (
            (* reuse [r] *)
            Tree.incr l ;
            r.Tree.left <- l ;
            assert (Tree.well_formed r) ;
            Tree.incr r ;
            Tree.decr n ;
            s.root <- r )
          else
            let r' = Tree.create l r.Tree.elt r.Tree.right in
            assert (Tree.well_formed r') ;
            Tree.incr r' ;
            Tree.decr n ;
            s.root <- r' ) ;
    assert (well_formed s) ;
    assert (not (mem k s)) ;
    assert (Balanced.equal (to_balanced s) (Balanced.remove k !b)) ;
    assert (not (mem k s))

  let replace i j s =
    assert (well_formed s) ;
    assert (
      b := to_balanced s ;
      true ) ;
    remove i s ;
    add j s ;
    assert (well_formed s) ;
    assert (
      Balanced.equal (to_balanced s) (Balanced.add j (Balanced.remove i !b))
    )

  let union s1 s2 =
    assert (well_formed s1) ;
    assert (well_formed s2) ;
    assert (
      b1 := to_balanced s1 ;
      b2 := to_balanced s2 ;
      true ) ;
    if not (is_empty s1) then (
      try add (d_singleton s1) s2
      with Not_found ->
        if is_empty s2 then (
          let n1 = s1.root and n2 = s2.root in
          Tree.incr n1 ;
          Tree.decr n2 ;
          s2.root <- n1 )
        else
          let n1 = s1.root and n2 = s2.root in
          let acc = ref n2 in
          let add1 k =
            if not (Tree.mem k !acc) then acc := Tree.add k !acc
          in
          Tree.iter add1 n1 ;
          Tree.incr !acc ;
          Tree.decr n2 ;
          s2.root <- !acc ) ;
    assert (well_formed s1) ;
    assert (well_formed s2) ;
    assert (Balanced.equal (to_balanced s2) (Balanced.union !b1 !b2)) ;
    assert (Balanced.equal !b1 (to_balanced s1))

  let choose s = if is_empty s then raise Not_found else s.root.Tree.elt

  exception Found

  let choose_if =
    let elt = ref (Obj.magic 0) in
    fun p s ->
      assert (well_formed s) ;
      let test x =
        if p x then (
          elt := x ;
          raise Found )
      in
      try
        Tree.iter test s.root ;
        raise Not_found
      with Found -> !elt

  let subset s1 s2 =
    assert (well_formed s1) ;
    assert (well_formed s2) ;
    let mem2 x = Tree.mem x s2.root in
    let res = Tree.for_all mem2 s1.root in
    assert (Balanced.subset (to_balanced s1) (to_balanced s2)) ;
    res

  let equal s1 s2 = subset s1 s2 && subset s2 s1

  let of_list l =
    let s = empty () in
    let add1 i = add i s in
    List.iter add1 l ;
    assert (well_formed s) ;
    s
end

module Make = Splay

(**/**)

(** Following for debugging only. *)
module Test = struct
  module Ints = Make (struct
    type t = int

    let compare = Stdlib.compare
    let pp fmt i = Format.fprintf fmt "%d" i
  end)

  let numofprobes = ref 10000
  let numofsets = ref !numofprobes
  let maxelt = ref 5
  let init = Random.self_init

  let run () =
    let debug_save = !Ints.debug in
    let max = ref 1 in
    (* current sets. *)
    let set_to_string x =
      Ints.pp Format.str_formatter x ;
      Format.flush_str_formatter ()
    in
    let sets = Array.make !numofsets (Ints.empty ()) in
    let genelt () = Random.int !maxelt in
    let genidx () = Random.int !max in
    let lookup i = sets.(i) in
    let getset () =
      let i = Random.int !max in
      (sets.(i), i)
    in
    let add () =
      let x = genelt () and s, i = getset () in
      Format.eprintf "\nadd[%d] %d %s@?" i x (set_to_string s) ;
      Ints.add x s ;
      Format.eprintf " --> %s@?" (set_to_string (lookup i))
    in
    let rem () =
      let x = genelt () and s, i = getset () in
      Format.eprintf "\nrem[%d] %d %s@?" i x (set_to_string s) ;
      Ints.remove x s ;
      Format.eprintf " --> %s@?" (set_to_string (lookup i))
    in
    let union () =
      let s1, i1 = getset () and s2, i2 = getset () in
      Format.eprintf "\nunion[%d] %s %s@?" i2 (set_to_string s1)
        (set_to_string s2) ;
      Ints.union s1 s2 ;
      Format.eprintf " --> %s@?" (set_to_string (lookup i2))
    in
    let replace () =
      let x = genelt () and y = genelt () and s, i = getset () in
      Format.eprintf "\nreplace[%d] %d %d %s@?" i x y (set_to_string s) ;
      let _ = Ints.replace x y s in
      Format.eprintf " --> %s@?" (set_to_string (lookup i))
    in
    let empty () =
      incr max ;
      Format.eprintf "\nempty[%d] @?" !max ;
      sets.(!max) <- Ints.empty ()
    in
    let singleton () =
      let x = genelt () in
      incr max ;
      Format.eprintf "\nsingleton[%d] %d" !max x ;
      sets.(!max) <- Ints.singleton x ;
      Format.eprintf " --> %s@?" (set_to_string (lookup !max))
    in
    let copy () =
      let s, i = getset () in
      incr max ;
      Format.eprintf "\ncopy[%d -> %d] %s@?" i !max (set_to_string s) ;
      sets.(!max) <- Ints.copy s ;
      Format.eprintf " --> %s@?" (set_to_string (lookup !max))
    in
    let apply () =
      match Random.int 7 with
      | 0 -> add ()
      | 5 -> copy ()
      | 6 -> empty ()
      | 4 -> singleton ()
      | 1 -> rem ()
      | 3 -> replace ()
      | 2 -> union ()
      | _ -> Gc.minor ()
    in
    let well_formed s = Ints.well_formed s in
    let check () =
      for i = 0 to !max - 1 do
        let s = sets.(i) in
        if not (well_formed s) then
          raise
            (Invalid_argument
               (Format.sprintf "\nSet[%d]: %s not well-formed@?" i
                  (set_to_string s)))
      done
    in
    Ints.debug := true ;
    for _ = 0 to !numofprobes do
      apply () ;
      check ()
    done ;
    Ints.debug := debug_save ;
    Format.eprintf "sets: self test ok.@?"
end
