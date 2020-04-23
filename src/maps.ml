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

module type EqType = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type key
  type value
  type t

  val empty : unit -> t
  val is_empty : t -> bool
  val singleton : key -> value -> t
  val is_singleton : t -> bool
  val find : key -> t -> value
  val set : key -> value -> t -> unit
  val remove : key -> t -> unit
  val copy : t -> t
  val mem : key -> t -> bool
  val iter : (key -> value -> unit) -> t -> unit
  val fold : (key -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val cardinal : t -> int
  val map : (value -> value) -> t -> t
  val replace : t -> key -> key -> t
  val for_all : (key -> value -> bool) -> t -> bool
  val exists : (key -> value -> bool) -> t -> bool
  val choose_if : (key -> value -> bool) -> t -> key * value
  val destruct : (key -> value -> bool) -> t -> key * value * t
  val to_list : t -> (key * value) list
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (Key : OrderedType) (Val : EqType) = struct
  type key = Key.t
  type value = Val.t

  module Bndng = struct
    type t = key * value

    let compare (k1, _) (k2, _) = Key.compare k1 k2

    let pp fmt (k, v) =
      Format.fprintf fmt "@[" ;
      Key.pp fmt k ;
      Format.fprintf fmt " |-> " ;
      Val.pp fmt v ;
      Format.fprintf fmt "@]@?"
  end

  module Graph = Sets.Make (Bndng)

  type t = Graph.t

  let pp = Graph.pp
  let uncurry p (x, y) = p x y
  let iter f = Graph.iter (uncurry f)
  let fold f = Graph.fold (uncurry f)
  let for_all p = Graph.for_all (uncurry p)
  let exists p = Graph.exists (uncurry p)

  let is_functional m =
    for_all
      (fun x u ->
        for_all
          (fun y v -> if Key.compare x y == 0 then Val.equal u v else true)
          m )
      m

  let well_formed = is_functional
  let empty = Graph.empty
  let is_empty = Graph.is_empty

  let singleton x v =
    let m1 = Graph.singleton (x, v) in
    assert (well_formed m1) ;
    m1

  let is_singleton m =
    assert (well_formed m) ;
    Graph.cardinal m = 1

  (** Alternative implementation. *)
  module Balanced = Map.Make (Key)

  let to_balanced m = fold Balanced.add m Balanced.empty

  let cardinal m =
    assert (well_formed m) ;
    Graph.cardinal m

  let bndng x m =
    assert (well_formed m) ;
    let keyx (y, _) = Key.compare x y == 0 in
    let b = Graph.choose_if keyx m in
    assert (Key.compare (fst b) x = 0) ;
    assert (
      try Val.equal (snd b) (Balanced.find x (to_balanced m))
      with _ -> false ) ;
    b

  let mem x m =
    assert (well_formed m) ;
    try
      let _ = bndng x m in
      assert (Balanced.mem x (to_balanced m)) ;
      true
    with Not_found ->
      assert (not (Balanced.mem x (to_balanced m))) ;
      false

  let find x m =
    assert (well_formed m) ;
    let v = snd (bndng x m) in
    assert (
      try Val.equal v (Balanced.find x (to_balanced m)) with _ -> false ) ;
    v

  let copy m =
    assert (well_formed m) ;
    let m' = Graph.copy m in
    assert (well_formed m') ;
    (* assert(Balanced.equal Val.equal (to_balanced m) (to_balanced m')); *)
    m'

  let b = ref Balanced.empty (* for debugging only. *)

  let set x v m =
    assert (well_formed m) ;
    assert (
      b := to_balanced m ;
      true ) ;
    try
      let xu = bndng x m in
      Graph.remove xu m ;
      Graph.add (x, v) m
    with Not_found ->
      Graph.add (x, v) m ;
      assert (well_formed m) ;
      assert (mem x m) ;
      assert (try Val.equal (find x m) v with _ -> false)

  (* ; assert(Balanced.equal Val.equal (to_balanced m) (Balanced.add x v
     !b)) *)

  let remove x m =
    assert (well_formed m) ;
    assert (
      b := to_balanced m ;
      true ) ;
    try Graph.remove (bndng x m) m
    with Not_found ->
      () ;
      assert (well_formed m) ;
      assert (not (mem x m))

  (* ; assert(Balanced.equal Val.equal (to_balanced m) (Balanced.remove x
     !b)) *)

  let map f m =
    assert (well_formed m) ;
    let acc = empty () in
    let add (x, v) = set x (f v) acc in
    Graph.iter add m ;
    assert (well_formed acc) ;
    acc

  let replace m x y =
    assert (well_formed m) ;
    try
      let d = find x m in
      let m' = copy m in
      remove x m' ;
      set y d m' ;
      assert (well_formed m') ;
      assert (not (mem x m')) ;
      m'
    with Not_found ->
      assert (not (mem x m)) ;
      m

  let choose_if p m =
    assert (well_formed m) ;
    let q (x, v) = p x v in
    Graph.choose_if q m

  let destruct p m =
    assert (well_formed m) ;
    let x, v = choose_if p m in
    let m' = copy m in
    remove x m' ;
    assert (well_formed m') ;
    assert (not (mem x m)) ;
    (* assert(Balanced.equal Val.equal (to_balanced m) (Balanced.add x v
       (to_balanced m'))); *)
    (x, v, m')

  let to_list = Graph.to_list

  let equal m1 m2 =
    assert (well_formed m1) ;
    assert (well_formed m2) ;
    (* not really. *)
    Graph.subset m1 m2 && Graph.subset m2 m1
end

module Expt (T : OrderedType) = Make (T) (Sets.Make (T))
