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

module type VAR = Type.ORDERED

module type TRM = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit

  type var

  val of_var : var -> t
  val iter : (var -> unit) -> t -> unit
  val map : (var -> t) -> t -> t
end

module type S = sig
  type var
  type trm
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val apply : t -> trm -> trm
  val lookup : t -> var -> trm
  val inv : t -> trm -> var
  val dom : var -> t -> bool
  val empty : unit -> t
  val is_empty : t -> bool
  val singleton : var -> trm -> t
  val copy : t -> t
  val fuse : t -> var -> trm -> unit
  val add : t -> var -> trm -> unit
  val update : t -> var -> trm -> unit
  val remove : t -> var -> unit
  val compose : t -> t -> unit
  val disjoint : t -> t -> bool
  val fold : (var -> trm -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (var -> trm -> unit) -> t -> unit
  val exists : (var -> trm -> bool) -> t -> bool
  val for_all : (var -> trm -> bool) -> t -> bool
  val choose : (var -> trm -> bool) -> t -> var * trm
end

module Make (Var : VAR) (Trm : TRM with type var = Var.t) = struct
  type var = Trm.var
  type trm = Trm.t

  exception Violation

  let for_all p t =
    let test x = if not (p x) then raise Violation in
    try
      Trm.iter test t ;
      true
    with Violation -> false

  exception Witness

  let exists p t =
    let test x = if p x then raise Witness in
    try
      Trm.iter test t ;
      false
    with Witness -> true

  let occurs x =
    let eqx = Var.equal x in
    exists eqx

  let ground =
    let all _ = true in
    fun t -> not (exists all t)

  let replace x s t =
    if Trm.equal (Trm.of_var x) s then t
    else
      let repl z = if Var.equal x z then s else Trm.of_var z in
      Trm.map repl t

  module Map = Maps.Make (Var) (Trm)

  type t = Map.t

  let well_formed rho =
    Map.for_all
      (fun x _ -> Map.for_all (fun _ t -> not (occurs x t)) rho)
      rho

  let dom = Map.mem
  let cod x = Map.exists (fun _ t -> occurs x t)

  let apply rho =
    let lookup x = try Map.find x rho with Not_found -> Trm.of_var x in
    Trm.map lookup

  let lookup rho x = Map.find x rho

  let inv rho t =
    let invt _ = Trm.equal t in
    fst (Map.choose_if invt rho)

  let empty = Map.empty
  let is_empty = Map.is_empty

  let disjoint rho tau =
    Map.for_all
      (fun x _ -> Map.for_all (fun y _ -> not (Var.equal x y)) tau)
      rho

  let[@warning "-32"] ground = Map.for_all (fun _ t -> ground t)

  let singleton x t =
    let rho = Map.empty () in
    Map.set x t rho ;
    assert (well_formed rho) ;
    rho

  let copy = Map.copy

  let fuse =
    let dom_change = Stacks.create () and cod_change = Stacks.create () in
    fun rho y b ->
      assert (well_formed rho) ;
      assert (not (occurs y b)) ;
      assert (for_all (fun x -> not (dom x rho)) b) ;
      if not (Trm.equal (Trm.of_var y) b) then (
        Stacks.clear dom_change ;
        Stacks.clear cod_change ;
        Map.iter
          (fun x a ->
            if occurs y a then (
              let a' = replace y b a in
              assert (not (occurs y a')) ;
              Stacks.push x dom_change ;
              Stacks.push a' cod_change ) )
          rho ;
        while not (Stacks.is_empty dom_change) do
          let x = Stacks.pop dom_change in
          let b = Stacks.pop cod_change in
          Map.set x b rho
        done ;
        assert (not (cod y rho)) ;
        assert (well_formed rho) )

  let add rho y b =
    assert (well_formed rho) ;
    assert (not (occurs y b)) ;
    assert (for_all (fun x -> not (dom x rho)) b) ;
    assert (not (dom y rho)) ;
    if not (Trm.equal (Trm.of_var y) b) then (
      fuse rho y b ;
      Map.set y b rho ;
      assert (dom y rho) ;
      assert (not (cod y rho)) ;
      assert (well_formed rho) )

  let compose rho tau =
    assert (disjoint rho tau) ;
    Map.iter (add rho) tau

  let update rho x t =
    assert (not (occurs x t)) ;
    Map.set x t rho

  let remove rho y = Map.remove y rho
  let pp = Map.pp
  let fold = Map.fold
  let iter = Map.iter
  let exists = Map.exists
  let for_all = Map.for_all
  let choose = Map.choose_if
  let equal = Map.equal
end

(** For debugging only. *)
module Test = struct
  module Var = struct
    type t = int

    let equal = ( = )
    let compare = Stdlib.compare
    let hash x = x
    let pp fmt = Format.fprintf fmt "%d"
    let max = 8
    let random () = Random.int max
  end

  module Term = struct
    type var = Var.t
    type t = Var of var | App of string * t list

    let equal = ( = )

    let rec pp fmt = function
      | Var i -> Format.fprintf fmt "x[%d]" i
      | App (f, al) ->
          Format.fprintf fmt "(%s" f ;
          List.iter (pp fmt) al ;
          Format.fprintf fmt ")"

    let hash = Hashtbl.hash
    let compare = Stdlib.compare
    let of_var x = Var x

    let rec iter f = function
      | Var x -> f x
      | App (_, al) -> List.iter (iter f) al

    let rec map f = function
      | Var x -> f x
      | App (g, al) ->
          let bl = List.map (map f) al in
          App (g, bl)

    let _maxdepth = 7
    let maxargs = 3
    let maxsize = ref 15

    let rec random () =
      let size = ref 0 in
      let funsym () = Format.sprintf "f[%d]" (Random.int 5) in
      let args () =
        let rec loop acc = function
          | 0 -> acc
          | i ->
              if !size > !maxsize then acc
              else (
                size := !size + 1 ;
                loop (random () :: acc) (i - 1) )
        in
        loop [] (Random.int maxargs)
      in
      if Random.int 2 mod 2 = 0 then of_var (Var.random ())
      else
        let f = funsym () and al = args () in
        App (f, al)
  end

  module Subst = Make (Var) (Term)

  module Heap = struct
    let max = 1000
    let heap = Array.make 1000 (Subst.empty ())
    let ptr = ref 0

    let alloc () =
      if !ptr < max then
        let[@warning "-26"] i = !ptr in
        incr ptr

    let random () = Random.int !ptr
    let get i = heap.(i)
  end

  let empty () = Heap.alloc ()

  let add () =
    let i = Heap.random () and x = Var.random () and t = Term.random () in
    Subst.add (Heap.get i) x t

  let compose () =
    let i = Heap.random () and j = Heap.random () in
    Subst.compose (Heap.get i) (Heap.get j)

  let maxruns = ref 10000

  let run () =
    for _ = 0 to !maxruns do
      match Random.int 3 with
      | 0 -> empty ()
      | 1 -> add ()
      | 2 -> compose ()
      | _ -> ()
    done
end
