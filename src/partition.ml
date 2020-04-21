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

module type VAR = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val preference : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type INFSYS = sig
  type var

  module Disequalities : Sets.S with type elt = var * var
  module Equalities : Maps.S with type key = var and type value = var

  type t

  val equalities : unit -> Equalities.t
  val disequalities : unit -> Disequalities.t
  val current : unit -> t
  val reset : unit -> unit
  val initialize : t -> unit
  val unchanged : unit -> bool
  val find : var -> var
  val canonical : var -> bool

  module Varset : Sets.S with type elt = var

  val deqs : var -> Varset.t
  val eqs : var -> Varset.t
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val empty : t

  exception Unsat

  val union : propagate_deq:(var -> var -> unit) -> var -> var -> unit
  val separate : var -> var -> unit
  val iter_equiv : (var -> unit) -> var -> unit
  val choose_equiv : (var -> bool) -> var -> var
  val iter_diseqs : (var -> var -> unit) -> var -> unit
end

module Make (Var : VAR) = struct
  type var = Var.t

  (** Finite maps and sets of term variables. *)

  module Varmap = Maps.Make (Var) (Var)
  module Varset = Sets.Make (Var)
  module Parent = Varmap
  module Diseqs = Maps.Make (Var) (Varset)
  module Rank = Maps.Make (Var) (Int)

  type t =
    { mutable parent: Parent.t
    ; mutable diseqs: Diseqs.t
    ; mutable rank: Rank.t }

  let empty =
    {parent= Parent.empty (); diseqs= Diseqs.empty (); rank= Rank.empty ()}

  (** The initial configuration. *)
  let init = ref empty

  module Config = struct
    module Parent = Config.Map (Parent)
    module Diseqs = Config.Map (Diseqs)
    module Rank = Config.Map (Rank)
  end

  let parent = Config.Parent.current
  let diseqs = Config.Diseqs.current
  let rank = Config.Rank.current
  let p x = try Config.Parent.find x with Not_found -> x
  let rk x = try Rank.find x (rank ()) with Not_found -> 0

  (** Canonical representative (without path compression). *)
  let rec repr x = try repr (Config.Parent.find x) with Not_found -> x

  (** Canonical representative with path compression. *)
  let rec find x =
    try
      let y = Config.Parent.find x in
      let z = find y in
      if not (Var.equal y z) then Config.Parent.set x z ;
      z
    with Not_found -> x

  let deqs =
    let empty = Varset.empty () in
    fun x ->
      try Config.Diseqs.find (find x)
      with Not_found ->
        assert (Varset.is_empty empty) ;
        empty

  let eqs z =
    let z = find z in
    let acc = Varset.empty () in
    let addeq x y =
      if Var.equal (find x) z then Varset.add x acc ;
      if Var.equal (find y) z then Varset.add x acc
    in
    Parent.iter addeq (parent ()) ;
    acc

  let iter_equiv f x =
    let x = find x in
    f x ;
    let apply y _ = if Var.equal (repr y) x then f y in
    Parent.iter apply (parent ())

  exception Found of Var.t

  let choose_equiv p x =
    let test y = if p y then raise (Found y) in
    try
      iter_equiv test x ;
      raise Not_found
    with Found y -> y

  let iter_diseqs =
    let module Visited = struct
      let s = Stacks.create ()
      let clear () = Stacks.clear s
      let holds y = Stacks.mem Var.equal y s
      let push y = Stacks.push y s
    end in
    fun f x ->
      let x = find x in
      let ys = deqs x in
      if Varset.is_empty ys then ()
      else
        let apply y =
          let y = find y in
          if Visited.holds y then ()
          else (
            Visited.push y ;
            f x y )
        in
        Visited.clear () ;
        Varset.iter apply ys

  let equal x y = Var.equal (find x) (find y)
  let canonical x = not (Config.Parent.mem x)

  let diseq x y =
    let x = find x and y = find y in
    assert (canonical x) ;
    assert (canonical y) ;
    try
      let zs = Diseqs.find x (diseqs ()) in
      let eqy z = Var.equal y (find z) in
      Varset.exists eqy zs
    with Not_found -> false

  let well_formed () =
    let[@warning "-26"] p = parent () and d = diseqs () in
    let ordered () = true
    and canonized () =
      Diseqs.for_all (fun x ys -> canonical x && not (Varset.is_empty ys)) d
    and consistent () =
      Diseqs.for_all
        (fun x ys -> Varset.for_all (fun y -> not (equal y x)) ys)
        d
    and closed () =
      Diseqs.for_all
        (fun _ ys -> Varset.for_all (fun y -> Diseqs.mem (find y) d) ys)
        d
    in
    ordered () && canonized () && consistent () && closed ()

  let initialize s =
    init := s ;
    Config.Parent.initialize s.parent ;
    Config.Diseqs.initialize s.diseqs ;
    Config.Rank.initialize s.rank

  let reset () = initialize empty

  let unchanged () =
    Config.Parent.unchanged () && Config.Diseqs.unchanged ()

  (** Shallow copy; updates ensure that components are protected. *)
  let current () =
    if unchanged () then !init
    else
      { parent= Config.Parent.current ()
      ; diseqs= Config.Diseqs.current ()
      ; rank= Config.Rank.current () }

  (** Least common ancestor of [x] and [y] by stepwise descending along find
      structure. Assumes, [x] and [y] are congruent modulo [s].*)
  let[@warning "-32"] lca x y =
    assert (equal x y) ;
    let rec descend x y =
      assert (equal x y) ;
      let c = Var.compare x y in
      if c = 0 then x
      else if c > 0 then descend (p x) y
      else descend x (p y)
    in
    descend x y

  module Equalities = Parent

  module Disequalities = Sets.Make (struct
    type t = var * var

    let[@warning "-32"] equal (x1, y1) (x2, y2) =
      Var.equal x1 x2 && Var.equal y1 y2

    let compare (x1, y1) (x2, y2) =
      let c = Var.compare x1 x2 in
      if c <> 0 then c else Var.compare y1 y2

    let[@warning "-32"] hash (x, y) =
      (Var.hash x + Var.hash y) land 0x3FFFFFFF

    let pp fmt (x, y) =
      Format.fprintf fmt "@[" ;
      Var.pp fmt x ;
      Format.fprintf fmt " <> " ;
      Var.pp fmt y ;
      Format.fprintf fmt "@]@?"
  end)

  let equalities () =
    let acc = Varmap.empty () in
    let add x y = Varmap.set x y acc in
    Parent.iter (fun x y -> add x (find y)) (parent ()) ;
    acc

  let disequalities () =
    let acc = Disequalities.empty () in
    let accumulate x y =
      let x, y = if Var.compare x y > 0 then (x, y) else (y, x) in
      Disequalities.add (x, y) acc
    in
    Diseqs.iter (fun x -> Varset.iter (accumulate x)) (diseqs ()) ;
    acc

  exception Unsat

  let combine xs ys =
    assert (not (Varset.is_empty xs)) ;
    assert (not (Varset.is_empty ys)) ;
    let cmb xs ys =
      assert (Varset.cardinal xs >= Varset.cardinal ys) ;
      let acc = Varset.copy xs in
      let mem z = Varset.exists (equal z) acc in
      let add z = if not (mem z) then Varset.add (find z) acc in
      Varset.iter add ys ;
      assert (not (Varset.is_empty acc)) ;
      acc
    in
    let n = Varset.cardinal xs and m = Varset.cardinal ys in
    if n >= m then cmb xs ys else cmb ys xs

  let merge_d ~propagate_deq x y =
    assert (equal x y) ;
    try
      let xs = Diseqs.find x (diseqs ()) in
      Config.Diseqs.remove x ;
      try
        let ys = Diseqs.find y (diseqs ()) in
        let xys = combine xs ys in
        assert (not (Varset.is_empty xys)) ;
        Config.Diseqs.set y xys ;
        Varset.iter
          (fun x -> if not (Varset.mem x ys) then propagate_deq y x)
          xys
      with Not_found ->
        Config.Diseqs.set y xs ;
        Varset.iter (propagate_deq y) xs
    with Not_found -> ()

  let merge ~propagate_deq x y =
    assert (canonical x) ;
    assert (canonical y) ;
    assert (not (equal x y)) ;
    Config.Parent.set x y ;
    Config.Rank.remove x ;
    merge_d ~propagate_deq x y ;
    assert (not (canonical x))

  let union ~propagate_deq x y =
    assert (well_formed ()) ;
    let x = find x and y = find y in
    let rx = rk x and ry = rk y in
    ( if Var.equal x y then ()
    else if diseq x y then raise Unsat
    else
      try
        let p = Var.preference x y in
        if p < 0 then (
          merge ~propagate_deq x y ;
          if rx > ry then Config.Rank.set y rx
          else if rx = ry then Config.Rank.set y (ry + 1) )
        else (
          merge ~propagate_deq y x ;
          if ry > rx then Config.Rank.set x ry
          else if ry = rx then Config.Rank.set x (rx + 1) )
      with Not_found ->
        if rx > ry then merge ~propagate_deq x y
        else if ry > rx then merge ~propagate_deq y x
        else (
          merge ~propagate_deq x y ;
          Config.Rank.set y (ry + 1) ) ) ;
    assert (well_formed ())

  let install_d x y =
    assert (canonical x) ;
    assert (canonical y) ;
    assert (not (equal x y)) ;
    assert (not (diseq x y)) ;
    try
      let xs = Diseqs.find x (diseqs ()) in
      let xs' = Varset.copy xs in
      Varset.add y xs' ;
      Config.Diseqs.set x xs'
    with Not_found ->
      let xs' = Varset.singleton y in
      Config.Diseqs.set x xs'

  let separate x y =
    assert (well_formed ()) ;
    let x = find x and y = find y in
    if diseq x y then ()
    else (
      install_d x y ;
      install_d y x ;
      assert (well_formed ()) ;
      if Var.equal x y then raise Unsat )
end
