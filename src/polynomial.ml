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

module type COEFF = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val inv : t -> t
  val random : unit -> t
end

module type INDETERMINATE = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val fresh : unit -> t
  val dummy : t
end

module type P = sig
  type indet
  type coeff

  module Coeff : COEFF with type t = coeff
  module Indet : INDETERMINATE with type t = indet
  module Map : Maps.S with type key = indet and type value = coeff

  type t = private {constant: coeff; monomials: Map.t; mutable hash: int}

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val diseq : t -> t -> bool
  val hash : t -> int
  val mem : indet -> t -> bool
  val const : t -> coeff
  val coeff : indet -> t -> coeff
  val pp : Format.formatter -> t -> unit
  val constant : coeff -> t
  val zero : t
  val one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val indet : indet -> t
  val multc : coeff -> t -> t
  val addc : coeff -> t -> t
  val addm : coeff -> indet -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mapc : (coeff -> coeff) -> t -> t
  val minus : t -> t
  val negated : t -> t -> bool

  exception Valid
  exception Unsat

  val solve0 : t -> indet * t
  val solve0_for : indet -> t -> t
  val rename : indet -> indet -> t -> t
  val instantiate : indet -> coeff -> t -> t
  val pivot : indet -> indet -> t -> t
  val eval : (indet -> coeff) -> t -> coeff
  val iter : (coeff -> unit) -> (indet -> coeff -> unit) -> t -> unit
  val fold : (coeff -> 'a) -> (indet -> coeff -> 'a -> 'a) -> t -> 'a
  val map : (indet -> t) -> t -> t
  val replace : indet -> t -> t -> t
  val for_all : (coeff -> bool) -> (indet -> coeff -> bool) -> t -> bool
  val exists : (coeff -> bool) -> (indet -> coeff -> bool) -> t -> bool
  val choose : (indet -> coeff -> bool) -> t -> indet * coeff
  val make : coeff -> Map.t -> t

  exception Nonindet
  exception Nonnum

  val d_indet : t -> indet
  val is_indet : t -> bool
  val is_constant : t -> bool
  val d_constant : t -> coeff
  val is_monomial : t -> bool
  val coeff_of_monomial : t -> coeff
  val indet_of_monomial : t -> indet
end

module Make (C : COEFF) (X : INDETERMINATE) = struct
  type indet = X.t
  type coeff = C.t

  module Coeff = C
  module Indet = X
  module Set = Sets.Make (X)
  module Map = Maps.Make (X) (C)

  type t = {constant: C.t; monomials: Map.t; mutable hash: int}

  let ordered_monomials p =
    let n = Map.cardinal p.monomials in
    let a =
      if n = 0 then [||]
      else
        let x, c = Map.choose p.monomials in
        Array.make n (c, x)
    in
    let i = ref 0 in
    let add x c =
      assert (!i < n) ;
      a.(!i) <- (c, x) ;
      incr i
    in
    let cmp (_, x) (_, y) = X.compare x y in
    Map.iter add p.monomials ;
    assert (!i = n) ;
    Array.sort cmp a ;
    a

  let pp fmt p =
    Format.fprintf fmt "@[" ;
    ( if Map.is_empty p.monomials then C.pp fmt p.constant
    else
      let pp_monomial (c, x) =
        if C.equal c C.one then X.pp fmt x
        else (
          C.pp fmt c ;
          Format.fprintf fmt "*" ;
          X.pp fmt x )
      in
      let pp_signed_monomial (c, x) =
        if C.compare c C.zero > 0 then (
          Format.fprintf fmt " + " ;
          pp_monomial (c, x) )
        else (
          Format.fprintf fmt " - " ;
          pp_monomial (C.neg c, x) )
      in
      let ma = ordered_monomials p in
      let n = Array.length ma in
      let c0, x0 = ma.(0) in
      if C.equal p.constant C.zero then pp_monomial (c0, x0)
      else (
        C.pp fmt p.constant ;
        pp_signed_monomial (c0, x0) ) ;
      for i = 1 to n - 1 do
        pp_signed_monomial ma.(i)
      done ) ;
    Format.fprintf fmt "@]"

  let to_string x =
    pp Format.str_formatter x ;
    Format.flush_str_formatter ()

  let well_formed p =
    let nonneg _ c = not (C.equal c C.zero) in
    Map.for_all nonneg p.monomials

  let hash =
    let accumulate x c acc = C.hash c + X.hash x + acc in
    fun p ->
      if p.hash >= 0 then p.hash
      else
        let h1 = C.hash p.constant in
        let h2 = Map.fold accumulate p.monomials h1 in
        p.hash <- h2 land 0x3FFFFFFF ;
        p.hash

  let equal p1 p2 =
    assert (well_formed p1) ;
    assert (well_formed p2) ;
    hash p1 == hash p2
    (* Optimization. *)
    && C.equal p1.constant p2.constant
    && Map.equal p1.monomials p2.monomials

  let compare p1 p2 =
    assert (well_formed p1) ;
    assert (well_formed p2) ;
    let h1 = hash p1 and h2 = hash p2 in
    if h1 < h2 then -1
    else if h1 > h2 then 1
    else if
      C.equal p1.constant p2.constant && Map.equal p1.monomials p2.monomials
    then 0
    else Stdlib.compare p1 p2

  let diseq p1 p2 =
    assert (well_formed p1) ;
    assert (well_formed p2) ;
    (not (C.equal p1.constant p2.constant))
    && Map.equal p1.monomials p2.monomials

  let iter fc fm p =
    assert (well_formed p) ;
    fc p.constant ;
    Map.iter fm p.monomials

  let fold fc fm p =
    assert (well_formed p) ;
    Map.fold fm p.monomials (fc p.constant)

  module Interp = struct
    let apply rho x =
      try rho x
      with _ ->
        raise (Invalid_argument (Format.sprintf "Partial Interpretation"))

    let[@warning "-32"] random xs =
      let module Finite = Maps.Make (X) (C) in
      let acc = Finite.empty () in
      let gen x =
        let c = C.random () in
        Finite.set x c acc
      in
      Set.iter gen xs ;
      fun x -> Finite.find x acc
  end

  let eval rho p =
    assert (well_formed p) ;
    let fc c = c and fm x c = C.add (C.mul c (Interp.apply rho x)) in
    fold fc fm p

  let for_all fc fm p =
    assert (well_formed p) ;
    fc p.constant && Map.for_all fm p.monomials

  let exists fc fm p =
    assert (well_formed p) ;
    fc p.constant || Map.exists fm p.monomials

  let mem x p =
    assert (well_formed p) ;
    Map.mem x p.monomials

  let dom p =
    assert (well_formed p) ;
    let acc = Set.empty () in
    let add1 x _ = Set.add x acc in
    Map.iter add1 p.monomials ;
    acc

  (** Checks if the relation [rel \[v1;...;vn\] v] is not violated for a
      number of randomly generated values for the polynomials [q1;...;qn]
      and [p]. Used for debugging only. *)
  module Check = struct
    let probe = ref 10

    let valid rel ql p =
      assert (List.for_all well_formed ql) ;
      assert (well_formed p) ;
      let dom =
        let acc = dom p in
        List.iter (fun q -> Set.union (dom q) acc) ql ;
        acc
      in
      let generate () =
        let acc = Map.empty () in
        let add1 x = Map.set x (C.random ()) acc in
        Set.iter add1 dom ;
        acc
      in
      let sat1 () =
        let rho = generate () in
        let value = eval (fun x -> Map.find x rho) in
        rel (List.map value ql) (value p)
      in
      let rec every i = if i < 0 then true else sat1 () && every (i - 1) in
      every !probe

    let valid2 rel q1 q2 p =
      let rel2 = function [v1; v2] -> rel v1 v2 | _ -> assert false in
      valid rel2 [q1; q2] p

    let valid1 rel q p =
      let rel1 = function [v] -> rel v | _ -> assert false in
      valid rel1 [q] p

    let equal p q = valid1 C.equal p q

    let add p1 p2 p =
      let add_rel v1 v2 v = C.equal (C.add v1 v2) v in
      let res = valid2 add_rel p1 p2 p in
      if not res then
        Format.eprintf "\nFailed check: add(%s, %s, %s)@?" (to_string p1)
          (to_string p2) (to_string p) ;
      res

    let equiv (p1, q1) (p2, q2) = equal p1 q1 = equal p2 q2
  end

  let length p =
    assert (well_formed p) ;
    let n = ref 0 in
    let count _ _ = incr n in
    Map.iter count p.monomials ;
    !n

  let const p =
    assert (well_formed p) ;
    p.constant

  let coeff x p =
    assert (well_formed p) ;
    try Map.find x p.monomials with Not_found -> C.zero

  let[@warning "-32"] dom x p = not (C.equal C.zero (coeff x p))

  (* exception Found
   * 
   * let choose =
   *   let coeff = ref (Obj.magic 0) and indet = ref (Obj.magic 0) in
   *   fun f p ->
   *     assert (well_formed p) ;
   *     let choose_m x c =
   *       if f x c then (
   *         coeff := c ;
   *         indet := x ;
   *         raise Found )
   *     in
   *     try
   *       Map.iter choose_m p.monomials ;
   *       raise Not_found
   *     with Found -> (!indet, !coeff) *)

  let choose f p = Map.choose_if f p.monomials

  let make c m =
    let p = {constant= c; monomials= m; hash= -1} in
    assert (well_formed p) ;
    p

  let is_constant p = Map.is_empty p.monomials
  let represents c p = is_constant p && C.equal c p.constant
  let is_zero = represents C.zero
  let is_one = represents C.one

  let constant =
    let empty = Map.empty () in
    let module Cache = Ephemeron.K1.Make (C) in
    let table = Cache.create 17 in
    fun c ->
      try Cache.find table c
      with Not_found ->
        let p = make c empty in
        assert (represents c p) ;
        assert (not (Cache.mem table c)) ;
        Cache.add table c p ;
        p

  let zero = constant C.zero
  let one = constant C.one

  exception Nonnum

  let d_constant p =
    assert (well_formed p) ;
    if Map.is_empty p.monomials then p.constant else raise Nonnum

  let indet =
    let module Cache = Ephemeron.K1.Make (X) in
    let table = Cache.create 17 in
    fun x ->
      try Cache.find table x
      with Not_found ->
        let p = make C.zero (Map.singleton x C.one) in
        assert (not (Cache.mem table x)) ;
        Cache.add table x p ;
        p

  exception Nonindet

  let d_indet p =
    assert (well_formed p) ;
    let top _ _ = true in
    if C.equal C.zero p.constant && Map.is_singleton p.monomials then
      let x, c = Map.choose_if top p.monomials in
      if C.equal C.one c then x else raise Nonindet
    else raise Nonindet

  let is_indet p =
    assert (well_formed p) ;
    try
      let _ = d_indet p in
      true
    with Nonindet -> false

  let is_monomial p =
    assert (well_formed p) ;
    C.equal C.zero p.constant && Map.is_singleton p.monomials

  let coeff_of_monomial p =
    assert (is_monomial p) ;
    let is_true _ _ = true in
    let _, c = Map.choose_if is_true p.monomials in
    c

  let indet_of_monomial p =
    assert (is_monomial p) ;
    let is_true _ _ = true in
    let x, _ = Map.choose_if is_true p.monomials in
    x

  let[@warning "-32"] monomial c x =
    if C.equal C.zero c then zero
    else if C.equal C.one c then indet x
    else make C.zero (Map.singleton x c)

  let multc c p =
    assert (well_formed p) ;
    if C.equal C.zero c then zero
    else if C.equal C.one c then p
    else
      let mc = C.mul c in
      let q = make (mc p.constant) (Map.map mc p.monomials) in
      assert (well_formed q) ;
      assert (
        let mult_rel u v = C.equal (mc u) v in
        Check.valid1 mult_rel p q ) ;
      q

  let neg p =
    assert (well_formed p) ;
    let q =
      let c' = C.neg p.constant in
      let m' = Map.empty () in
      let invert x c = Map.set x (C.neg c) m' in
      Map.iter invert p.monomials ;
      make c' m'
    in
    assert (well_formed q) ;
    assert (
      let neg_rel u v = C.equal (C.neg u) v in
      Check.valid1 neg_rel p q ) ;
    q

  let addc c p =
    assert (well_formed p) ;
    if C.equal C.zero c then p
    else
      let q = make (C.add c p.constant) p.monomials in
      assert (well_formed q) ;
      assert (
        let addc_rel u v = C.equal (C.add c u) v in
        Check.valid1 addc_rel p q ) ;
      q

  let incr = addc C.one
  let[@warning "-32"] decr = addc (C.neg C.one)

  let addm c x p =
    assert (well_formed p) ;
    if C.equal C.zero c then p
    else
      let m = Map.copy p.monomials in
      try
        let d = Map.find x m in
        let cd = C.add c d in
        if C.equal cd C.zero then Map.remove x m else Map.set x cd m ;
        make p.constant m
      with Not_found ->
        Map.set x c m ;
        make p.constant m

  let add p1 p2 =
    assert (well_formed p1) ;
    assert (well_formed p2) ;
    let addp p1 p2 =
      assert (length p1 >= length p2) ;
      let c = C.add p1.constant p2.constant
      and m =
        let acc = Map.copy p1.monomials in
        let add1 x c =
          assert (not (C.equal c C.zero)) ;
          try
            let d = Map.find x acc in
            let cd = C.add c d in
            if C.equal C.zero cd then Map.remove x acc else Map.set x cd acc
          with Not_found -> Map.set x c acc
        in
        Map.iter add1 p2.monomials ;
        acc
      in
      make c m
    in
    let p =
      try addc (d_constant p1) p2
      with Nonnum -> (
        try addc (d_constant p2) p1
        with Nonnum ->
          let n1 = length p1 and n2 = length p2 in
          if n1 >= n2 then addp p1 p2 else addp p2 p1 )
    in
    assert (well_formed p) ;
    assert (Check.add p1 p2 p) ;
    p

  let sub p1 p2 =
    assert (well_formed p1) ;
    assert (well_formed p2) ;
    try addc (C.neg (d_constant p2)) p1
    with Nonnum ->
      let c' = C.sub p1.constant p2.constant in
      let m' = Map.copy p1.monomials in
      let subtract x c =
        assert (not (C.equal c C.zero)) ;
        try
          let d = Map.find x m' in
          let cd = C.sub c d in
          if C.equal C.zero cd then Map.remove x m' else Map.set x cd m'
        with Not_found -> Map.set x (C.neg c) m'
      in
      Map.iter subtract p2.monomials ;
      let p = make c' m' in
      assert (well_formed p) ;
      assert (
        let sub_rel v1 v2 v = C.equal (C.sub v1 v2) v in
        Check.valid2 sub_rel p1 p2 p ) ;
      p

  let mapc f p =
    assert (well_formed p) ;
    let c' = f p.constant and m' = Map.empty () in
    let addm x d =
      let d' = f d in
      if C.equal C.zero d then () else Map.set x d' m'
    in
    Map.iter addm p.monomials ;
    make c' m'

  let minus p =
    let q = mapc C.neg p in
    assert (
      let min_rel u v = C.equal (C.neg u) v in
      Check.valid1 min_rel p q ) ;
    q

  let negated p q = equal (minus p) q

  (* let c = p.constant and d = q.constant in
   * let ml = p.monomials and nl = q.monomials in
   * C.equal (C.neg c) d && failwith "negated: to do" *)

  let map f p =
    let c = p.constant and ms = p.monomials in
    if Map.is_empty ms then p
    else
      let accumulate x d =
        try add (multc d (f x)) with _ -> add (multc d (indet x))
      in
      Map.fold accumulate ms (constant c)

  let replace x p q =
    let lookup y = if X.equal x y then p else raise Not_found in
    map lookup q

  let rename x y p =
    assert (well_formed p) ;
    if X.equal x y then p
    else
      let q =
        try
          let c = Map.find x p.monomials in
          let m' = Map.copy p.monomials in
          Map.remove x m' ;
          ( try
              let d = Map.find y p.monomials in
              assert (not (C.equal C.zero d)) ;
              let cd = C.add c d in
              if C.equal C.zero cd then Map.remove y m' else Map.set y cd m'
            with Not_found -> Map.set y c m' ) ;
          make p.constant m'
        with Not_found -> p
      in
      assert (not (mem x q)) ;
      q

  let instantiate x c p =
    let k = coeff x p in
    if C.equal C.zero k then p
    else
      let d' = C.add p.constant (C.mul k c) in
      let mp' = Map.copy p.monomials in
      Map.remove x mp' ;
      make d' mp'

  let[@warning "-32"] subst p x q =
    assert (well_formed p) ;
    assert (well_formed q) ;
    assert (not (mem x q)) ;
    try rename x (d_indet q) p
    with Nonindet -> (
      try
        let d = Map.find x p.monomials in
        let c' = C.add p.constant q.constant in
        let m' = Map.copy p.monomials in
        let addm z e =
          try
            let f = Map.find z m' in
            let g = C.add f (C.mul d e) in
            if C.equal C.zero g then Map.remove z m' else Map.set z g m'
          with Not_found ->
            let de = C.mul d e in
            if C.equal C.zero de then () else Map.set z de m'
        in
        Map.remove x m' ;
        Map.iter addm q.monomials ;
        let p' = make c' m' in
        assert (well_formed p') ;
        p'
      with Not_found -> p )

  exception Valid
  exception Unsat

  let solve0 p =
    assert (well_formed p) ;
    let top _ _ = true in
    let c = p.constant in
    try
      (* [c + d*x + m = 0] <=> [x = -c/d -1/d*m] *)
      let x, d, m = Map.destruct top p.monomials in
      assert (not (C.equal C.zero d)) ;
      let neginvd = C.neg (C.inv d) in
      let c' = C.mul c neginvd in
      if Map.is_empty m then (x, constant c')
      else
        let m' = Map.empty () in
        let mult_neginvd x e = Map.set x (C.mul neginvd e) m' in
        Map.iter mult_neginvd m ;
        let q = make c' m' in
        assert (well_formed q) ;
        assert (Check.equiv (p, zero) (indet x, q)) ;
        (x, q)
    with Not_found ->
      assert (is_constant p) ;
      if is_zero p then raise Valid else raise Unsat

  let solve0_for x p =
    assert (mem x p) ;
    let d = coeff x p in
    let c = const p in
    (* [c + d * x + q = 0]. *)
    let q =
      let pm = Map.copy p.monomials in
      Map.remove x pm ;
      pm
    in
    (* ==> [x = - 1/d * (q + c)]. *)
    multc (C.neg (C.inv d)) (make c q)

  (** Transform [x = p\[y\]] to equivalent [y = q\[x\]]. *)
  let pivot x y p =
    assert (well_formed p) ;
    assert (not (mem x p)) ;
    assert (mem y p) ;
    let c = p.constant and m = p.monomials in
    try
      (* [x = c + d*y + m'] ==> [y = 1/d*x + (-c/d - 1/d*m')] *)
      let d = Map.find y m in
      let invd = C.inv d in
      let neginvd = C.neg invd in
      let c' = C.mul c neginvd in
      let m' = Map.empty () in
      let add z e =
        if X.equal z y then () else Map.set z (C.mul e neginvd) m'
      in
      Map.iter add m ;
      let q = addm invd x (make c' m') in
      assert (well_formed q) ;
      assert (mem x q) ;
      assert (Check.equiv (indet x, p) (indet y, q)) ;
      q
    with Not_found -> raise (Invalid_argument "Polynomial.pivot")
end

(* module Lazy (Poly : P) =
 * (* : P with type coeff = Poly.C.t and type indet = Poly.X.t *)
 * struct
 *   module Coeff = Poly.Coeff
 *   module Indet = Poly.Indet
 * 
 *   type coeff = Coeff.t
 *   type indet = Indet.t
 * 
 *   type t =
 *     | Inj of Poly.t
 *     | Replace of Poly.X.t * Poly.X.t * Poly.t
 *     | Inst of Poly.X.t * Poly.C.t * Poly.t
 *     | Subst of Poly.X.t * Poly.t * Poly.t
 *     | Pivot of Poly.X.t * Poly.X.t * Poly.t
 * 
 *   let rec to_poly = function
 *     | Inj p -> p
 *     | Replace (x, y, p) -> Poly.replace x y (to_poly p)
 *     | Inst (x, c, p) -> Poly.instantiate x c (to_poly p)
 *     | Subst (x, p, q) -> Poly.apply x p (to_poly q)
 *     | Pivot (x, y, p) -> Poly.pivot x y (to_poly p)
 * end *)

(**/**)

(** Following for debugging only. *)
module Test = struct
  [@@@warning "-32"]

  module C = struct
    let max_random = ref 20

    let random =
      let initialized = ref false in
      fun () ->
        if not !initialized then (
          Random.self_init () ;
          initialized := true ) ;
        let d = Random.int !max_random and n = Random.int !max_random in
        if n = 0 then Q.of_int d else Q.of_ints d n

    include Q

    let pp = pp_print
    let hash = Hashtbl.hash
  end

  module X = struct
    type t = int

    let equal = ( == )
    let compare = Stdlib.compare
    let hash i = i
    let pp fmt i = Format.fprintf fmt "x[%d]" i
    let fresh () = Random.int 34
    let dummy = 0
  end

  module P = Make (C) (X)

  let to_string x =
    P.pp Format.str_formatter x ;
    Format.flush_str_formatter ()

  let numofprobes = ref 10000
  let numofpoly = ref !numofprobes
  let maxvar = ref 5
  let maxcoeff = ref 7
  let init = Random.self_init

  module Heap = struct
    let max = ref 1
    let heap = Array.make !numofpoly P.zero

    let alloc p =
      assert (!max < !numofpoly - 1) ;
      incr max ;
      heap.(!max) <- p

    let lookup i = heap.(i)
    let set i p = heap.(i) <- p
  end

  module Random = struct
    let int = Random.int
    let variable () = int !maxvar
    let coeff () = C.random ()

    let poly () =
      let i = int !Heap.max in
      Heap.lookup i
  end

  module Ops = struct
    let incr () =
      let p = Random.poly () in
      Format.eprintf "\nincr <-- %s@?" (to_string p) ;
      let q = P.incr p in
      Format.eprintf "\nincr --> %s@?" (to_string q) ;
      Heap.alloc q

    let add () =
      let p = Random.poly () in
      let q = Random.poly () in
      Format.eprintf "\nadd <-- %s  %s@?" (to_string p) (to_string q) ;
      let r = P.add p q in
      Format.eprintf "\nadd --> %s@?" (to_string r) ;
      Heap.alloc r

    let addm () =
      let c = Random.coeff () in
      let x = Random.variable () in
      let p = Random.poly () in
      Format.eprintf "\naddm <-- %s %s %s@?" (C.to_string c)
        (to_string (P.indet x))
        (to_string p) ;
      let q = P.addm c x p in
      Format.eprintf "\naddm --> %s@?" (to_string q) ;
      Heap.alloc q

    let addc () =
      let c = Random.coeff () in
      let p = Random.poly () in
      Format.eprintf "\naddc <-- %s %s@?" (C.to_string c) (to_string p) ;
      let q = P.addc c p in
      Format.eprintf "\naddc --> %s@?" (to_string q) ;
      Heap.alloc q

    let multc () =
      let c = Random.coeff () in
      let p = Random.poly () in
      Format.eprintf "\nmultc <-- %s %s@?" (C.to_string c) (to_string p) ;
      let q = P.multc c p in
      Format.eprintf "\nmultc --> %s@?" (to_string q) ;
      Heap.alloc q

    let neg () =
      let p = Random.poly () in
      Heap.alloc (P.neg p)

    let sub () =
      let p = Random.poly () in
      let q = Random.poly () in
      Format.eprintf "\nsub <-- %s  %s@?" (to_string p) (to_string q) ;
      let r = P.sub p q in
      Format.eprintf "\nsub --> %s@?" (to_string r) ;
      Heap.alloc r

    let solve0 () =
      let p = Random.poly () in
      try
        Format.eprintf "\nsolve <-- %s@?" (to_string p) ;
        let x, _q = P.solve0 p in
        Format.eprintf "\nsolve --> %s = %s@?"
          (to_string (P.indet x))
          (to_string p) ;
        ()
      with _ -> ()
  end

  let run () =
    let apply () =
      match Random.int 10 with
      | 0 -> Ops.incr ()
      | 1 -> Ops.add ()
      | 2 -> Ops.addc ()
      | 3 -> Ops.multc ()
      | 4 -> Ops.addc ()
      | 5 -> Ops.multc ()
      | 6 -> Ops.neg ()
      (* | 7 -> Ops.sub() *)
      | 8 -> Ops.solve0 ()
      | _ -> Ops.addm ()
    in
    for _ = 0 to !numofprobes do
      apply ()
    done ;
    Format.eprintf "sets: self test ok.@?"
end
