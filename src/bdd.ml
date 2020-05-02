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
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val dummy : t
end

module type FML = sig
  type var
  type t

  val pp : bool * bool * int -> Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val mk_true : t
  val mk_false : t
  val mk_posvar : var -> t
  val mk_negvar : var -> t
  val mk_neg : t -> t
  val mk_ite : t -> t -> t -> t
  val mk_imp : t -> t -> t
  val mk_conj : t -> t -> t
  val mk_disj : t -> t -> t
  val mk_iff : t -> t -> t
  val mk_xor : t -> t -> t
  val cofactor_pos : t -> var -> t
  val cofactor_neg : t -> var -> t
  val implies : t -> t -> bool
  val contrad : t -> t -> bool
  val cond : t -> var
  val pos : t -> t
  val neg : t -> t
  val is_valid : t -> bool
  val is_unsat : t -> bool
  val is_ite : t -> bool
  val occurs : var -> t -> bool
  val union : var -> var -> t -> t
  val separate : var -> var -> t -> t
  val implicant : t -> (bool * var) Queue.t -> unit
  val and_elim : t -> var list * var list * t
end

module Make (Var : VAR) = struct
  type var = Var.t

  (* Switch between static and dynamic ordering *)
  (* Note: The [static] variable ordering seems to be much more efficient. *)
  let dynamic_order = false

  (* Enable path compression for dynamic ordering *)
  let path_compression = false

  let _ =
    assert (
      Format.eprintf
        "\n BDD checks  = true; dynamicOrder = %b; pathCompression = %b@?"
        dynamic_order path_compression ;
      true )

  (** Dynamic variable ordering (see [topvar]). *)
  module Ge = struct
    module Vars = Sets.Make (Var)
    module Table = Ephemeron.K1.Make (Var)

    let table = Table.create 7

    (** Immediated successors of [x] in [>>] relation. *)
    let succ =
      let empty = Vars.empty () in
      fun x ->
        try Table.find table x
        with Not_found ->
          assert (Vars.is_empty empty) ;
          empty

    let store x y =
      try
        let zs = Table.find table x in
        Vars.add y zs ;
        Table.replace table x zs
      with Not_found -> Table.add table x (Vars.singleton y)

    (** [x >= y] holds iff

        - [x = y] aor
        - [y] in [succ(x)] or there is [z] in [succ(x)] and [z >= y]. *)
    let rec holds x y =
      Var.equal x y
      ||
      let zl = succ x in
      Vars.exists (Var.equal y) zl
      ||
      let hld z = holds z y in
      let res = Vars.exists hld zl in
      if path_compression && res then (* path compression *)
        store x y ;
      res

    type cmp = True | False | Unknown

    let compare x y =
      if holds x y then True else if holds y x then False else Unknown

    let extend x y =
      assert (not (holds x y)) ;
      store x y

    let add x y = if holds x y then () else extend x y

    module Succ = Maps.Make (Var) (Vars)

    let to_map () =
      let acc = Succ.empty () in
      let add x ys = Succ.set x ys acc in
      Table.iter add table ;
      acc
  end

  let ( >>= ) x y =
    if dynamic_order then Ge.holds x y else Var.compare x y >= 0

  type t =
    {mutable guard: Var.t; mutable pos: t; mutable neg: t; mutable hash: int}

  type bdd = t (* nickname *)

  let rec dummy = {guard= Var.dummy; pos= dummy; neg= dummy; hash= 0}
  let mk_false = {dummy with hash= 0}
  let mk_true = {dummy with hash= 1}
  let is_true b = b == mk_true
  let is_false b = b == mk_false
  let is_valid = is_true
  let is_unsat = is_false
  let is_const b = is_true b || is_false b
  let is_ite b = not (is_const b)
  let is_posvar b = is_ite b && is_true b.pos && is_false b.neg
  let is_negvar b = is_ite b && is_false b.pos && is_true b.neg
  let is_literal b = is_ite b && is_const b.pos && is_const b.neg

  let cond b =
    assert (is_ite b) ;
    b.guard

  let pos b =
    assert (is_ite b) ;
    b.pos

  let neg b =
    assert (is_ite b) ;
    b.neg

  let to_var b =
    assert (is_literal b) ;
    b.guard

  (** Hash values are hashed, and negative hash slot incicates that the hash
      has not been computed. Hashs of subtrees are weighted differently so
      that bdds and the negation have different hashs. *)
  let rec hash b = if is_const b then b.hash else hash_node b

  and hash_node n =
    if n.hash >= 0 then n.hash
    else (
      n.hash <- (Var.hash n.guard + hash n.pos + hash n.neg) land 0x3FFFFFFF ;
      n.hash )

  let equal = ( == )

  type occ = Once | Multiple of int

  let occurrences =
    let bndng = ref 0 in
    let table = Hashtbl.create 7 in
    fun b ->
      Hashtbl.clear table ;
      bndng := 0 ;
      let add b =
        if is_ite b then
          try
            match Hashtbl.find table b with
            | Once ->
                Hashtbl.add table b (Multiple !bndng) ;
                incr bndng
            | Multiple _ -> ()
          with Not_found -> Hashtbl.add table b Once
      in
      let rec fill b =
        if is_ite b then (
          fill b.pos ;
          fill b.neg ;
          add b )
      in
      fill b ;
      table

  let binding table b =
    match Hashtbl.find table b with
    | Once -> raise Not_found
    | Multiple n -> n

  exception Witness

  let some_binding table =
    let test _ = function Multiple _ -> raise Witness | Once -> () in
    try
      Hashtbl.iter test table ;
      false
    with Witness -> true

  let compare b1 b2 =
    let h1 = hash b1 and h2 = hash b2 in
    if h1 > h2 then 1
    else if h1 < h2 then -1
    else if b1 == b2 then 0
    else Stdlib.compare b1 b2

  let infix = ref false

  (* print terms in shared form. *)
  let shared = ref true

  (* negative value means there is no [maxdepth] set. *)
  let maxdepth = ref (-1)

  let rec pp_term depth table fmt b =
    if is_valid b then Format.fprintf fmt "true"
    else if is_unsat b then Format.fprintf fmt "false"
    else
      try
        if not !shared then raise Not_found ;
        let bndng = binding table b in
        Format.fprintf fmt "var(%d)" bndng
      with Not_found ->
        if !maxdepth >= 0 && depth >= !maxdepth then
          Format.fprintf fmt "..."
        else pp_ite depth table fmt b

  and pp_ite depth table fmt b =
    if not (is_ite b) then invalid_arg "Bdd.ppIte: not an if-then-else expr" ;
    if !infix && is_unsat b.pos && is_valid b.neg then (
      Format.fprintf fmt "~" ;
      Var.pp fmt b.guard )
    else if !infix && is_unsat b.neg then (
      Var.pp fmt b.guard ;
      if not (is_valid b.pos) then (
        Format.fprintf fmt " & " ;
        pp_term (depth + 1) table fmt b.pos ) )
    else if !infix && is_valid b.pos then (
      Var.pp fmt b.guard ;
      if not (is_unsat b.neg) then (
        Format.fprintf fmt " | " ;
        pp_term (depth + 1) table fmt b.neg ) )
    else (
      Format.fprintf fmt "@[ite(" ;
      Var.pp fmt b.guard ;
      Format.fprintf fmt ", " ;
      pp_term (depth + 1) table fmt b.pos ;
      Format.fprintf fmt ", " ;
      pp_term (depth + 1) table fmt b.neg ;
      Format.fprintf fmt ")@]" )

  let pp_defs fmt table =
    if some_binding table then (
      Format.fprintf fmt "\nwhere " ;
      let pp_binding b = function
        | Once -> ()
        | Multiple n ->
            Format.fprintf fmt "\n  var(%d) := " n ;
            pp_ite 0 table fmt b
      in
      Hashtbl.iter pp_binding table )

  let pp (infix_curr, shared_curr, maxdepth_curr) fmt b =
    let[@warning "-26"] save_infix = !infix in
    let[@warning "-26"] save_shared = !shared in
    let[@warning "-26"] save_maxdepth = !maxdepth in
    infix := infix_curr ;
    shared := shared_curr ;
    maxdepth := maxdepth_curr ;
    let table = if !shared then occurrences b else Hashtbl.create 0 in
    pp_term 0 table fmt b ;
    if !shared then pp_defs fmt table ;
    Format.fprintf fmt "@?"

  let to_string b =
    pp (false, false, -1) Format.str_formatter b ;
    Format.flush_str_formatter ()

  let occurs x =
    let rec occx b =
      if is_const b then false
      else Var.equal x b.guard || occx b.pos || occx b.neg
    in
    occx

  let less b p =
    is_const b
    ||
    let q = b.guard in
    (not (Var.equal p q)) && p >>= q

  let rec ordered b =
    is_const b
    || (not (b.pos == b.neg))
       && less b.pos b.guard
       && less b.neg b.guard
       && ordered b.pos
       && ordered b.neg

  let ordered b =
    let res = ordered b in
    if not res then (
      Format.eprintf "Error: not ordered: " ;
      pp (false, false, -1) Format.err_formatter b ;
      if dynamic_order then (
        Format.eprintf "\n Succ: " ;
        Ge.Succ.pp Format.err_formatter (Ge.to_map ()) ) ;
      Format.eprintf "@?" ) ;
    res

  module Node = struct
    type t = bdd

    let equal n m =
      assert ((not (is_const n)) && not (is_const m)) ;
      Var.equal n.guard m.guard && n.pos == m.pos && n.neg == m.neg

    let hash n =
      assert (not (is_const n)) ;
      hash_node n
  end

  let mk_posvar =
    let dummy = {mk_true with hash= -1} in
    let module Cache = Weak.Make (Node) in
    let cache = Cache.create 17 in
    fun p ->
      dummy.guard <- p ;
      dummy.hash <- -1 ;
      try Cache.find cache dummy
      with Not_found ->
        let b = {guard= p; pos= mk_true; neg= mk_false; hash= -1} in
        assert (ordered b) ;
        Cache.add cache b ;
        b

  let mk_negvar =
    let dummy = {mk_false with hash= -1} in
    let module Cache = Weak.Make (Node) in
    let cache = Cache.create 17 in
    fun p ->
      dummy.guard <- p ;
      dummy.hash <- -1 ;
      try Cache.find cache dummy
      with Not_found ->
        let b = {guard= p; pos= mk_false; neg= mk_true; hash= -1} in
        assert (ordered b) ;
        Cache.add cache b ;
        b

  (** Hashconsed construction of ITE terms. *)
  let hashconsed_ite =
    let dummy = {mk_true with hash= -1} in
    let module Cache = Weak.Make (Node) in
    let cache = Cache.create 17 in
    fun g pos neg ->
      assert (ordered pos && ordered neg) ;
      assert (less pos g && less neg g) ;
      if is_true pos && is_false neg then mk_posvar g
      else if is_false pos && is_true neg then mk_negvar g
      else (
        dummy.guard <- g ;
        dummy.pos <- pos ;
        dummy.neg <- neg ;
        dummy.hash <- -1 ;
        try Cache.find cache dummy
        with Not_found ->
          let b = {guard= g; pos; neg; hash= dummy.hash} in
          assert (ordered b) ;
          Cache.add cache b ;
          b )

  (** The positive cofactor of [b] wrt. variable [p] is [b\[p:=True\]]. *)
  let cofactor_pos b p =
    assert (ordered b) ;
    let rec cofactor b =
      assert (ordered b) ;
      if is_const b then b
      else
        let q = b.guard in
        let cmp = Var.compare p q in
        if cmp = 0 then b.pos
        else if cmp > 0 then b
        else
          let pos' = cofactor b.pos in
          let neg' = cofactor b.neg in
          assert (not (occurs p pos')) ;
          assert (not (occurs p neg')) ;
          if b.pos == pos' && b.neg == neg' then b
          else hashconsed_ite q pos' neg'
    in
    let b' = cofactor b in
    assert (not (occurs p b')) ;
    assert (ordered b') ;
    b'

  (** The negative cofactor of [b] wrt. variable [p] is [b\[p:=False\]]. *)
  let cofactor_neg b p =
    assert (ordered b) ;
    let rec cofactor b =
      assert (ordered b) ;
      if is_const b then b
      else
        let q = b.guard in
        let cmp = Var.compare p q in
        if cmp = 0 then b.neg
        else if cmp > 0 then b
        else
          let pos' = cofactor b.pos in
          let neg' = cofactor b.neg in
          assert (not (occurs p pos')) ;
          assert (not (occurs p neg')) ;
          if b.pos == pos' && b.neg == neg' then b
          else hashconsed_ite q pos' neg'
    in
    let b' = cofactor b in
    assert (not (occurs p b')) ;
    assert (ordered b') ;
    b'

  (** Build a BDD using Shannon expansion:
      [ite(p,pos,neg) = p & cofactor_pos(pos,p) | ~p & cofactor_neg(neg,p)]. *)
  module Build = struct
    module Fml = struct
      type t = bdd

      let hash = hash
      let equal = equal
    end

    module Table = Ephemeron.Kn.Make (Fml)

    let table = Table.create 17
    let dummy = [|dummy; dummy; dummy|]

    let fill_dummy a b c =
      dummy.(0) <- a ;
      dummy.(1) <- b ;
      dummy.(2) <- c

    let cof_pos b p =
      assert (ordered b) ;
      if is_const b then b
      else
        let b' = if Var.equal p b.guard then b.pos else b in
        assert (less b' p) ;
        b'

    let cof_neg b p =
      assert (ordered b) ;
      if is_const b then b
      else
        let b' = if Var.equal p b.guard then b.neg else b in
        assert (less b' p) ;
        b'

    let max p q = if Var.compare p q >= 0 then p else q

    let max2 prefer p q =
      match Ge.compare p q with
      | Ge.True -> p
      | Ge.False -> q
      | Ge.Unknown ->
          if prefer then (
            (* prefer [p] over [q]. *)
            Ge.add p q ;
            p )
          else if Var.compare p q >= 0 then (
            Ge.add p q ;
            p )
          else (
            Ge.add q p ;
            q )

    let max3 prefer p1 p2 p3 =
      match (Ge.compare p2 p1, Ge.compare p3 p1) with
      | Ge.True, Ge.True -> max2 prefer p2 p3
      | Ge.False, Ge.False -> p1
      | Ge.Unknown, Ge.Unknown ->
          max2 prefer (max2 prefer p1 p2) (max2 prefer p1 p3)
      | Ge.True, Ge.False -> p2
      | Ge.False, Ge.True -> p3
      | Ge.False, Ge.Unknown -> max2 prefer p1 p3
      | Ge.True, Ge.Unknown -> max2 prefer p2 p3
      | Ge.Unknown, Ge.False -> max2 prefer p1 p2
      | Ge.Unknown, Ge.True -> max2 prefer p3 p2

    let topvar_static p1 b2 b3 =
      match (is_const b2, is_const b3) with
      | true, true -> p1
      | true, false -> max p1 b3.guard
      | false, true -> max p1 b2.guard
      | false, false -> max p1 (max b2.guard b3.guard)

    (* Maximal variable wrt to current variable ordering. Prefers variable
       [p1]; builds up dynamic variable ordering. *)
    let topvar_dynamic prefer p1 b2 b3 =
      match (is_const b2, is_const b3) with
      | false, false -> max3 prefer p1 b2.guard b3.guard
      | false, true -> max2 prefer p1 b2.guard
      | true, false -> max2 prefer p1 b3.guard
      | true, true -> p1

    let topvar prefer p1 b2 b3 =
      if dynamic_order then topvar_dynamic prefer p1 b2 b3
      else topvar_static p1 b2 b3

    let rec build_var p b2 b3 =
      assert (ordered b2) ;
      assert (ordered b3) ;
      if b2 == b3 then b2
      else if is_ite b2 && Var.equal p b2.guard then
        (* [ite(p, ite(p, x, _), b3) = ite(p, x, b3)]. *)
        build_var p b2.pos b3
      else if is_ite b3 && Var.equal p b3.guard then
        (* [ite(p, b2, ite(p, _, y)) = ite(p, b2, y)]. *)
        build_var p b2 b3.neg
      else
        let max = topvar true p b2 b3 in
        if Var.equal max p then (
          assert (less b2 max) ;
          assert (less b3 max) ;
          hashconsed_ite max b2 b3 )
        else
          let pos = make (mk_posvar p) (cof_pos b2 max) (cof_pos b3 max) in
          let neg = make (mk_posvar p) (cof_neg b2 max) (cof_neg b3 max) in
          assert (less pos max) ;
          assert (less neg max) ;
          if pos == neg then pos else hashconsed_ite max pos neg

    (** Select {i top predicate} and build recursively using positive and
        negative cofactors. The following equalities are applied from
        left-to-right to bring a variable to the left-most position:

        - commConjP [ite(b1,true,ite(b3,true,false)) = ite(b3,true,b1)]
        - commConjN [ite(b1,ite(b2,true,false),false) = ite(b2,b1,false)]
        - commDisjP [ite(b1,true,ite(b3,false,true)) = ite(b3,b1,true)]
        - commDisjN [ite(b1, ite(b2,false,true),false) = ite(b2,false,b1)]
        - contraImpP
          [ite(b1,ite(b2,true,false),true) =
          ite(b2,true,ite(b1,false,true))]
        - contraImpN
          [ite(b1,ite(b2,false,true),true) =
          ite(b2,ite(b1,false,true),true)]
        - commXorP
          [ite(b1,ite(b2,false,true),ite(b2,true,false)) =
          ite(b2,ite(b1,false,true),b1)]
        - commXorN
          [ite(b1,ite(b2,true,false),ite(b2,false,true)) =
          ite(b2,b1,ite(b1,false,true))] *)
    and build b1 b2 b3 =
      assert (ordered b1) ;
      assert (ordered b2) ;
      assert (ordered b3) ;
      if b2 == b3 then b2
      else if is_true b1 then b2
      else if is_false b1 then b3
      else if is_posvar b1 then build_var (to_var b1) b2 b3
      else if is_negvar b1 then build_var (to_var b1) b3 b2
      else if is_false b2 && is_true b3 then
        build_var b1.guard b1.neg b1.pos
      else if is_true b2 && is_posvar b3 then
        build_var (to_var b3) mk_true b1
      else if is_false b3 && is_posvar b2 then
        build_var (to_var b2) b1 mk_false
      else if is_true b2 && is_negvar b3 then
        build_var (to_var b3) b1 mk_true
      else if is_false b3 && is_negvar b2 then
        build_var (to_var b2) mk_false b1
      else if is_true b3 && is_posvar b2 then
        build_var (to_var b2) mk_true (build b1 mk_false mk_true)
      else if is_true b3 && is_negvar b2 then
        build_var (to_var b2) (build b1 mk_false mk_true) mk_true
      else if
        is_negvar b2 && is_posvar b3 && Var.equal (to_var b2) (to_var b3)
      then build_var (to_var b2) (build b1 mk_false mk_true) b1
      else if
        is_posvar b2 && is_negvar b3 && Var.equal (to_var b2) (to_var b3)
      then build_var (to_var b2) b1 (build b1 mk_false mk_true)
      else
        let max = topvar false b1.guard b2 b3 in
        let pos = make (cof_pos b1 max) (cof_pos b2 max) (cof_pos b3 max) in
        let neg = make (cof_neg b1 max) (cof_neg b2 max) (cof_neg b3 max) in
        assert (less pos max) ;
        assert (less neg max) ;
        if pos == neg then pos else hashconsed_ite max pos neg

    (** Memoized build. *)
    and make b1 b2 b3 =
      assert (ordered b1) ;
      assert (ordered b2) ;
      assert (ordered b3) ;
      if b2 == b3 then b2
      else (
        fill_dummy b1 b2 b3 ;
        try Table.find table dummy
        with Not_found ->
          let b = build b1 b2 b3 in
          Table.add table [|b1; b2; b3|] b ;
          assert (ordered b) ;
          b )
  end

  module Check = struct
    module Bool = struct
      type t = bool

      let equal = ( = )
      let random () = Random.bool ()

      let pp fmt = function
        | true -> Format.fprintf fmt "true"
        | false -> Format.fprintf fmt "false"
    end

    module Ite = struct
      type fml = t
      type t = fml
      type var = Var.t
      type value = Bool.t

      let pp = pp (false, false, -1)

      let rec iter f b =
        if is_ite b then (
          f b.guard ;
          iter f b.pos ;
          iter f b.neg )

      exception Partial

      let eval f =
        let rec evl b =
          if is_true b then true
          else if is_false b then false
          else if f b.guard then evl b.pos
          else evl b.neg
        in
        evl
    end

    module Checker = Check.Make (Var) (Bool) (Ite)

    let valid2 msg name = Checker.valid2 (msg ^ ": " ^ name)
    let valid1 msg name = Checker.valid1 (msg ^ ": " ^ name)
    let neg msg = valid1 msg "neg" (fun v1 v -> (not v1) = v)
    let conj msg = valid2 msg "conj" (fun v1 v2 v -> (v1 && v2) = v)
    let disj msg = valid2 msg "disj" (fun v1 v2 v -> (v1 || v2) = v)
    let iff msg = valid2 msg "iff" (fun v1 v2 v -> v1 = v2 = v)

    let imp msg =
      valid2 msg "imp" (fun v1 v2 v -> (if v1 then v2 else true) = v)

    let xor msg = valid2 msg "xor" (fun _ _ _ -> true)
    let[@warning "-32"] equiv msg = valid1 msg "equiv" (fun v1 v -> v1 = v)

    let[@warning "-32"] implies msg =
      valid1 msg "implies" (fun v1 v -> if v1 then v else true)
  end

  let mk_ite b1 b2 b3 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    assert (ordered b3) ;
    let b = Build.make b1 b2 b3 in
    assert (ordered b) ;
    b

  let mk_neg b =
    if is_true b then mk_false
    else if is_false b then mk_true
    else
      let b' = mk_ite b mk_false mk_true in
      assert (Check.neg "mk_neg" b b') ;
      assert (ordered b) ;
      b'

  let mk_conj b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    if b1 == b2 then b1
    else if is_false b1 || is_false b2 then mk_false
    else if is_true b1 then b2
    else if is_true b2 then b1
    else
      let b = mk_ite b1 b2 mk_false in
      assert (ordered b) ;
      assert (Check.conj "mk_conj" b1 b2 b) ;
      b

  let mk_disj b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    if b1 == b2 then b1
    else if is_true b1 || is_true b2 then mk_true
    else if is_false b1 then b2
    else if is_false b2 then b1
    else
      let b = mk_ite b1 mk_true b2 in
      assert (ordered b) ;
      assert (Check.disj "mk_disj" b1 b2 b) ;
      b

  let mk_imp b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    if b1 == b2 then mk_true
    else if is_false b1 then mk_true
    else
      let b = mk_ite b1 b2 mk_true in
      assert (ordered b) ;
      assert (Check.imp "mk_imp" b1 b2 b) ;
      b

  let mk_iff b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    if b1 == b2 then mk_true
    else
      let b = mk_ite b1 b2 (mk_neg b2) in
      assert (ordered b) ;
      assert (Check.iff "mk_iff" b1 b2 b) ;
      b

  let mk_xor b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    let b = mk_ite b1 (mk_neg b2) b2 in
    assert (ordered b) ;
    assert (Check.xor "mk_xor" b1 b2 b) ;
    b

  let union p q b =
    assert (ordered b) ;
    let b' = mk_conj b (mk_iff (mk_posvar p) (mk_posvar q)) in
    assert (ordered b') ;
    b'

  let separate p q b =
    assert (ordered b) ;
    let b' = mk_conj b (mk_xor (mk_posvar p) (mk_posvar q)) in
    assert (ordered b') ;
    b'

  let equal b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    b1 == b2

  let implies b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    mk_imp b1 b2 == mk_true

  let contrad b1 b2 =
    assert (ordered b1) ;
    assert (ordered b2) ;
    mk_conj b1 b2 == mk_false

  exception Unsat

  let implicant b q =
    assert (not (is_unsat b)) ;
    let rec sat n =
      if is_true n then ()
      else if is_false n then raise Unsat
      else
        let p = n.guard in
        try
          Queue.push (true, p) q ;
          sat n.pos
        with Unsat ->
          Queue.push (false, p) q ;
          sat n.neg
    in
    assert (not (is_unsat b)) ;
    Queue.clear q ;
    sat b

  let and_elim b =
    let pl = ref [] and nl = ref [] in
    let rec elim b =
      if is_const b then b
      else if is_false b.neg then (
        pl := b.guard :: !pl ;
        elim b.pos )
      else if is_false b.pos then (
        nl := b.guard :: !nl ;
        elim b.neg )
      else b
    in
    let b' = elim b in
    assert (ordered b') ;
    (!pl, !nl, b')
end

(**/**)

(** For debugging only. *)
module Test = struct
  let numofprobes = ref 10000
  let maxvar = ref 17
  let maxpred = ref 3
  let maxheap = ref 100

  module Var = struct
    type t = int

    let to_string = Format.sprintf "x[%d]"
    let equal = ( = )
    let hash i = i
    let pp fmt i = Format.fprintf fmt "x[%d]" i
    let compare = Stdlib.compare
    let random () = Random.int !maxvar
    let dummy = -1
  end

  module Fml = Make (Var)
  open Fml

  module Heap = struct
    let heap = Array.make !maxheap mk_false
    let max = ref 1

    let[@warning "-32"] reset = function
      | [] ->
          max := 1 ;
          heap.(0) <- mk_false
      | pl ->
          max := List.length pl - 1 ;
          for i = 0 to !max do
            heap.(i) <- List.nth pl i
          done

    let full () = !max >= !maxheap - 1

    let alloc p =
      assert (!max < !maxheap - 1) ;
      incr max ;
      heap.(!max) <- p ;
      !max

    let lookup i = heap.(i)
    let set i p = heap.(i) <- p

    let random () =
      let i = Random.int !max in
      (i, lookup i)
  end

  module Ops = struct
    let[@warning "-32"] mk_true () =
      if not (Heap.full ()) then (
        Format.eprintf "\ntrue <-- ()@?" ;
        let i = Heap.alloc mk_true in
        Format.eprintf "\ntrue[%d] --> %s@?" i (to_string mk_true) )

    let[@warning "-32"] mk_false () =
      if not (Heap.full ()) then (
        Format.eprintf "\nfalse <-- ()@?" ;
        let i = Heap.alloc mk_false in
        Format.eprintf "\nfalse[%d] --> %s@?" i (to_string mk_false) )

    let mk_posvar () =
      if not (Heap.full ()) then (
        let p = Var.random () in
        Format.eprintf "\nposvar <-- %s@?" (Var.to_string p) ;
        let b = mk_posvar p in
        let i = Heap.alloc b in
        Format.eprintf "\nposvar[%d] --> %s@?" i (to_string b) )

    let mk_negvar () =
      if not (Heap.full ()) then (
        let p = Var.random () in
        Format.eprintf "\nnegvar <-- %s@?" (Var.to_string p) ;
        let b = mk_negvar p in
        let i = Heap.alloc b in
        Format.eprintf "\nnegvar[%d] --> %s@?" i (to_string b) )

    let mk_neg () =
      let i1, b1 = Heap.random () in
      Format.eprintf "\nneg <-- %s @?" (to_string b1) ;
      let b = mk_neg b1 in
      if is_ite b then Heap.set i1 b ;
      Format.eprintf "\nconj[%d] --> %s@?" i1 (to_string b)

    let mk_conj () =
      let _i1, b1 = Heap.random () and i2, b2 = Heap.random () in
      Format.eprintf "\nconj <-- %s %s @?" (to_string b1) (to_string b2) ;
      let b = mk_conj b1 b2 in
      if is_ite b then Heap.set i2 b ;
      Format.eprintf "\nconj[%d] --> %s@?" i2 (to_string b)

    let mk_disj () =
      let _i1, b1 = Heap.random () and i2, b2 = Heap.random () in
      Format.eprintf "\ndisj <-- %s %s @?" (to_string b1) (to_string b2) ;
      let b = mk_disj b1 b2 in
      if is_ite b then Heap.set i2 b ;
      Format.eprintf "\ndisj[%d] --> %s@?" i2 (to_string b)
  end

  let init () = Random.self_init ()

  let run () =
    let apply () =
      match Random.int 10 with
      | 2 -> Ops.mk_negvar ()
      | 3 -> Ops.mk_posvar ()
      | 4 | 5 -> Ops.mk_neg ()
      | 6 | 7 -> Ops.mk_conj ()
      | 8 | 9 -> Ops.mk_disj ()
      | _ -> ()
    in
    for _ = 0 to !numofprobes do
      apply ()
    done ;
    Format.eprintf "bdd: self test ok.@?"
end
