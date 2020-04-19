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
  val fresh : unit -> t
end

module type T = sig
  type var
  type t

  val arity : t -> int
  val arg : int -> t -> t
  val hash : t -> int
  val equal : t -> t -> bool
  val diseq : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val is_var : t -> bool
  val of_var : var -> t
  val to_var : t -> var
  val tuple : t array -> t
  val nil : t
  val pair : t -> t -> t
  val triple : t -> t -> t -> t
  val is_pair : t -> bool
  val is_triple : t -> bool
  val is_tuple : int -> t -> bool
  val is_proj : t -> bool
  val proj : int -> int -> t -> t
  val map : (var -> t) -> t -> t
  val exists : (var -> bool) -> t -> bool
  val for_all : (var -> bool) -> t -> bool
  val occurs : var -> t -> bool
  val choose : t -> var
  val replace : var -> t -> t -> t
  val rename : var -> var -> t -> t
  val iter : (var -> unit) -> t -> unit

  module Subst : Subst.S with type var = var and type trm = t

  exception Unsat

  val solve : t -> t -> Subst.t
end

exception Witness

let array_exists p a =
  let test s = if p s then raise Witness in
  try
    Array.iter test a ;
    false
  with Witness -> true

exception Counterexample

let array_for_all p a =
  let test s = if not (p s) then raise Counterexample in
  try
    Array.iter test a ;
    true
  with Counterexample -> false

let array_for_all2 p a b =
  let n = Array.length a and m = Array.length b in
  n = m
  && n > 0
  &&
  try
    for i = 0 to n - 1 do
      if not (p a.(i) b.(i)) then raise Counterexample
    done ;
    true
  with Counterexample -> false

module Tuple (Var : VAR) = struct
  type var = Var.t

  type t = Var of var | Tuple of tuple | Proj of proj

  and tuple = {mutable args: t array; mutable hash: int}

  and proj =
    { mutable index: int
    ; mutable width: int
    ; mutable arg: t
    ; mutable hashp: int }

  type term = t (* nickname *)

  let is_var = function Var _ -> true | _ -> false

  let is_tuple n = function
    | Tuple a -> Array.length a.args = n
    | _ -> false

  let is_pair = is_tuple 2
  let is_triple = is_tuple 3
  let is_proj = function Proj _ -> true | _ -> false

  let rec is_constructor = function
    | Var _ -> true
    | Proj _ -> false
    | Tuple a -> array_for_all is_constructor a.args

  exception Variable

  let arity = function
    | Proj _ -> 1
    | Tuple a -> Array.length a.args
    | Var _ -> raise Variable

  let arg i t =
    assert (0 <= i && i < arity t) ;
    match t with
    | Proj a -> a.arg
    | Tuple a -> a.args.(i)
    | Var _ -> raise Variable

  let rec hash = function
    | Var n -> Var.hash n
    | Tuple a -> hash_tuple a
    | Proj a -> hash_proj a

  and hash_tuple a =
    if a.hash >= 0 then a.hash
    else
      let plus acc t = acc + hash t in
      let hsh = Array.fold_left plus 0 a.args land 0x3FFFFFFF in
      a.hash <- hsh ;
      hsh

  and hash_proj p =
    if p.hashp >= 0 then p.hashp
    else
      let hsh = (hash p.arg + p.index + p.width) land 0x3FFFFFFF in
      p.hashp <- hsh ;
      hsh

  let equal = ( == ) (* terms are hashconsed *)

  let equal_proj p q =
    p.index = q.index && p.width = q.width && p.arg == q.arg

  let equal_tuple a b = array_for_all2 ( == ) a.args b.args

  let rec diseq s t =
    match (s, t) with
    | Tuple a, Tuple b when Array.length a.args <> Array.length b.args ->
        true
    | Tuple a, _ -> diseq_tuple a t
    | _, Tuple b -> diseq_tuple b s
    | Proj a, _ -> diseq_proj a t
    | _, Proj b -> diseq_proj b s
    | _ -> false

  and diseq_tuple a t =
    let equalt s = if equal s t then raise Witness in
    try
      Array.iter equalt a.args ;
      false
    with Witness -> true

  and diseq_proj a t =
    equal a.arg t
    || match a.arg with Proj b -> diseq_proj b t | _ -> false

  let compare s t =
    if s == t then 0
    else
      match (s, t) with
      | Var x, Var y -> Var.compare x y
      | Var _, (Tuple _ | Proj _) -> -1
      | (Tuple _ | Proj _), Var _ -> 1
      | _ ->
          let hs = hash s and ht = hash t in
          if hs < ht then -1 else if hs > ht then 1 else Stdlib.compare s t

  let compare_proj p q =
    let c = Stdlib.compare p.index q.index in
    if c <> 0 then c
    else
      let d = Stdlib.compare p.width q.width in
      if d <> 0 then d else compare p.arg q.arg

  let rec pp fmt t =
    match t with
    | Var x -> Var.pp fmt x
    | Tuple a -> pp_tuple fmt a
    | Proj p -> pp_proj fmt p

  and pp_tuple fmt a =
    let n = Array.length a.args in
    if n = 0 then Format.fprintf fmt "<>"
    else (
      assert (n >= 2) ;
      Format.fprintf fmt "<" ;
      for i = 0 to n - 2 do
        pp fmt a.args.(i) ;
        Format.fprintf fmt ", "
      done ;
      pp fmt a.args.(n - 1) ;
      Format.fprintf fmt ">" )

  and pp_proj fmt p =
    Format.fprintf fmt "proj[%d,%d](" p.index p.width ;
    pp fmt p.arg ;
    Format.fprintf fmt ")"

  let to_var = function
    | Var x -> x
    | _ -> invalid_arg "Tuple.to_var: argument not a variable."

  let of_var =
    let module Cache = Weakhash.Make (Var) in
    let universe = Cache.create 27 in
    fun x ->
      try Cache.find universe x
      with Not_found ->
        let t = Var x in
        assert (not (Cache.mem universe x)) ;
        Cache.add universe x t ;
        t

  let nil = Tuple {args= Array.make 0 (Obj.magic 0); hash= 0}

  let dummy_tuple =
    let arb = Obj.magic 0 in
    let table = Hashtbl.create 4 in
    fun n ->
      try Hashtbl.find table n
      with Not_found ->
        let a = {args= Array.make n arb; hash= -1} in
        Hashtbl.add table n a ;
        a

  module Tuple = struct
    type tuple = t
    type t = tuple

    let equal s t =
      hash s = hash t
      &&
      match (s, t) with
      | Tuple a, Tuple b -> equal_tuple a b
      | _ -> assert false

    let hash = function Tuple a -> hash_tuple a | _ -> assert false
  end

  let pair =
    let module Cache = Weak.Make (struct
      type t = term

      let equal s t =
        assert (is_pair s) ;
        assert (is_pair t) ;
        match (s, t) with
        | Tuple a, Tuple b -> equal_tuple a b
        | _ -> assert false

      let hash t =
        assert (is_pair t) ;
        match t with Tuple a -> hash_tuple a | _ -> assert false
    end) in
    let cache = Cache.create 7 in
    let tuple = dummy_tuple 2 in
    let dummy = Tuple tuple in
    fun s t ->
      match (s, t) with
      | Proj a, Proj b
        when (a.index = 0 && a.width = 2)
             && (b.index = 1 && a.width = 2)
             && equal a.arg b.arg ->
          a.arg
      | _ -> (
          tuple.args.(0) <- s ;
          tuple.args.(1) <- t ;
          tuple.hash <- -1 ;
          try Cache.find cache dummy
          with Not_found ->
            let p = Tuple {args= Array.copy tuple.args; hash= tuple.hash} in
            Cache.add cache p ;
            p )

  let triple =
    let module Cache = Weak.Make (Tuple) in
    let cache = Cache.create 7 in
    let tuple = dummy_tuple 3 in
    let dummy = Tuple tuple in
    fun s t r ->
      match (s, t, r) with
      | Proj a, Proj b, Proj c
        when (a.index = 0 && a.width = 3)
             && (b.index = 1 && a.width = 3)
             && (c.index = 2 && a.width = 3)
             && equal a.arg b.arg
             && equal b.arg c.arg ->
          a.arg
      | _ -> (
          tuple.args.(0) <- s ;
          tuple.args.(1) <- t ;
          tuple.args.(2) <- r ;
          tuple.hash <- -1 ;
          try Cache.find cache dummy
          with Not_found ->
            let t3 =
              Tuple {args= Array.copy tuple.args; hash= tuple.hash}
            in
            Cache.add cache t3 ;
            t3 )

  let tuple =
    let module Cache = Weak.Make (Tuple) in
    let cache = Cache.create 7 in
    let extensional n a =
      assert (n > 0 && n = Array.length a) ;
      let rec test_rest i x =
        if i >= n then raise Not_found
        else
          match a.(i) with
          | Proj a when a.index = i && a.width = n && x == a.arg ->
              test_rest (i + 1) x
          | _ -> raise Not_found
      in
      match a.(0) with
      | Proj a when a.index = 0 && a.width = n -> test_rest 1 a.arg
      | _ -> raise Not_found
    in
    fun a ->
      match Array.length a with
      | 0 -> nil
      | 1 -> a.(0)
      | 2 -> pair a.(0) a.(1)
      | 3 -> triple a.(0) a.(1) a.(2)
      | n -> (
        try extensional n a
        with Not_found -> (
          let tuple = dummy_tuple n in
          let dummy = Tuple tuple in
          for i = 0 to n - 1 do
            tuple.args.(i) <- a.(i)
          done ;
          tuple.hash <- -1 ;
          try Cache.find cache dummy
          with Not_found ->
            let tt =
              Tuple {args= Array.copy tuple.args; hash= tuple.hash}
            in
            Cache.add cache tt ;
            tt ) )

  let proj =
    let module Cache = Weak.Make (struct
      type t = term

      let equal s t =
        assert (is_proj s) ;
        assert (is_proj t) ;
        match (s, t) with
        | Proj p, Proj q -> equal_proj p q
        | _ -> assert false

      let hash t =
        assert (is_proj t) ;
        match t with Proj p -> hash_proj p | _ -> assert false
    end) in
    let cache = Cache.create 7 in
    let proj = {index= 0; width= 0; arg= Obj.magic 0; hashp= -1} in
    let dummy = Proj proj in
    fun i n t ->
      assert (0 <= i && i < n && n < max_int) ;
      match t with
      | Tuple a when Array.length a.args = n -> a.args.(i)
      | _ -> (
          proj.index <- i ;
          proj.width <- n ;
          proj.arg <- t ;
          proj.hashp <- -1 ;
          try Cache.find cache dummy
          with Not_found ->
            (* reuse hash. *)
            let p = Proj {index= i; width= n; arg= t; hashp= proj.hashp} in
            Cache.add cache p ;
            p )

  let iter f =
    let rec iterf = function
      | Var x -> f x
      | Proj a -> iterf a.arg
      | Tuple a ->
          let n = Array.length a.args in
          if n > 0 then
            for i = 0 to n - 1 do
              let s = a.args.(i) in
              iterf s
            done
    in
    iterf

  let exists p t =
    let test x = if p x then raise Witness in
    try
      iter test t ;
      false
    with Witness -> true

  let occurs (x : Var.t) =
    let eqx y = Var.equal x y in
    exists eqx

  exception Violation

  let for_all p t =
    let test x = if not (p x) then raise Violation in
    try
      iter test t ;
      true
    with Violation -> false

  let dummy_array n = Array.make n (Obj.magic 0)

  let map f =
    let rec mapf t =
      match t with
      | Var x -> ( try f x with _ -> t )
      | Proj a ->
          let i = a.index and n = a.width and s = a.arg in
          let s' = mapf s in
          if s == s' then t else proj i n s'
      | Tuple a -> (
        match Array.length a.args with
        | 0 -> t
        | 2 ->
            let s1 = a.args.(0) and s2 = a.args.(1) in
            let s1' = mapf s1 and s2' = mapf s2 in
            if s1 == s1' && s2 == s2' then t else pair s1' s2'
        | 3 ->
            let s1 = a.args.(0) and s2 = a.args.(1) and s3 = a.args.(2) in
            let s1' = mapf s1 and s2' = mapf s2 and s3' = mapf s3 in
            if s1 == s1' && s2 == s2' && s3 == s3' then t
            else triple s1' s2' s3'
        | n ->
            assert (n >= 2) ;
            let dummy = dummy_array n in
            let changed = ref false in
            for i = 0 to n - 1 do
              let s = a.args.(i) in
              let s' = mapf s in
              dummy.(i) <- s' ;
              if not (s == s') then changed := true
            done ;
            if !changed then tuple (Array.copy dummy) else t )
    in
    mapf

  exception Found of var

  let choose t =
    let install x = raise (Found x) in
    try
      iter install t ;
      raise Not_found
    with Found x -> x

  let replace x s t =
    if equal (of_var x) s then t
    else
      let repl z = if Var.equal x z then s else of_var z in
      map repl t

  let rename x y t =
    if Var.equal x y then t
    else
      let repl z = if Var.equal x z then of_var y else of_var z in
      map repl t

  module Subst =
    Subst.Make
      (Var)
      (struct
        type term = t
        type var = Var.t
        type t = term

        let equal = equal
        let hash = hash
        let pp = pp
        let map = map
        let iter = iter
        let of_var = of_var
        let compare = compare
      end)

  exception Unsat

  let solve =
    let module Fresh = struct
      let lhs = ref (Obj.magic 0)
      let rhs = ref (Obj.magic 1)

      let init s t =
        lhs := s ;
        rhs := t

      let is x = (not (occurs x !lhs)) && not (occurs x !rhs)
    end in
    let module Todo = struct
      let todo = Stacks.create ()
      let clear () = Stacks.clear todo
      let is_empty () = Stacks.is_empty todo

      let push s t =
        let cmp = compare s t in
        if cmp = 0 then ()
        else if cmp > 0 then Stacks.push (s, t) todo
        else Stacks.push (t, s) todo

      let pop () = Stacks.pop todo
    end in
    let module Subst = struct
      let current = ref (Subst.empty ())
      let clear () = current := Subst.empty ()
      let apply t = Subst.apply !current t
      let add x t = Subst.add !current x t
      let fuse x t = Subst.fuse !current x t
      let dom x = Subst.dom x !current
      let get () = !current
    end in
    let module Rename = struct
      module Proj = struct
        type t = proj

        let equal = equal_proj
        let hash = hash_proj
        let compare = compare_proj
        let pp = pp_proj
      end

      module Table = Maps.Make (Proj) (Var)

      let current = ref (Table.empty ())
      let clear () = current := Table.empty ()

      let find p =
        assert (is_var p.arg) ;
        let x = to_var p.arg in
        try Table.find p !current
        with Not_found ->
          let i = p.index and n = p.width in
          let fresh_args = Array.make n (Obj.magic 0) in
          for j = 0 to n - 1 do
            let k = Var.fresh () in
            let q =
              if i = j then p
              else {index= j; width= n; arg= p.arg; hashp= -1}
            in
            Table.set q k !current ;
            fresh_args.(j) <- of_var k
          done ;
          assert (not (Subst.dom x)) ;
          Subst.add x (tuple fresh_args) ;
          Table.find p !current

      let get () = !current
    end in
    let rec preprocess t =
      match t with
      | Var _ -> t
      | Proj p ->
          let i = p.index and n = p.width and s = p.arg in
          if is_var s then of_var (Rename.find p)
          else
            let s' = preprocess s in
            if s == s' then t else proj i n s'
      | Tuple a ->
          let n = Array.length a.args in
          let changed = ref false in
          let dummy = dummy_array n in
          for j = 0 to n - 1 do
            let s = a.args.(j) in
            let s' = preprocess s in
            dummy.(j) <- s' ;
            if not (s == s') then changed := true
          done ;
          if !changed then tuple (Array.copy dummy) else t
    in
    let solve1 s t =
      match (s, t) with
      | Tuple a, Tuple b ->
          let n = Array.length a.args and m = Array.length b.args in
          if n <> m then raise Unsat
          else
            for i = 0 to n - 1 do
              Todo.push a.args.(i) b.args.(i)
            done
      | Var x, t ->
          if occurs x t then raise Unsat
          else if Fresh.is x then Subst.fuse x t
          else Subst.add x t
      | s, Var y ->
          if occurs y s then raise Unsat
          else if Fresh.is y then Subst.fuse y s
          else Subst.add y s
      | _ -> invalid_arg "Tuple.solve: unreachable"
    in
    fun s t ->
      Todo.clear () ;
      Subst.clear () ;
      Rename.clear () ;
      Fresh.init s t ;
      let s = preprocess s in
      let t = preprocess t in
      assert (is_constructor s) ;
      assert (is_constructor t) ;
      Todo.push s t ;
      while not (Todo.is_empty ()) do
        let s, t = Todo.pop () in
        let s = Subst.apply s and t = Subst.apply t in
        if equal s t then ()
        else if diseq s t then raise Unsat
        else solve1 s t
      done ;
      Subst.get ()
end

module Infsys
    (Var : VAR)
    (Tuple : T with type var = Var.t)
    (IF : Shostak.V with type var = Var.t) =
  Shostak.Make (Var) (Tuple) (IF)
