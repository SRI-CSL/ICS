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

exception Unsatisfiable

let stdout = Format.std_formatter
let stderr = Format.err_formatter

type theory = U | A | T | F

(** {i Rationals} *)
module Q = struct
  open Num

  type t = num

  let of_int = num_of_int
  let make x y = div_num (num_of_int x) (num_of_int y)
  let zero = of_int 0
  let one = of_int 1
  let two = of_int 2
  let add = add_num
  let sub = sub_num
  let minus = minus_num
  let mult = mult_num
  let div = div_num
  let inv = div one
  let compare = compare_num
  let equal = eq_num
  let is_zero = equal zero
  let is_one = equal one
  let lt = lt_num
  let le = le_num
  let gt = gt_num
  let ge = ge_num
  let is_pos x = gt x zero
  let is_neg x = lt x zero
  let is_nonneg x = ge x zero
  let is_nonpos x = le x zero
  let of_z = num_of_big_int
  let to_z = big_int_of_num
  let ratio x = Ratio.cautious_normalize_ratio (ratio_of_num x)
  let denominator x = Ratio.denominator_ratio (ratio x)
  let numerator x = Ratio.numerator_ratio (ratio x)
  let is_integer = is_integer_num
  let hash = Hashtbl.hash
  let to_string = string_of_num
  let of_string = num_of_string
  let pp fmt x = Format.fprintf fmt "%s" (to_string x)
  let max_random = ref 20

  let random =
    let initialized = ref false in
    fun () ->
      if not !initialized then (
        Random.self_init () ;
        initialized := true ) ;
      let d = Random.int !max_random and n = Random.int !max_random in
      if n = 0 then of_int d else make d n
end

module Name = Name

let k = ref 0

(** {i Empty structure} *)
module type UNIT = sig end

module Intern (Fresh : sig
  val name : string
end) =
struct
  (** {i External names} *)
  module Extern = struct
    type t = {mutable name: string; mutable hash: int}

    let hash x =
      if x.hash >= 0 then x.hash
      else
        let h = Hashtbl.hash_param 4 4 x.name in
        x.hash <- h ;
        h

    let equal x y = hash x == hash y && x.name = y.name

    let compare x y =
      if equal x y then 0 else if hash x > hash y then 1 else -1

    let to_string x = x.name
    let pp fmt x = Format.fprintf fmt "%s@?" x.name
  end

  (** {i Internal names} *)
  module Intern = struct
    type t = int

    let hash x =
      assert (x >= 0) ;
      x

    let compare x y = if x == y then 0 else if x > y then 1 else -1
    let to_string x = Format.sprintf "%s!%d" Fresh.name x
    let pp fmt x = Format.fprintf fmt "%s@?" (to_string x)
  end

  type t = Extern of Extern.t | Intern of Intern.t
  type var = t (* nickname *)

  let is_internal = function Extern _ -> false | Intern _ -> true
  let is_external = function Extern _ -> true | Intern _ -> false
  let equal = ( == ) (* hashconsed. *)

  let compare x y =
    match (x, y) with
    | Extern e1, Extern e2 -> Extern.compare e1 e2
    | Intern i1, Intern i2 -> Intern.compare i1 i2
    | Intern _, Extern _ -> 1
    | Extern _, Intern _ -> -1

  let preference x y =
    match (x, y) with
    | Extern _, Intern _ -> 1
    | Intern _, Extern _ -> -1
    | _ -> raise Not_found

  let of_string =
    let module Cache = Weak.Make (struct
      type t = var

      let equal x y =
        assert (is_external x) ;
        assert (is_external y) ;
        match (x, y) with
        | Extern e1, Extern e2 -> Extern.equal e1 e2
        | _ -> assert false

      let hash x =
        assert (is_external x) ;
        match x with Extern e -> Extern.hash e | _ -> assert false
    end) in
    let cache = Cache.create 17 in
    let extern = {Extern.name= ""; Extern.hash= -1} in
    let dummy = Extern extern in
    fun s ->
      extern.Extern.name <- s ;
      extern.Extern.hash <- -1 ;
      try Cache.find cache dummy
      with Not_found ->
        let x = Extern {Extern.name= s; hash= extern.Extern.hash} in
        Cache.add cache x ;
        x

  let of_name n = of_string (Name.to_string n)

  let to_string = function
    | Extern e -> Extern.to_string e
    | Intern i -> Intern.to_string i

  let internal =
    let cache = Hashtbl.create 17 in
    fun k ->
      try Hashtbl.find cache k
      with Not_found ->
        let x = Intern k in
        Hashtbl.add cache k x ;
        x

  let fresh () =
    incr k ;
    internal !k

  let hash = function
    | Extern e -> Extern.hash e
    | Intern e -> Intern.hash e

  let pp fmt = function
    | Extern e -> Extern.pp fmt e
    | Intern i -> Intern.pp fmt i

  let is_fresh = function Intern i -> i > !k | Extern _ -> false
end

(** {i Term variables} *)
module Var = Intern (struct
  let name = "v"
end)

(** {i Set of term variables} *)
module Vars = Sets.Make (Var)

(** {i Unintepreted function symbols.} *)
module Funsym = Name

(** {i Terms} *)
module Term = struct
  (** {i Polynomials with rational coefficients.} *)
  module Polynomial = Polynomial.Make (Q) (Var)

  (** {i Uninterpreted term application} *)
  module Uninterp = Cc.Apply (Var) (Funsym)

  (** {i Tuple terms} *)
  module Tuple = Tuple.Tuple (Var)

  (** {i Functional Array Terms} *)
  module Array = Funarr.Flat (Var)

  type t =
    | Var of Var.t
    | Uninterp of Uninterp.t
    | Arith of Polynomial.t
    | Tuple of Tuple.t
    | Array of Array.t

  let hash = function
    | Var x -> Var.hash x
    | Uninterp a -> Uninterp.hash a
    | Arith p -> Polynomial.hash p
    | Tuple t -> Tuple.hash t
    | Array a -> Array.hash a

  let pp fmt = function
    | Var x -> Var.pp fmt x
    | Uninterp a -> Uninterp.pp fmt a
    | Arith p -> Polynomial.pp fmt p
    | Tuple t -> Tuple.pp fmt t
    | Array a -> Array.pp fmt a

  let to_string t =
    pp Format.str_formatter t ;
    Format.flush_str_formatter ()

  let well_formed = function
    | Var _ | Uninterp _ -> true
    | Arith p -> not (Polynomial.is_indet p)
    | Tuple t -> not (Tuple.is_var t)
    | Array _ -> true

  let equal = ( == ) (* hashconsing constructors *)

  let diseq s t =
    match (s, t) with
    | Arith p, Arith q -> Polynomial.diseq p q
    | Tuple ss, Tuple tt -> Tuple.diseq ss tt
    | (Arith _ | Array _), Tuple tt -> Tuple.is_tuple 0 tt
    | Tuple tt, (Arith _ | Array _) -> Tuple.is_tuple 0 tt
    | _ -> false

  exception Variable

  let theory = function
    | Uninterp _ -> U
    | Arith _ -> A
    | Tuple _ -> T
    | Array _ -> F
    | Var _ -> raise Variable

  let compare s t =
    match (s, t) with
    | Var x, Var y -> Var.compare x y
    | Var _, _ -> -1
    | _, Var _ -> 1
    | Uninterp a, Uninterp b -> Uninterp.compare a b
    | Uninterp _, _ -> -1
    | _, Uninterp _ -> 1
    | Arith p, Arith q -> Polynomial.compare p q
    | Arith _, _ -> -1
    | _, Arith _ -> 1
    | Tuple ss, Tuple tt -> Tuple.compare ss tt
    | Tuple _, _ -> -1
    | _, Tuple _ -> 1
    | Array a, Array b -> Array.compare a b

  let to_var = function
    | Var x -> x
    | _ -> raise (Invalid_argument "not a variable")

  let of_var =
    let module Cache = Weakhash.Make (Var) in
    let universe = Cache.create 153 in
    fun x ->
      try Cache.find universe x
      with Not_found ->
        let t = Var x in
        assert (not (Cache.mem universe x)) ;
        Cache.add universe x t ;
        t

  let of_apply =
    let module Cache = Weakhash.Make (Uninterp) in
    let table = Cache.create 107 in
    fun a ->
      try Cache.find table a
      with Not_found ->
        let t = Uninterp a in
        assert (not (Cache.mem table a)) ;
        Cache.add table a t ;
        t

  let of_poly =
    let module Cache = Weakhash.Make (Polynomial) in
    let table = Cache.create 107 in
    fun p ->
      try of_var (Polynomial.d_indet p)
      with Polynomial.Nonindet -> (
        try Cache.find table p
        with Not_found ->
          let t = Arith p in
          assert (not (Cache.mem table p)) ;
          Cache.add table p t ;
          t )

  let of_num c = of_poly (Polynomial.constant c)

  let of_tuple =
    let module Cache = Weakhash.Make (Tuple) in
    let table = Cache.create 107 in
    fun tt ->
      if Tuple.is_var tt then of_var (Tuple.to_var tt)
      else
        try Cache.find table tt
        with Not_found ->
          let t = Tuple tt in
          assert (not (Cache.mem table tt)) ;
          Cache.add table tt t ;
          t

  let of_array =
    let module Cache = Weakhash.Make (Array) in
    let table = Cache.create 107 in
    fun tt ->
      try Cache.find table tt
      with Not_found ->
        let t = Array tt in
        assert (not (Cache.mem table tt)) ;
        Cache.add table tt t ;
        t

  let iter f =
    let iterf = function
      | Var x -> f x
      | Uninterp {Uninterp.funsym= _; Uninterp.arg= x} -> f x
      | Arith p ->
          let fc _ = () and fm x _ = f x in
          Polynomial.iter fc fm p
      | Tuple t -> Tuple.iter f t
      | Array (Array.Update (x, y, z)) ->
          f x ;
          f y ;
          f z
      | Array (Array.Lookup (x, y)) ->
          f x ;
          f y
    in
    iterf

  exception Witness

  let exists p t =
    let test x = if p x then raise Witness in
    try
      iter test t ;
      false
    with Witness -> true

  let contains_fresh = exists Var.is_fresh
end

module Term2 = Type.Product (Term) (Term)

module Propvar = Intern (struct
  let name = "p"
end)

(** {i Predicate symbols.} *)
module Predsym = struct
  (** {i Arithmetic predicate symbols} *)
  module Arith = struct
    type t = Nonneg | Pos | Real | Equal0 | Diseq0 | Int

    let equal = ( == )

    let to_string = function
      | Nonneg -> "nonneg"
      | Pos -> "pos"
      | Real -> "real"
      | Equal0 -> "equal0"
      | Diseq0 -> "diseq0"
      | Int -> "int"

    let pp fmt p = Format.fprintf fmt "%s" (to_string p)

    let hash = function
      | Nonneg -> 127
      | Pos -> 255
      | Real -> 511
      | Equal0 -> 753
      | Diseq0 -> 911
      | Int -> 1357

    let compare = Stdlib.compare

    let sub p q =
      match (p, q) with
      | Equal0, Nonneg -> true
      | Int, Real -> true
      | _ -> false

    let disjoint p q =
      match (p, q) with
      | Equal0, Diseq0 -> true
      | Diseq0, Equal0 -> true
      | _ -> false
  end

  type t = Uninterp of Name.t | Arith of Arith.t | Tuple of int | Array

  let uninterp =
    let module Cache = Weakhash.Make (Name) in
    let table = Cache.create 7 in
    fun str ->
      let n = Name.of_string str in
      try Cache.find table n
      with Not_found ->
        let p = Uninterp n in
        assert (not (Cache.mem table n)) ;
        Cache.add table n p ;
        p

  let nonneg = Arith Arith.Nonneg
  let pos = Arith Arith.Pos
  let real = Arith Arith.Real
  let equal0 = Arith Arith.Equal0
  let diseq0 = Arith Arith.Diseq0
  let integer = Arith Arith.Int

  let of_arith = function
    | Arith.Nonneg -> nonneg
    | Arith.Pos -> pos
    | Arith.Real -> real
    | Arith.Equal0 -> equal0
    | Arith.Diseq0 -> diseq0
    | Arith.Int -> integer

  let tuple =
    let module Cache = Hashtbl.Make (Int) in
    let table = Cache.create 7 in
    fun i ->
      assert (i >= 0) ;
      try Cache.find table i
      with Not_found ->
        let p = Tuple i in
        Cache.add table i p ;
        p

  let array = Array

  let theory = function
    | Uninterp _ -> U
    | Arith _ -> A
    | Tuple _ -> T
    | Array -> F

  let to_string = function
    | Uninterp p -> Name.to_string p
    | Arith p -> Arith.to_string p
    | Tuple n -> Format.sprintf "tuple[%d]" n
    | Array -> "array"

  let pp fmt p = Format.fprintf fmt "%s" (to_string p)

  let hash = function
    | Uninterp n -> Name.hash n
    | Arith p -> Arith.hash p
    | Tuple n -> n
    | Array -> 53

  let equal = ( == )

  let sub p q =
    equal p q
    || match (p, q) with Arith p, Arith q -> Arith.sub p q | _ -> false

  let compare = Stdlib.compare

  let disjoint p q =
    match (p, q) with
    | Arith _, (Tuple _ | Array) -> true
    | (Tuple _ | Array), Arith _ -> true
    | Tuple n, Tuple m -> n <> m
    | Tuple _, Array -> true
    | Array, Tuple _ -> true
    | Arith p, Arith q -> Arith.disjoint p q
    | _ -> false
end

(** {i Datatype of formulas} *)
module Formula = struct
  (** {i Binary decision diagrams with propositional variables as
         conditions}. *)
  module Bdd = Bdd.Make (Propvar)

  type t =
    | Equal of Term.t * Term.t
    | Diseq of Term.t * Term.t
    | Poslit of Predsym.t * Term.t
    | Neglit of Predsym.t * Term.t
    | Arith of Predsym.Arith.t * Term.Polynomial.t
    | Prop of Bdd.t

  let pretty = ref true
  let maxdepth = ref (-1)

  let pp fmt = function
    | Equal (s, t) ->
        Term.pp fmt s ;
        Format.fprintf fmt " = " ;
        Term.pp fmt t
    | Diseq (s, t) ->
        Term.pp fmt s ;
        Format.fprintf fmt " <> " ;
        Term.pp fmt t
    | Poslit (p, t) ->
        Predsym.pp fmt p ;
        Format.fprintf fmt "(" ;
        Term.pp fmt t ;
        Format.fprintf fmt ")"
    | Neglit (p, t) ->
        Format.fprintf fmt "~" ;
        Predsym.pp fmt p ;
        Format.fprintf fmt "(" ;
        Term.pp fmt t ;
        Format.fprintf fmt ")"
    | Arith (p, t) ->
        Predsym.Arith.pp fmt p ;
        Format.fprintf fmt "(" ;
        Term.Polynomial.pp fmt t ;
        Format.fprintf fmt ")"
    | Prop p -> Bdd.pp (!pretty, !pretty, !maxdepth) fmt p

  let to_string p =
    pp Format.str_formatter p ;
    Format.flush_str_formatter ()

  let hash = function
    | Equal (s, _) -> Term.hash s
    | Diseq (s, _) -> Term.hash s
    | Poslit (_, s) -> Term.hash s
    | Neglit (_, s) -> Term.hash s
    | Arith (_, p) -> Term.Polynomial.hash p
    | Prop b -> Bdd.hash b

  let equal = ( == )
  let is_true = function Prop b -> Bdd.is_valid b | _ -> false
  let is_false = function Prop b -> Bdd.is_unsat b | _ -> false

  let compare p q =
    let hp = hash p and hq = hash q in
    if hp < hq then -1
    else if hp > hq then 1
    else if equal p q then 0
    else Stdlib.compare p q

  let mk_equal =
    let module Cache = Weakhash.Make (Term2) in
    let table = Cache.create 107 in
    let dummy = Term2.make (Obj.magic 0) (Obj.magic 0) in
    fun s t ->
      Term2.fill dummy s t ;
      try Cache.find table dummy
      with Not_found ->
        let e = Equal (s, t) in
        assert (not (Cache.mem table (Term2.make s t))) ;
        Cache.add table (Term2.make s t) e ;
        e

  let mk_diseq =
    let module Cache = Weakhash.Make (Term2) in
    let table = Cache.create 107 in
    let dummy = Term2.make (Obj.magic 0) (Obj.magic 0) in
    fun s t ->
      Term2.fill dummy s t ;
      try Cache.find table dummy
      with Not_found ->
        let e = Diseq (s, t) in
        assert (not (Cache.mem table (Term2.make s t))) ;
        Cache.add table (Term2.make s t) e ;
        e

  let mk_apply =
    let module Apply = Type.Product (Predsym) (Term) in
    let module Cache = Weakhash.Make (Apply) in
    let table = Cache.create 7 in
    let dummy = Apply.make (Obj.magic 0) (Obj.magic 0) in
    fun p t ->
      Apply.fill dummy p t ;
      try Cache.find table dummy
      with Not_found ->
        let a = Poslit (p, t) in
        assert (not (Cache.mem table (Apply.make p t))) ;
        Cache.add table (Apply.make p t) a ;
        a

  let mk_negapply =
    let module Apply = Type.Product (Predsym) (Term) in
    let module Cache = Weakhash.Make (Apply) in
    let table = Cache.create 7 in
    let dummy = Apply.make (Obj.magic 0) (Obj.magic 0) in
    fun p t ->
      Apply.fill dummy p t ;
      try Cache.find table dummy
      with Not_found ->
        let a = Neglit (p, t) in
        assert (not (Cache.mem table (Apply.make p t))) ;
        Cache.add table (Apply.make p t) a ;
        a

  let mk_arith =
    let module Apply = Type.Product (Predsym.Arith) (Term.Polynomial) in
    let module Cache = Weakhash.Make (Apply) in
    let table = Cache.create 7 in
    let dummy = Apply.make (Obj.magic 0) (Obj.magic 0) in
    fun p t ->
      Apply.fill dummy p t ;
      try Cache.find table dummy
      with Not_found ->
        let a = Arith (p, t) in
        assert (not (Cache.mem table (Apply.make p t))) ;
        Cache.add table (Apply.make p t) a ;
        a

  let mk_prop =
    let module Prop = struct
      type t = Bdd.t

      let pp = Bdd.pp (!pretty, !pretty, !maxdepth)
      let compare = Bdd.compare
      let equal = Bdd.equal
      let hash = Bdd.hash
    end in
    let module Cache = Weakhash.Make (Prop) in
    let cache = Cache.create 7 in
    fun b ->
      try Cache.find cache b
      with Not_found ->
        let p = Prop b in
        assert (not (Cache.mem cache b)) ;
        Cache.add cache b p ;
        p

  let mk_true = mk_prop Bdd.mk_true
  let mk_false = mk_prop Bdd.mk_false
end

module Formulas = Sets.Make (Formula)

let footprint = ref false

module Footprint = struct
  open Formula

  let out name = Format.eprintf "\n%s: " name
  let flush () = Format.eprintf "@."

  let pp p =
    Formula.pp stderr p ;
    Format.eprintf "@?"

  let v = Term.of_var

  let equal x y =
    if !footprint then (
      out "prop" ;
      pp (mk_equal (v x) (v y)) ;
      flush () )

  let diseq x y =
    if !footprint then (
      out "prop" ;
      pp (mk_diseq (v x) (v y)) ;
      flush () )

  let valid1 p x =
    if !footprint then (
      out "prop" ;
      pp (mk_apply p (v x)) ;
      flush () )

  let unsat1 p x =
    if !footprint then (
      out "prop" ;
      pp (mk_negapply p (v x)) ;
      flush () )

  let valid0 p =
    if !footprint then (
      out "prop" ;
      Propvar.pp stderr p ;
      flush () )

  let unsat0 p =
    if !footprint then (
      out "prop" ;
      Propvar.pp stderr p ;
      flush () )

  let process p =
    if !footprint then (
      out "process" ;
      pp p ;
      flush () )

  let close () =
    if !footprint then (
      out "close" ;
      flush () )

  let resolve () =
    if !footprint then (
      out "resolve" ;
      flush () )

  let learned p =
    if !footprint then (
      out "learned" ;
      pp (Formula.mk_prop p) ;
      flush () )

  let implicant p =
    if !footprint then (
      out "implicant" ;
      pp (Formula.mk_prop p) ;
      flush () )

  let implicant_candidate n =
    if !footprint then Format.eprintf "implicant(%d)@?" n
end

module type INFSYS = sig
  type t

  val empty : t
  val initialize : t -> unit
  val current : unit -> t
  val unchanged : unit -> bool
  val find : theory -> Var.t -> Term.t
  val inv : Term.t -> Var.t
  val can : Term.t -> Var.t
  val equal : Term.t -> Term.t -> bool
  val diseq : Term.t -> Term.t -> bool
  val alias : Term.t -> Var.t
  val process : Formula.t -> unit
  val prop_eq : Var.t -> unit
  val prop_deq : Var.t -> Var.t -> unit
  val normalize : unit -> unit
end

(** Names for inference system signatures. *)
module type PARTITION = Partition.INFSYS with type var = Var.t

module type SIMPLEX =
  Simplex.INFSYS
    with type var = Var.t
     and type coeff = Q.t
     and type poly = Term.Polynomial.t

module type PROP =
  Prop.INFSYS with type var = Propvar.t and type t = Formula.Bdd.t

module type CC =
  Cc.INFSYS
    with type var = Var.t
     and type funsym = Name.t
     and type apply = Term.Uninterp.t

module type TUPLE =
  Shostak.INFSYS with type var = Var.t and type trm = Term.Tuple.t

module type FUNARR =
  Funarr.INFSYS with type var = Var.t and type flat = Term.Array.t

module type LITERAL =
  Literal.INFSYS with type predsym = Predsym.t and type var = Var.t

module type PROPVAR = Nullary.INFSYS with type var = Propvar.t

module type RENAME =
  Rename.INFSYS
    with type propvar = Propvar.t
     and type predsym = Predsym.t
     and type var = Var.t

module V : PARTITION = Partition.Make (Var)
module L0 : PROPVAR = Nullary.Make (Propvar)
module L1open = Literal.Make (Var) (Predsym)
module Aopen = Simplex.Make (Term.Polynomial)
module Uopen = Cc.Make (Var) (Funsym) (Term.Uninterp)
module Topen = Tuple.Infsys (Var) (Term.Tuple)
module Fopen = Funarr.Make (Var) (Term.Array)
module Ropen = Rename.Make (Propvar) (Predsym) (Var)
module Popen = Prop.Make (Propvar) (Formula.Bdd)

(** Flag for indicating that a propagation rule might not be called
    immediately. In this case, application of this rule is delayed until it
    is safe to apply it. *)
let critical = ref false

(** Various propagators. They will be initialized after all the inference
    systems have been closed. This should instead be done using recursive
    modules, but I was not able to make this work in ocaml 3.08. *)
let propagate_eq : (Var.t -> Var.t -> unit) ref = ref (fun _ _ -> ())

let propagate_deq : (Var.t -> Var.t -> unit) ref = ref (fun _ _ -> ())
let propagate_valid0 : (Propvar.t -> unit) ref = ref (fun _ -> ())
let propagate_unsat0 : (Propvar.t -> unit) ref = ref (fun _ -> ())

let propagate_valid1 : (Predsym.t -> Var.t -> unit) ref =
  ref (fun _ _ -> ())

let propagate_unsat1 : (Predsym.t -> Var.t -> unit) ref =
  ref (fun _ _ -> ())

module Union = struct
  let lhs = Stacks.create ()
  let rhs = Stacks.create ()

  let reset () =
    Stacks.clear lhs ;
    Stacks.clear rhs

  let rec process x y =
    if V.equal x y then ()
    else if V.diseq x y then raise Unsatisfiable
    else if !critical then delay x y
    else process_critical x y

  and delay x y =
    Stacks.push x lhs ;
    Stacks.push y rhs

  and process_critical x y =
    let x = V.find x and y = V.find y in
    if V.equal x y then ()
    else
      try
        if !footprint then Footprint.equal x y ;
        critical := true ;
        (* let dx = V.deqs x and dy = V.deqs y in *)
        V.union !propagate_deq x y ;
        assert (V.canonical x || V.canonical y) ;
        if V.canonical y then !propagate_eq x y else !propagate_eq y x ;
        critical := false
      with exc ->
        critical := false ;
        raise exc

  let closed () = Stacks.is_empty lhs

  let close () =
    while not (Stacks.is_empty lhs) do
      let x = Stacks.pop lhs and y = Stacks.pop rhs in
      process_critical x y
    done
end

module Separate = struct
  let lhs = Stacks.create ()
  let rhs = Stacks.create ()

  let reset () =
    Stacks.clear lhs ;
    Stacks.clear rhs

  let propagate : (Var.t -> Var.t -> unit) ref = ref (fun _ _ -> ())

  let rec process x y =
    if V.diseq x y then ()
    else if V.equal x y then raise Unsatisfiable
    else if !critical then delay x y
    else process_critical x y

  and delay x y =
    Stacks.push x lhs ;
    Stacks.push y rhs

  and process_critical x y =
    if !footprint then Footprint.diseq x y ;
    critical := true ;
    V.separate x y ;
    !propagate x y ;
    critical := false

  let closed () = Stacks.is_empty lhs

  let close () =
    while not (Stacks.is_empty lhs) do
      let x = Stacks.pop lhs and y = Stacks.pop rhs in
      process_critical x y
    done
end

module Valid0 = struct
  let vars = Stacks.create ()
  let reset () = Stacks.clear vars

  let rec process x =
    if L0.is_valid x then ()
    else if L0.is_unsat x then raise Unsatisfiable
    else if !critical then delay x
    else process_critical x

  and delay x = Stacks.push x vars

  and process_critical x =
    if L0.is_valid x then ()
    else if L0.is_unsat x then raise Unsatisfiable
    else (
      if !footprint then Footprint.valid0 x ;
      critical := true ;
      L0.process_valid x ;
      !propagate_valid0 x ;
      critical := false )

  let closed () = Stacks.is_empty vars

  let close () =
    while not (closed ()) do
      process_critical (Stacks.pop vars)
    done
end

module Unsat0 = struct
  let vars = Stacks.create ()
  let reset () = Stacks.clear vars

  let rec process x =
    if L0.is_unsat x then ()
    else if L0.is_valid x then raise Unsatisfiable
    else if !critical then delay x
    else process_critical x

  and delay x = Stacks.push x vars

  and process_critical x =
    if L0.is_unsat x then ()
    else if L0.is_valid x then raise Unsatisfiable
    else (
      if !footprint then Footprint.unsat0 x ;
      critical := true ;
      L0.process_unsat x ;
      !propagate_unsat0 x ;
      critical := false )

  let closed () = Stacks.is_empty vars

  let close () =
    while not (closed ()) do
      process_critical (Stacks.pop vars)
    done
end

module L1 : LITERAL = L1open (struct
  type var = Var.t

  let find = V.find
  let canonical = V.canonical
  let equal = V.equal
  let diseq = V.diseq
  let union = Union.process
  let separate = Separate.process
end)

module Valid1 = struct
  let preds = Stacks.create ()
  let args = Stacks.create ()

  let reset () =
    Stacks.clear preds ;
    Stacks.clear args

  let rec process p x =
    if L1.valid p x then ()
    else if L1.unsat p x then raise Unsatisfiable
    else if !critical then delay p x
    else process_critical p (V.find x)

  and delay p x =
    Stacks.push p preds ;
    Stacks.push x args

  and process_critical p x =
    let x = V.find x in
    if L1.valid p x then ()
    else if L1.unsat p x then raise Unsatisfiable
    else (
      if !footprint then Footprint.valid1 p x ;
      critical := true ;
      L1.process_pos p x ;
      !propagate_valid1 p x ;
      critical := false )

  let closed () = Stacks.is_empty preds

  let close () =
    while not (closed ()) do
      let p = Stacks.pop preds and x = Stacks.pop args in
      process_critical p x
    done
end

module Unsat1 = struct
  let preds = Stacks.create ()
  let args = Stacks.create ()

  let reset () =
    Stacks.clear preds ;
    Stacks.clear args

  let rec process p x =
    if L1.unsat p x then ()
    else if L1.valid p x then raise Unsatisfiable
    else if !critical then delay p x
    else process_critical p (V.find x)

  and delay p x =
    Stacks.push p preds ;
    Stacks.push x args

  and process_critical p x =
    let x = V.find x in
    if L1.unsat p x then ()
    else if L1.valid p x then raise Unsatisfiable
    else (
      if !footprint then Footprint.unsat1 p x ;
      critical := true ;
      L1.process_neg p x ;
      !propagate_unsat1 p x ;
      critical := false )

  let closed () = Stacks.is_empty preds

  let close () =
    while not (closed ()) do
      let p = Stacks.pop preds and x = Stacks.pop args in
      process_critical p x
    done
end

(** Closing inference systems. *)
module rec U : CC = Uopen (struct
  type var = Var.t

  let find = V.find
  let canonical = V.canonical
  let equal = V.equal
  let diseq = V.diseq
  let union = Union.process
end)

and A : SIMPLEX = Aopen (struct
  type var = Var.t

  let find x = V.find x
  let canonical x = V.canonical x
  let equal x y = V.equal x y
  let diseq x y = V.diseq x y
  let is_real x = L1.valid Predsym.real x
  let is_integer x = L1.valid Predsym.integer x
  let union x y = Union.process x y
  let separate x y = Separate.process x y
  let real x = L1.process_pos Predsym.real x
  let integer x = L1.process_pos Predsym.integer x
end)

and T : TUPLE = Topen (struct
  type var = Var.t

  let find = V.find
  let canonical = V.canonical
  let equal = V.equal
  let diseq = V.diseq
  let union = Union.process
  let separate = Separate.process
end)

and F : FUNARR = Fopen (struct
  type var = Var.t

  let find = V.find
  let canonical = V.canonical
  let equal = V.equal
  let diseq = V.diseq
  let choose_equiv = V.choose_equiv
  let iter_equiv = V.iter_equiv
  let iter_diseqs = V.iter_diseqs
  let union = Union.process

  let ite (x, y) (a, b) (c, d) =
    let z = Formula.Bdd.mk_posvar (R.alias_equal x y) in
    let p = Formula.Bdd.mk_posvar (R.alias_equal a b) in
    let n = Formula.Bdd.mk_posvar (R.alias_equal c d) in
    P.process_conjoin (Formula.Bdd.mk_ite z p n)

  let array = L1.process_pos Predsym.array
end)

and R : RENAME = Ropen (struct
  type propvar = Propvar.t
  type predsym = Predsym.t
  type var = Var.t

  let find = V.find
  let canonical = V.canonical
  let equal = V.equal
  let diseq = V.diseq
  let equiv = P.process_union
  let disjoint = P.process_separate
  let implies = P.process_sub
  let valid0 = Valid0.process
  let unsat0 = Unsat0.process
  let valid1 = Valid1.process
  let unsat1 = Unsat1.process
  let union = Union.process
  let separate = Separate.process
end)

and P : PROP = Popen (struct
  type var = Propvar.t

  let valid = Valid0.process
  let unsat = Unsat0.process
end)

let add_arith p t =
  match p with
  | Predsym.Arith.Nonneg -> A.process_nonneg t
  | Predsym.Arith.Pos -> A.process_pos t
  | Predsym.Arith.Equal0 -> A.process_eq0 t
  | Predsym.Arith.Diseq0 -> A.process_deq0 t
  | Predsym.Arith.Real -> L1.process_pos Predsym.real (A.alias t)
  | Predsym.Arith.Int -> L1.process_pos Predsym.integer (A.alias t)

let add_negarith p t =
  match p with
  | Predsym.Arith.Nonneg -> A.process_pos (Term.Polynomial.minus t)
  | Predsym.Arith.Pos -> A.process_nonneg (Term.Polynomial.minus t)
  | Predsym.Arith.Equal0 -> A.process_deq0 t
  | Predsym.Arith.Diseq0 -> A.process_eq0 t
  | Predsym.Arith.Real -> L1.process_neg Predsym.real (A.alias t)
  | Predsym.Arith.Int -> L1.process_neg Predsym.integer (A.alias t)

(* use following hack since recursive modules in ocaml 3.08 are pathetic.
   Instead the modules [Union] through [Valid1] should be part of the
   recursive modules above. *)
let _ =
  propagate_eq :=
    fun x y ->
      assert (V.canonical y) ;
      assert (not (V.canonical x)) ;
      assert (V.equal x y) ;
      U.close x y ;
      A.propagate_eq x y ;
      L1.propagate_eq x y ;
      T.propagate x y ;
      F.propagate_eq x y ;
      R.propagate_eq x y

let _ =
  propagate_deq :=
    fun x y ->
      let x = V.find x and y = V.find y in
      F.propagate_deq x y ;
      R.propagate_deq x y

let _ =
  propagate_valid0 :=
    fun p ->
      P.propagate_valid p ;
      R.propagate_valid0 p

let _ =
  propagate_unsat0 :=
    fun p ->
      P.propagate_unsat p ;
      R.propagate_unsat0 p

let _ =
  propagate_valid1 :=
    fun p x ->
      match p with
      | Predsym.Arith p -> add_arith p (Term.Polynomial.indet x)
      | _ -> R.propagate_valid1 p x

let _ =
  propagate_unsat1 :=
    fun p x ->
      match p with
      | Predsym.Arith p -> add_negarith p (Term.Polynomial.indet x)
      | _ -> R.propagate_unsat1 p x

type t =
  { context: Formula.t list
  ; p: P.t
  ; l0: L0.t
  ; l1: L1.t
  ; r: R.t
  ; v: V.t
  ; u: U.t
  ; a: A.t
  ; t: T.t
  ; f: F.t
  ; upper: int
  ; mutable status: status }

and status = Sat of Formula.t | Unsat of Formula.t list | Unknown

let empty =
  { context= []
  ; upper= 0
  ; status= Sat Formula.mk_true
  ; p= P.empty
  ; l0= L0.empty
  ; l1= L1.empty
  ; r= R.empty
  ; v= V.empty
  ; u= U.empty
  ; a= A.empty
  ; t= T.empty
  ; f= F.empty }

let print_context fmt =
  let rec loop = function
    | [] -> ()
    | [p] -> Formula.pp fmt p
    | p :: pl ->
        Formula.pp fmt p ;
        Format.fprintf fmt ", " ;
        loop pl
  in
  loop

let pp fmt s =
  Format.fprintf fmt "@[ctxt(" ;
  print_context fmt s.context ;
  Format.fprintf fmt ")@]@?"

let init = ref empty
let curr_context = ref []
let curr_status = ref Unknown
let pp_context () = print_context stdout !curr_context

module Vareqs = Maps.Make (Var) (Vars)

let var_equals () =
  let acc = Vareqs.empty () in
  let add x y =
    let y' = V.find y in
    try
      let zs = Vareqs.find y' acc in
      Vars.add x zs ;
      Vars.add y zs ;
      Vars.add y' zs ;
      Vareqs.set y' zs acc
    with Not_found ->
      let zs = Vars.empty () in
      Vars.add x zs ;
      Vars.add y zs ;
      Vars.add y' zs ;
      Vareqs.set y' zs acc
  in
  V.Equalities.iter add (V.equalities ()) ;
  acc

let var_diseqs () =
  let acc = Formulas.empty () in
  V.Disequalities.iter
    (fun (x, y) ->
      Formulas.add (Formula.mk_diseq (Term.of_var x) (Term.of_var y)) acc )
    (V.disequalities ()) ;
  acc

let constant_equals () =
  let acc = Formulas.empty () in
  A.R.Constant.iter
    (fun x c ->
      let e = Formula.mk_equal (Term.of_var x) (Term.of_num c) in
      Formulas.add e acc )
    (A.R.constant ()) ;
  acc

let regular_equals () =
  let acc = Formulas.empty () in
  A.R.Solset.iter
    (fun x p ->
      let e = Formula.mk_equal (Term.of_var x) (Term.of_poly p) in
      Formulas.add e acc )
    (A.R.solset ()) ;
  acc

let tableau_equals () =
  let acc = Formulas.empty () in
  A.T.Solset.iter
    (fun x p ->
      let e = Formula.mk_equal (Term.of_var x) (Term.of_poly p) in
      Formulas.add e acc )
    (A.T.solset ()) ;
  acc

let rec theory_equals i =
  match i with
  | A -> arith_equals ()
  | T -> tuple_equals ()
  | U -> uninterp_equals ()
  | F -> array_equals ()

and arith_equals () =
  let acc = constant_equals () in
  Formulas.union (regular_equals ()) acc ;
  Formulas.union (tableau_equals ()) acc ;
  acc

and tuple_equals () =
  let acc = Formulas.empty () in
  T.Subst.iter
    (fun x t ->
      let e = Formula.mk_equal (Term.of_var x) (Term.of_tuple t) in
      Formulas.add e acc )
    (T.config ()) ;
  acc

and array_equals () =
  let acc = Formulas.empty () in
  F.Cfg.iter
    (fun x t ->
      let e = Formula.mk_equal (Term.of_var x) (Term.of_array t) in
      Formulas.add e acc )
    (F.config ()) ;
  acc

and uninterp_equals () =
  let acc = Formulas.empty () in
  U.Find.iter
    (fun u a ->
      let e = Formula.mk_equal (Term.of_var u) (Term.of_apply a) in
      Formulas.add e acc )
    (U.context ()) ;
  acc

let slacks () =
  let acc = Vars.empty () in
  A.S.Slacks.iter (fun x -> Vars.add x acc) (A.S.current ()) ;
  acc

let literals () =
  let acc = Formulas.empty () in
  L0.Valid.iter
    (fun p -> Formulas.add (Formula.mk_prop (Formula.Bdd.mk_posvar p)) acc)
    (L0.valid ()) ;
  L0.Unsat.iter
    (fun p -> Formulas.add (Formula.mk_prop (Formula.Bdd.mk_negvar p)) acc)
    (L0.unsat ()) ;
  L1.Pos.iter
    (fun x ps ->
      L1.Set.iter
        (fun p -> Formulas.add (Formula.mk_apply p (Term.of_var x)) acc)
        ps )
    (L1.pos ()) ;
  L1.Neg.iter
    (fun x ps ->
      L1.Set.iter
        (fun p -> Formulas.add (Formula.mk_negapply p (Term.of_var x)) acc)
        ps )
    (L1.neg ()) ;
  acc

module Rename = Maps.Make (Propvar) (Formula)

let renames () =
  let acc = Rename.empty () in
  R.Monadic.iter
    (fun u (p, x) -> Rename.set u (Formula.mk_apply p (Term.of_var x)) acc)
    (R.monadic ()) ;
  R.Equal.iter
    (fun u (x, y) ->
      assert (not (Rename.mem u acc)) ;
      Rename.set u (Formula.mk_equal (Term.of_var x) (Term.of_var y)) acc )
    (R.equal ()) ;
  acc

let prop () = Formula.mk_prop (P.current ())

let rec pp_config () =
  pp_vareqs () ;
  pp_vardiseqs () ;
  pp_arith () ;
  pp_uninterp () ;
  pp_tuple () ;
  pp_array () ;
  pp_rename () ;
  pp_literals () ;
  pp_prop ()

and pp_vareqs () =
  let v = var_equals () in
  if not (Vareqs.is_empty v) then (
    Format.fprintf stdout "v: " ;
    Vareqs.pp stdout v ;
    Format.fprintf stdout "\n" )

and pp_vardiseqs () =
  let d = var_diseqs () in
  if not (Formulas.is_empty d) then (
    Format.fprintf stdout "d: " ;
    Formulas.pp stdout d ;
    Format.fprintf stdout "\n" )

and pp_arith () =
  let c = constant_equals () in
  let r = regular_equals () in
  let t = tableau_equals () in
  let sl = slacks () in
  if not (Formulas.is_empty c && Formulas.is_empty r && Formulas.is_empty t)
  then (
    if not (Formulas.is_empty c) then (
      Format.fprintf stdout "c: " ;
      Formulas.pp stdout c ;
      Format.fprintf stdout "\n" ) ;
    if not (Formulas.is_empty r) then (
      Format.fprintf stdout "r: " ;
      Formulas.pp stdout r ;
      Format.fprintf stdout "\n" ) ;
    if not (Formulas.is_empty t) then (
      Format.fprintf stdout "t: " ;
      Formulas.pp stdout t ;
      Format.fprintf stdout "\n" ) ;
    if not (Vars.is_empty sl) then (
      Format.fprintf stdout "slacks: " ;
      Vars.pp stdout sl ;
      Format.fprintf stdout "\n" ) )

and pp_uninterp () =
  let u = uninterp_equals () in
  if not (Formulas.is_empty u) then (
    Format.fprintf stdout "u: " ;
    Formulas.pp stdout u ;
    Format.fprintf stdout "\n" )

and pp_tuple () =
  let t = tuple_equals () in
  if not (Formulas.is_empty t) then (
    Format.fprintf stdout "t: " ;
    Formulas.pp stdout t ;
    Format.fprintf stdout "\n" )

and pp_array () =
  let f = array_equals () in
  if not (Formulas.is_empty f) then (
    Format.fprintf stdout "f: " ;
    Formulas.pp stdout f ;
    Format.fprintf stdout "\n" )

and pp_rename () =
  let r = renames () in
  if not (Rename.is_empty r) then (
    Format.fprintf stdout "r: " ;
    Rename.pp stdout r ;
    Format.fprintf stdout "\n" )

and pp_literals () =
  let r = literals () in
  if not (Formulas.is_empty r) then (
    Format.fprintf stdout "l: " ;
    Formulas.pp stdout r ;
    Format.fprintf stdout "\n" )

and pp_prop () =
  let p = prop () in
  if not (Formula.is_true p) then (
    Format.fprintf stdout "p: " ;
    Formula.pp stdout p ;
    Format.fprintf stdout "\n" )

let rec initialize s =
  init := s ;
  k := s.upper ;
  curr_context := s.context ;
  curr_status := s.status ;
  V.initialize s.v ;
  P.initialize s.p ;
  L0.initialize s.l0 ;
  L1.initialize s.l1 ;
  R.initialize s.r ;
  V.initialize s.v ;
  U.initialize s.u ;
  A.initialize s.a ;
  T.initialize s.t ;
  F.initialize s.f ;
  reset_channels ()

and reset_channels () =
  Union.reset () ;
  Separate.reset () ;
  Valid0.reset () ;
  Unsat0.reset () ;
  Valid1.reset () ;
  Unsat1.reset ()

let reset () = initialize empty

let unchanged () =
  P.unchanged ()
  && L0.unchanged ()
  && L1.unchanged ()
  && R.unchanged ()
  && V.unchanged ()
  && U.unchanged ()
  && A.unchanged ()
  && T.unchanged ()
  && F.unchanged ()

let closed () =
  Union.closed ()
  && Separate.closed ()
  && Valid0.closed ()
  && Unsat0.closed ()
  && Valid1.closed ()
  && Unsat1.closed ()

let current () =
  assert (closed ()) ;
  if unchanged () then !init
  else
    { context= !curr_context
    ; upper= !k
    ; status= !curr_status
    ; p= P.current ()
    ; l0= L0.current ()
    ; l1= L1.current ()
    ; r= R.current ()
    ; v= V.current ()
    ; u= U.current ()
    ; a= A.current ()
    ; t= T.current ()
    ; f= F.current () }

let find i x =
  match i with
  | U -> Term.of_var x
  | A -> Term.of_poly (A.find x)
  | T -> Term.of_tuple (T.find x)
  | F -> Term.of_array (F.find x)

let inv = function
  | Term.Uninterp a -> U.inv (Term.Uninterp.funsym a) (Term.Uninterp.arg a)
  | Term.Arith p -> A.inv p
  | Term.Tuple t -> T.inv t
  | Term.Array a -> F.inv a
  | Term.Var _ -> raise Not_found

let canonical = function
  | Term.Var x -> V.canonical x
  | Term.Uninterp a -> (
      let f = Term.Uninterp.funsym a and x = Term.Uninterp.arg a in
      V.canonical x
      &&
      try
        let _ = U.inv f x in
        false
      with Not_found -> true )
  | Term.Array a -> (
    try
      let _ = F.inv a in
      false
    with Not_found -> (
      match a with
      | Term.Array.Update (a, i, x) ->
          V.canonical a && V.canonical i && V.canonical x
      | Term.Array.Lookup (a, i) -> V.canonical a && V.canonical i ) )
  | Term.Arith p -> (
      let canonical_m x _ = V.canonical x in
      Term.Polynomial.Map.for_all canonical_m p.Term.Polynomial.monomials
      &&
      try
        let _ = A.inv p in
        false
      with Not_found -> true )
  | _ -> true

let can t =
  let t' =
    match t with
    | Term.Var x ->
        let y = V.find x in
        if Var.equal x y then t else Term.of_var y
    | Term.Uninterp a -> (
        let f = Term.Uninterp.funsym a and x = Term.Uninterp.arg a in
        let y = V.find x in
        try Term.of_var (V.find (U.inv f y))
        with Not_found ->
          if Var.equal x y then t
          else Term.of_apply (Term.Uninterp.make f y) )
    | Term.Arith p -> (
        let q = Term.Polynomial.map A.find p in
        try Term.of_var (V.find (A.inv q))
        with Not_found ->
          if Term.Polynomial.equal p q then t else Term.of_poly q )
    | Term.Tuple tt -> (
        let tt' = Term.Tuple.map T.find tt in
        try Term.of_var (V.find (T.inv tt'))
        with Not_found ->
          if Term.Tuple.equal tt tt' then t else Term.of_tuple tt' )
    | Term.Array a -> (
        let a' = Term.Array.map V.find a in
        try Term.of_var (V.find (F.inv a'))
        with Not_found ->
          if Term.Array.equal a a' then t else Term.of_array a' )
  in
  assert (canonical t') ;
  t'

let diseq =
  let memoize deq x y =
    let res = deq x y in
    if res then Separate.process x y ;
    res
  in
  fun x y ->
    let x = V.find x and y = V.find y in
    memoize V.diseq x y || memoize U.diseq x y || memoize L1.diseq x y

let alias = function
  | Term.Var x -> V.find x
  | Term.Arith p -> A.alias p
  | Term.Tuple t -> T.alias t
  | Term.Array a -> F.alias a
  | Term.Uninterp a ->
      U.alias (Term.Uninterp.funsym a) (Term.Uninterp.arg a)

let term2poly = function
  | Term.Var n -> (
    try A.find n with Not_found -> Term.Polynomial.indet n )
  | Term.Arith q -> q
  | t -> Term.Polynomial.indet (alias t)

let poly2term p =
  try Term.of_var (V.find (A.inv p)) with Not_found -> Term.of_poly p

let zero () = A.alias Term.Polynomial.zero
let do_normalize = ref true
let is_sat () = R.is_empty () || P.is_valid ()
let is_unsat () = match !curr_status with Unsat _ -> true | _ -> false

let apply_process process p =
  try process p
  with
  | Unsatisfiable | L0.Unsat | L1.Unsat | A.Unsat | V.Unsat | P.Unsat ->
    curr_status := Unsat !curr_context ;
    reset_channels () ;
    raise Unsatisfiable

let rec process p =
  assert (closed ()) ;
  if !footprint then Footprint.process p ;
  let add p =
    if Formula.is_true p || is_unsat () then ()
    else (
      curr_context := p :: !curr_context ;
      add_formula p ;
      close () ;
      if !do_normalize then normalize () ;
      curr_status := if is_sat () then Sat Formula.mk_true else Unknown ;
      assert (closed ()) )
  in
  apply_process add p

and add_formula = function
  | Formula.Equal (s, t) -> add_equal s t
  | Formula.Diseq (s, t) -> add_diseq s t
  | Formula.Arith (p, t) -> add_arith p t
  | Formula.Poslit (p, t) -> add_poslit p t
  | Formula.Neglit (p, t) -> add_neglit p t
  | Formula.Prop b -> P.process_conjoin b

and add_equal s t =
  if Term.equal s t then ()
  else
    match (s, t) with
    | Term.Arith p, Term.Arith q ->
        A.process_eq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | Term.Arith p, _ ->
        let q = Term.Polynomial.indet (alias t) in
        A.process_eq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | _, Term.Arith q ->
        let p = Term.Polynomial.indet (alias s) in
        A.process_eq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | Term.Tuple ss, Term.Tuple tt -> T.process_eq ss tt
    | Term.Tuple ss, _ ->
        let tt = Term.Tuple.of_var (alias t) in
        T.process_eq ss tt
    | _, Term.Tuple tt ->
        let ss = Term.Tuple.of_var (alias s) in
        T.process_eq ss tt
    | _ ->
        let x = alias s and y = alias t in
        if V.equal x y then () else Union.process x y

and add_diseq s t =
  if Term.equal s t then raise Unsatisfiable
  else
    match (s, t) with
    | Term.Arith p, Term.Arith q ->
        A.process_deq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | Term.Arith p, _ ->
        let q = Term.Polynomial.indet (alias t) in
        A.process_deq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | _, Term.Arith q ->
        let p = Term.Polynomial.indet (alias s) in
        A.process_deq0 (Term.Polynomial.sub (A.can p) (A.can q))
    | _ ->
        let x = alias s and y = alias t in
        Separate.process x y

and add_poslit p t =
  match p with
  | Predsym.Arith p -> add_arith p (term2poly t)
  | _ -> L1.process_pos p (alias t)

and add_neglit p t =
  match p with
  | Predsym.Arith p -> add_negarith p (term2poly t)
  | _ -> L1.process_neg p (alias t)

and close () =
  if closed () then ()
  else (
    if !footprint then Footprint.close () ;
    Union.close () ;
    Separate.close () ;
    Valid0.close () ;
    Unsat0.close () ;
    Valid1.close () ;
    Unsat1.close () ;
    close () )

and normalize () =
  A.normalize () ;
  T.normalize ()

let context () = !curr_context
let status () = !curr_status
let equal s1 s2 = s1.context == s2.context

let descendant s1 s2 =
  let c1 = s1.context and c2 = s2.context in
  let rec search c = c == c2 || ((not (c = [])) && search (List.tl c)) in
  search c1

let var n = Term.of_var (V.find n)

let is_num c =
  let poly_is_num c p =
    try Q.equal c (Term.Polynomial.d_constant p)
    with Term.Polynomial.Nonnum -> false
  in
  function
  | Term.Var n -> (
    try poly_is_num c (A.find (V.find n)) with Not_found -> false )
  | Term.Arith p -> poly_is_num c p
  | _ -> false

let d_num t =
  try
    match t with
    | Term.Var x -> Term.Polynomial.d_constant (A.find (V.find x))
    | Term.Arith p -> Term.Polynomial.d_constant p
    | _ -> raise Not_found
  with Term.Polynomial.Nonnum -> raise Not_found

let max t =
  let p = A.can (term2poly t) in
  if A.restricted p then poly2term (A.max p) else t

let min t =
  let p = A.can (Term.Polynomial.minus (term2poly t)) in
  if A.restricted p then poly2term (Term.Polynomial.minus (A.max p)) else t

let sup t =
  let p = A.can (term2poly t) in
  if not (A.restricted p) then raise Not_found
  else
    let p = A.max p in
    if A.maximized p then Term.Polynomial.const p else raise Not_found

let inf t =
  let p = A.can (term2poly t) in
  if not (A.restricted p) then raise Not_found
  else
    let p = A.max (Term.Polynomial.minus p) in
    if A.maximized p then Q.minus (Term.Polynomial.const p)
    else raise Not_found

let bigint k = poly2term (Term.Polynomial.constant (Q.of_z k))
let constz k = poly2term (Term.Polynomial.constant (Q.of_int k))

let zero =
  let z = Term.Polynomial.constant (Q.of_int 0) in
  fun () -> poly2term z

let constq n d =
  let q = Q.make n d in
  poly2term (Term.Polynomial.constant q)

let mult c t =
  assert (canonical t) ;
  if Q.is_zero c then constz 0
  else if Q.is_one c then t
  else
    let p =
      try Term.Polynomial.constant (Q.mult c (d_num t))
      with Not_found -> Term.Polynomial.multc c (term2poly t)
    in
    poly2term p

let multz n = mult (Q.of_int n)
let multq = mult

let addc c t =
  if Q.is_zero c then t
  else
    let p =
      try Term.Polynomial.constant (Q.add c (d_num t))
      with Not_found -> Term.Polynomial.addc c (term2poly t)
    in
    poly2term p

let add s t =
  try addc (d_num s) t
  with Not_found -> (
    try addc (d_num t) s
    with Not_found ->
      poly2term (Term.Polynomial.add (term2poly s) (term2poly t)) )

let sub s t =
  try addc (Q.minus (d_num t)) s
  with Not_found ->
    poly2term (Term.Polynomial.sub (term2poly s) (term2poly t))

let minus t = poly2term (Term.Polynomial.minus (term2poly t))

let term2tuple = function
  | Term.Var n -> ( try T.find n with Not_found -> Term.Tuple.of_var n )
  | Term.Tuple t -> t
  | t -> Term.Tuple.of_var (alias t)

let tuple2term t =
  try Term.of_var (V.find (T.inv t)) with Not_found -> Term.of_tuple t

let nil () = tuple2term Term.Tuple.nil
let pair s t = tuple2term (Term.Tuple.pair (term2tuple s) (term2tuple t))

let triple s t r =
  tuple2term
    (Term.Tuple.triple (term2tuple s) (term2tuple t) (term2tuple r))

let tuple = function
  | [] -> nil ()
  | [t] -> t
  | [s; t] -> pair s t
  | [s; t; r] -> triple s t r
  | tl ->
      let (n : int) = List.length tl in
      let a = Array.make n (Obj.magic 0) in
      for i = 0 to n - 1 do
        let (t : Term.Tuple.t) = term2tuple (List.nth tl i) in
        a.(i) <- t
      done ;
      tuple2term (Term.Tuple.tuple a)

let proj i n t = tuple2term (Term.Tuple.proj i n (term2tuple t))
let left = proj 0 2
let right = proj 1 2

let apply f t =
  let n = alias t in
  try Term.of_var (V.find (U.inv f n))
  with Not_found -> Term.of_apply (Term.Uninterp.make f n)

let constant f = apply f (nil ())

let term2array = function
  | Term.Var n -> F.find n
  | Term.Array a -> a
  | _ -> raise Not_found

let array2term a =
  try Term.of_var (V.find (F.inv a)) with Not_found -> Term.of_array a

let lookup a i =
  let i = alias i in
  match a with
  | Term.Array (Term.Array.Update (_, j, y)) when V.equal i j ->
      Term.of_var y
  | Term.Array (Term.Array.Update (b, j, _)) when V.diseq i j ->
      array2term (Term.Array.lookup b i)
  | _ -> array2term (Term.Array.lookup (alias a) i)

let rec update a i x =
  let i = alias i in
  let x = alias x in
  match a with
  | Term.Array (Term.Array.Update (b, j, _)) when V.equal i j ->
      update_var b i x
  | Term.Array (Term.Array.Update (b, j, y))
    when Var.compare i j > 0 && V.diseq i j ->
      update_var (alias (update_var b i x)) j y
  | _ -> update_var (alias a) i x

and update_var a i x = array2term (Term.Array.update a i x)

let do_minimize = ref false

let with_do_minimize arith_test p =
  let save_flag = !A.complete in
  try
    A.complete := !do_minimize ;
    let res = arith_test p in
    A.complete := save_flag ;
    res
  with exc ->
    A.complete := save_flag ;
    raise exc

let rec valid p =
  match p with
  | Formula.Equal (s, t) -> valid_equal s t
  | Formula.Diseq (s, t) -> valid_diseq s t
  | Formula.Poslit (p, t) -> valid_poslit p t
  | Formula.Neglit (p, t) -> valid_neglit p t
  | Formula.Arith (p, t) -> valid_arith p t
  | Formula.Prop b -> valid_bdd b

and valid_equal s t =
  let s = can s and t = can t in
  Term.equal s t

and valid_diseq s t =
  match (can s, can t) with
  | Term.Var x, Term.Var y -> diseq x y
  | s, t -> Term.diseq s t

and valid_bdd b =
  if Formula.Bdd.is_valid b then true
  else if Formula.Bdd.is_unsat b then false
  else
    let p = Formula.Bdd.cond b
    and pos = Formula.Bdd.pos b
    and neg = Formula.Bdd.neg b in
    if L0.is_valid p then valid_bdd pos
    else if L0.is_unsat p then valid_bdd neg
    else Formula.Bdd.implies (P.current ()) b

and valid_poslit p t =
  match p with
  | Predsym.Arith p -> valid_arith p (term2poly t)
  | Predsym.Uninterp p -> valid_uninterp p t
  | Predsym.Tuple n -> valid_tuple n t
  | Predsym.Array -> valid_array t

and valid_arith p t =
  match p with
  | Predsym.Arith.Nonneg -> with_do_minimize A.is_nonneg t
  | Predsym.Arith.Pos -> with_do_minimize A.is_pos t
  | Predsym.Arith.Equal0 -> with_do_minimize A.is_equal0 t
  | Predsym.Arith.Diseq0 -> with_do_minimize A.is_diseq0 t
  | Predsym.Arith.Real -> (
    try L1.valid Predsym.real (Term.Polynomial.d_indet t) with _ -> false )
  | Predsym.Arith.Int -> (
    try L1.valid Predsym.integer (Term.Polynomial.d_indet t)
    with _ -> false )

and valid_uninterp p t =
  match can t with
  | Term.Var x -> L1.valid (Predsym.uninterp (Name.to_string p)) x
  | _ -> false

and valid_tuple n t =
  match can t with
  | Term.Var x -> L1.valid (Predsym.tuple n) x
  | Term.Tuple tt when Term.Tuple.is_tuple n tt -> true
  | _ -> false

and valid_array t =
  match can t with
  | Term.Var x -> L1.valid Predsym.array x
  | Term.Array _ -> true
  | _ -> false

and valid_neglit _p _t = false

let tt = Formula.mk_prop Formula.Bdd.mk_true
let ff = Formula.mk_prop Formula.Bdd.mk_false

let posvar p =
  if L0.is_valid p then tt
  else if L0.is_unsat p then ff
  else Formula.mk_prop (Formula.Bdd.mk_posvar p)

let negvar p =
  if L0.is_valid p then ff
  else if L0.is_unsat p then tt
  else Formula.mk_prop (Formula.Bdd.mk_negvar p)

let equal0 p =
  let p = A.can p in
  if A.is_equal0 p then tt
  else if A.is_diseq0 p then ff
  else Formula.mk_arith Predsym.Arith.Equal0 p

let eq s t =
  let mk_eq s t =
    if Term.compare s t > 0 then Formula.mk_equal s t
    else Formula.mk_equal t s
  in
  let mk_eq0 p q =
    equal0
      ( if Q.compare (Term.Polynomial.const p) (Term.Polynomial.const q) <= 0
      then Term.Polynomial.sub p q
      else Term.Polynomial.sub q p )
  in
  let s = can s in
  let t = can t in
  if is_num Q.zero t then equal0 (term2poly s)
  else if is_num Q.zero s then equal0 (term2poly t)
  else
    match (s, t) with
    | Term.Var x, Term.Var y ->
        if V.equal x y then tt else if diseq x y then ff else mk_eq s t
    | Term.Arith p, Term.Arith q -> mk_eq0 p q
    | Term.Arith p, _ -> mk_eq0 p (term2poly t)
    | _, Term.Arith q -> mk_eq0 (term2poly s) q
    | _ ->
        if Term.equal s t then tt
        else if Term.diseq s t then ff
        else mk_eq s t

let diseq0 p =
  let p = A.can p in
  if A.is_equal0 p then ff
  else if A.is_diseq0 p then tt
  else if Term.Polynomial.is_monomial p then (
    let x = Term.Polynomial.indet_of_monomial p in
    assert (not (Q.equal (Term.Polynomial.coeff_of_monomial p) Q.zero)) ;
    Formula.mk_arith Predsym.Arith.Diseq0 (Term.Polynomial.indet x) )
  else Formula.mk_arith Predsym.Arith.Diseq0 p

let deq s t =
  let mk_deq s t =
    if Term.compare s t > 0 then Formula.mk_diseq s t
    else Formula.mk_diseq t s
  in
  if Term.equal s t then ff
  else if Term.diseq s t then tt
  else
    match (s, t) with
    | Term.Var x, Term.Var y ->
        if V.equal x y then ff else if diseq x y then tt else mk_deq s t
    | Term.Arith p, Term.Arith q -> diseq0 (Term.Polynomial.sub p q)
    | Term.Arith p, _ -> diseq0 (Term.Polynomial.sub p (term2poly t))
    | _, Term.Arith q -> diseq0 (Term.Polynomial.sub (term2poly s) q)
    | _ -> mk_deq s t

let nonneg t =
  let p = A.can (term2poly t) in
  let c = Term.Polynomial.const p in
  if Q.is_nonneg c && A.minimized p then tt
  else if Q.is_neg c && A.maximized p then ff
  else if !do_minimize && A.restricted p then
    let p = A.min p in
    if Q.is_nonneg c && A.minimized p then tt
    else if Q.is_neg c && A.maximized p then ff
    else Formula.mk_arith Predsym.Arith.Nonneg p
  else Formula.mk_arith Predsym.Arith.Nonneg p

let pos t =
  let p = A.can (term2poly t) in
  let c = Term.Polynomial.const p in
  if Q.is_pos c && A.minimized p then tt
  else if Q.is_nonpos c && A.maximized p then ff
  else if !do_minimize && A.restricted p then
    let p = A.min p in
    if Q.is_pos c && A.minimized p then tt
    else if Q.is_nonpos c && A.maximized p then ff
    else Formula.mk_arith Predsym.Arith.Pos p
  else Formula.mk_arith Predsym.Arith.Pos p

let ge s t = nonneg (sub s t)
let gt s t = pos (sub s t)
let le s t = ge t s
let lt s t = gt t s

let is_tuple n = function
  | Term.Tuple t -> if Term.Tuple.is_tuple n t then tt else ff
  | Term.Var x ->
      let p = Predsym.tuple n in
      if L1.valid p x then tt
      else if L1.unsat p x then ff
      else Formula.mk_apply p (Term.of_var x)
  | t -> Formula.mk_apply (Predsym.tuple n) t

let is_array t =
  match t with
  | Term.Tuple _ | Term.Arith _ -> ff
  | Term.Var x ->
      if L1.valid Predsym.array x then tt
      else if L1.unsat Predsym.array x then ff
      else Formula.mk_apply Predsym.array t
  | _ -> Formula.mk_apply Predsym.array t

let is_real t =
  assert (canonical t) ;
  match t with
  | Term.Arith _ -> tt
  | Term.Var x ->
      if L1.valid Predsym.real x then tt
      else if L1.unsat Predsym.real x then ff
      else Formula.mk_apply Predsym.real t
  | Term.Tuple _ | Term.Array _ -> ff
  | Term.Uninterp _ -> Formula.mk_apply Predsym.real t

let is_integer t =
  assert (canonical t) ;
  match t with
  | Term.Arith _ -> ff (* needs to be made more precise *)
  | Term.Var x ->
      if L1.valid Predsym.integer x then tt
      else if L1.unsat Predsym.integer x then ff
      else Formula.mk_apply Predsym.integer t
  | Term.Tuple _ | Term.Array _ -> ff
  | Term.Uninterp _ -> Formula.mk_apply Predsym.integer t

let uninterp p t =
  try
    let x = inv t in
    if L1.valid p x then tt
    else if L1.unsat p x then ff
    else Formula.mk_apply p t
  with Not_found -> Formula.mk_apply p t

let poslit p t =
  let t = can t in
  match p with
  | Predsym.Arith Predsym.Arith.Nonneg -> nonneg t
  | Predsym.Arith Predsym.Arith.Pos -> pos t
  | Predsym.Arith Predsym.Arith.Real -> is_real t
  | Predsym.Arith Predsym.Arith.Int -> is_integer t
  | Predsym.Arith Predsym.Arith.Equal0 -> equal0 (term2poly t)
  | Predsym.Arith Predsym.Arith.Diseq0 -> diseq0 (term2poly t)
  | Predsym.Uninterp _ -> uninterp p t
  | Predsym.Tuple n -> is_tuple n t
  | Predsym.Array -> is_array t

let rec neglit p t =
  let t = can t in
  match p with
  | Predsym.Arith p -> neg_arith p (term2poly t)
  | _ ->
      let u = R.alias_monadic p (alias t) in
      Formula.mk_prop (Formula.Bdd.mk_negvar u)

and neg_arith p t =
  match p with
  | Predsym.Arith.Nonneg -> pos (poly2term (Term.Polynomial.minus t))
  | Predsym.Arith.Pos -> nonneg (poly2term (Term.Polynomial.minus t))
  | Predsym.Arith.Equal0 -> diseq0 t
  | Predsym.Arith.Diseq0 -> equal0 t
  | Predsym.Arith.Real ->
      let u = R.alias_monadic Predsym.real (A.alias t) in
      Formula.mk_prop (Formula.Bdd.mk_negvar u)
  | Predsym.Arith.Int ->
      let u = R.alias_monadic Predsym.integer (A.alias t) in
      Formula.mk_prop (Formula.Bdd.mk_negvar u)

let formula2bdd = function
  | Formula.Equal (s, t) ->
      Formula.Bdd.mk_posvar (R.alias_equal (alias s) (alias t))
  | Formula.Diseq (s, t) ->
      Formula.Bdd.mk_negvar (R.alias_equal (alias s) (alias t))
  | Formula.Arith (p, t) ->
      Formula.Bdd.mk_posvar
        (R.alias_monadic (Predsym.of_arith p) (A.alias t))
  | Formula.Poslit (p, t) ->
      Formula.Bdd.mk_posvar (R.alias_monadic p (alias t))
  | Formula.Neglit (p, t) ->
      Formula.Bdd.mk_negvar (R.alias_monadic p (alias t))
  | Formula.Prop b -> b

let neg = function
  | Formula.Equal (s, t) -> deq s t
  | Formula.Diseq (s, t) -> eq s t
  | Formula.Poslit (p, t) -> neglit p t
  | Formula.Neglit (p, t) -> poslit p t
  | Formula.Arith (p, t) -> neg_arith p t
  | Formula.Prop b -> Formula.mk_prop (Formula.Bdd.mk_neg b)

let valid_atom p =
  match p with
  | Formula.Equal (s, t) -> valid_equal s t
  | Formula.Diseq (s, t) -> valid_diseq s t
  | Formula.Poslit (p, t) -> valid_poslit p t
  | Formula.Arith (p, t) -> valid_arith p t
  | Formula.Neglit _ -> false
  | Formula.Prop _ -> false

let andthen p q =
  if Formula.equal p q then p
  else if valid_atom p then q
  else if valid_atom q then p
  else
    match (p, q) with
    | Formula.Poslit (qsym, s), Formula.Poslit (psym, t)
      when Term.equal s t && Predsym.sub qsym psym ->
        q
    | Formula.Poslit (qsym, s), Formula.Poslit (psym, t)
      when Term.equal s t && Predsym.sub psym qsym ->
        p
    | _ ->
        let b = formula2bdd p and c = formula2bdd q in
        if Formula.Bdd.is_unsat b || Formula.Bdd.is_unsat c then ff
        else if Formula.Bdd.is_valid b then q
        else if Formula.Bdd.is_valid c then p
        else Formula.mk_prop (Formula.Bdd.mk_conj b c)

let orelse p q =
  if Formula.equal p q then p
  else if valid_atom p then tt
  else if valid_atom q then ff
  else
    let b = formula2bdd p and c = formula2bdd q in
    if Formula.Bdd.is_valid b || Formula.Bdd.is_valid c then tt
    else if Formula.Bdd.is_unsat b then q
    else if Formula.Bdd.is_unsat c then p
    else Formula.mk_prop (Formula.Bdd.mk_disj b c)

let equiv p q =
  if Formula.equal p q then tt
  else if valid_atom p then p
  else if valid_atom q then p
  else
    let b = formula2bdd p and c = formula2bdd q in
    Formula.mk_prop (Formula.Bdd.mk_iff b c)

let implies p q =
  if Formula.equal p q then tt
  else if valid_atom q then tt
  else
    let b = formula2bdd p and c = formula2bdd q in
    Formula.mk_prop (Formula.Bdd.mk_imp b c)

let ite p q r =
  if Formula.equal q r then q
  else if valid_atom p then q
  else
    let b = formula2bdd p in
    if Formula.Bdd.is_valid b then q
    else if Formula.Bdd.is_unsat b then r
    else
      let c = formula2bdd q and d = formula2bdd r in
      Formula.mk_prop (Formula.Bdd.mk_ite b c d)

let xor p q = ite p (neg q) q

let apply_with s f a =
  assert (closed ()) ;
  let save = current () in
  initialize s ;
  try
    let b = f a in
    initialize save ;
    b
  with exc ->
    initialize save ;
    raise exc

(** The [s] argument is the context, [c] is the unprocessed literals, and
    [a] is the accumulator. The invariants are that

    - [a] is always already asserted in context [s], and
    - [assert(s, C)] is inconsistent.

    The algorithm is based on Juncker's quick explain:

    {v
       xp B C A = 
         if empty?(C) || inconsistent?(B) then A else 
           if singleton?(C) then A U C else 
              let (C1,C2) = split(C) in
              let B' = assert(B, C1) in
              let A' = xp(B', C2, A) in
              let B'' = assert(S, A') in
                  xp(B'', C1, A')
    v} *)
let rec explain pl =
  let save = current () in
  try
    let ql = xp empty pl [] in
    initialize save ;
    ql
  with exc ->
    initialize save ;
    raise exc

and xp b c a =
  let split c =
    let rec spl c1 c2 n =
      assert (c2 <> []) ;
      match (n, c2) with
      | 0, _ -> (c1, c2)
      | n, e :: c2' -> spl (e :: c1) c2' (n - 1)
      | _ -> assert false
    in
    assert (List.length c >= 2) ;
    spl [] c (List.length c / 2)
  in
  let processl s pl =
    initialize s ;
    let todo = ref pl in
    while !todo <> [] do
      try
        process (List.hd !todo) ;
        todo := List.tl !todo
      with Unsatisfiable -> todo := []
    done ;
    current ()
  in
  match b.status with
  | Unsat _ -> a
  | _ -> (
    match c with
    | [] -> a
    | [p] -> p :: a
    | _ ->
        let c1, c2 = split c in
        let b' = processl b c1 in
        let a' = xp b' c2 a in
        let b'' = processl b a' in
        xp b'' c1 a' )

type resolve = Implicant of Formula.Bdd.t | Learned of Formula.Bdd.t

(** [implicant()] returns either an implicant or raises [Unsatisfiable]. *)
let rec implicant =
  let queue = Queue.create () in
  let conjoin_learned b =
    P.process_conjoin b ;
    close ()
  in
  fun () ->
    let rec loop () =
      let b = P.current () in
      if Formula.Bdd.is_unsat b then raise Unsatisfiable
      else if Formula.Bdd.is_valid b then b
      else (
        Formula.Bdd.implicant b queue ;
        if !footprint then
          Footprint.implicant_candidate (Queue.length queue) ;
        match process_implicant queue with
        | Implicant impl ->
            if !footprint then Footprint.implicant impl ;
            impl
        | Learned l ->
            if !footprint then Footprint.learned l ;
            apply_process conjoin_learned l ;
            loop () )
    in
    loop ()

and process_implicant imp =
  if Queue.is_empty imp then Implicant Formula.Bdd.mk_true
  else
    let ctxt = ref Formula.Bdd.mk_true in
    let learned = ref Formula.Bdd.mk_true in
    let save = current () in
    try
      initialize save ;
      while not (Queue.is_empty imp) do
        match Queue.take imp with
        | true, p ->
            let curr = Formula.Bdd.mk_posvar p in
            if L0.is_valid p then
              learned :=
                Formula.Bdd.mk_conj !learned (Formula.Bdd.mk_imp !ctxt curr)
            else (
              ctxt := Formula.Bdd.mk_conj !ctxt curr ;
              process (Formula.mk_prop curr) )
        | false, p ->
            let curr = Formula.Bdd.mk_negvar p in
            if L0.is_unsat p then
              learned :=
                Formula.Bdd.mk_conj !learned (Formula.Bdd.mk_imp !ctxt curr)
            else (
              ctxt := Formula.Bdd.mk_conj !ctxt curr ;
              process (Formula.mk_prop curr) )
      done ;
      Implicant !ctxt
    with Unsatisfiable ->
      initialize save ;
      (* Learned(Formula.Bdd.mk_conj (Formula.Bdd.mk_neg !ctxt) !learned) *)
      Learned (Formula.Bdd.mk_neg !ctxt)

let unsat_cores = ref true

let resolve () =
  match status () with
  | Unsat _ as st -> st
  | Sat _ as st -> st
  | Unknown -> (
      if !footprint then Footprint.resolve () ;
      try
        let impl = implicant () in
        assert (not (Formula.Bdd.is_unsat impl)) ;
        curr_status := Sat (Formula.mk_prop impl) ;
        !curr_status
      with Unsatisfiable ->
        let xp =
          if !unsat_cores then explain !curr_context else !curr_context
        in
        curr_status := Unsat xp ;
        !curr_status )

let complete_tests = ref false

let rec implied fml = valid fml || valid_complete fml

and valid_complete fml =
  assert (closed ()) ;
  let check p =
    try
      process (neg p) ;
      false
    with Unsatisfiable -> true
  in
  apply_with (current ()) check fml
