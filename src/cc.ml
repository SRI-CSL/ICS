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

module type FUNSYM = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type APPLY = sig
  type funsym
  type var
  type t = private {funsym: funsym; arg: var}

  val funsym : t -> funsym
  val arg : t -> var
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val make : funsym -> var -> t
end

module Apply (Var : VAR) (Funsym : FUNSYM) = struct
  type var = Var.t
  type funsym = Funsym.t
  type t = {funsym: Funsym.t; arg: Var.t}

  let funsym a = a.funsym
  let arg a = a.arg
  let equal a b = Funsym.equal a.funsym b.funsym && Var.equal a.arg b.arg

  let compare a b =
    let c = Funsym.compare a.funsym b.funsym in
    if c <> 0 then c else Var.compare a.arg b.arg

  let hash a = (Funsym.hash a.funsym + Var.hash a.arg) land 0x3FFFFFFF

  let pp fmt a =
    Funsym.pp fmt a.funsym ;
    Format.fprintf fmt "(" ;
    Var.pp fmt a.arg ;
    Format.fprintf fmt ")"

  let make f x = {funsym= f; arg= x}
end

module type INTERFACE = sig
  type var

  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val union : var -> var -> unit
end

module type INFSYS = sig
  type var
  type funsym
  type apply
  type t

  val empty : t
  val current : unit -> t
  val initialize : t -> unit
  val reset : unit -> unit

  module Find : Maps.S with type key = var and type value = apply

  val context : unit -> Find.t
  val unchanged : unit -> bool
  val is_empty : unit -> bool
  val congruence_closed : unit -> bool
  val lookup : var -> apply
  val inv : funsym -> var -> var
  val dom : var -> bool
  val diseq : var -> var -> bool
  val alias : funsym -> var -> var
  val close : var -> var -> unit
end

module Make
    (Var : VAR)
    (Funsym : FUNSYM)
    (Apply : APPLY with type var = Var.t and type funsym = Funsym.t)
    (V : INTERFACE with type var = Var.t) =
struct
  type var = Var.t
  type funsym = Funsym.t
  type apply = Apply.t

  module Varset = Setunion.Make (Var)
  module Find = Maps.Make (Var) (Apply)
  module Dep = Maps.Make (Var) (Varset)

  (** Representation of a context with [find] bindings [u |-> f(x)]. An
      index [dep] is maintained with bindings [x |-> {u{1},...,u{n}}] such
      that all [u{i}] are domain variables in [find] and [x] is
      [V-canonical].

      In addition, for all bindings [u |-> f(x')] in [find] there is a
      [V]-canonical [x] in [dep] with [x |-> {u{1},...,u{n}}] such that

      - [x =V x'] and
      - the variables in [{u{1},...,u{n}}] are exactly the domain variables
        with [V, U |= u{i} = f(x)] *)
  type t = {mutable find: Find.t; mutable dep: Dep.t}

  let empty = {find= Find.empty (); dep= Dep.empty ()}

  (** Initial configuration. *)
  let init = ref empty

  (** Current configuration. *)
  module Config = struct
    module Find = Config.Map (Find)
    module Dep = Config.Map (Dep)
  end

  let context = Config.Find.current
  let apply x = Find.find x (context ())
  let dom x = Find.mem x (context ())

  let dep x =
    try Dep.find (V.find x) (Config.Dep.current ())
    with Not_found -> Varset.empty

  let well_formed () =
    let for_all p = Dep.for_all (fun y -> Varset.for_all (p y)) in
    let ok y x =
      V.canonical y
      &&
      let {Apply.funsym= _; Apply.arg= y'} = apply x in
      V.equal y' y
    in
    for_all ok (Config.Dep.current ())

  let congruence_closed () =
    let for_all f = Find.for_all f (Config.Find.current ()) in
    for_all (fun u {Apply.funsym= _f; Apply.arg= x} ->
        for_all (fun v {Apply.funsym= _g; Apply.arg= y} ->
            (* CR: why ignore funsyms? *)
            if V.equal x y then V.equal u v else true ) )

  let initialize s =
    init := s ;
    Config.Find.initialize s.find ;
    Config.Dep.initialize s.dep

  let reset () = initialize empty
  let unchanged () = Config.Find.unchanged () && Config.Dep.unchanged ()

  let current () =
    if unchanged () then !init
    else {find= Config.Find.current (); dep= Config.Dep.current ()}

  let is_empty () = Find.is_empty (Config.Find.current ())

  let lookup u =
    (* CR: suspicious ignored arg of test *)
    let test v {Apply.funsym= _; Apply.arg= _y} = V.equal u v in
    let _, ({Apply.funsym= f; Apply.arg= y} as a) =
      Find.choose_if test (Config.Find.current ())
    in
    let y' = V.find y in
    if y == y' then a else Apply.make f y'

  let inv f y =
    let xs = dep y in
    if Varset.is_empty xs then raise Not_found
    else
      let found x =
        assert (dom x) ;
        let a = apply x in
        Funsym.equal f (Apply.funsym a) && V.equal (V.find (Apply.arg a)) y
      in
      Varset.choose found xs

  let alias f y =
    try inv f y
    with Not_found ->
      let u = Var.fresh () in
      let y = V.find y in
      assert (not (Varset.mem u (dep y))) ;
      Config.Find.set u (Apply.make f y) ;
      Config.Dep.set y (Varset.add u (dep y)) ;
      u

  let close x y =
    assert (not (V.canonical x)) ;
    assert (V.canonical y) ;
    assert (V.equal x y) ;
    let dep = Config.Dep.current () in
    let us = try Dep.find x dep with Not_found -> Varset.empty in
    let vs = try Dep.find y dep with Not_found -> Varset.empty in
    let vs' = ref vs in
    (* don't use canonical form for getting dependencies since those have
       not been synchronized yet. *)
    Varset.iter
      (fun u ->
        try
          let {Apply.funsym= f; Apply.arg= x'} = apply u in
          assert (V.equal x x') ;
          Varset.iter
            (fun v ->
              assert (not (Var.equal u v)) ;
              try
                let {Apply.funsym= g; Apply.arg= y'} = apply v in
                assert (V.equal y y') ;
                if Funsym.equal f g then (
                  V.union v u ;
                  Config.Find.remove v ;
                  (* remove [v |-> g(y')]. *)
                  vs' := Varset.rem v !vs' )
              with Not_found -> () )
            vs
        with Not_found -> () )
      us ;
    assert (Varset.disjoint us !vs') ;
    Config.Dep.remove x ;
    if Varset.is_empty us && Varset.is_empty !vs' then Config.Dep.remove y
    else Config.Dep.set y (Varset.union us !vs') ;
    assert (not (Config.Dep.mem x)) ;
    assert (well_formed ())

  (** [u = f(x), v = f(y), u <> v ==> x <> y] *)
  let diseq x y =
    let us = dep x and vs = dep y in
    Varset.exists
      (fun u ->
        assert (dom u) ;
        let {Apply.funsym= f; Apply.arg= x'} = apply u in
        assert (V.equal x x') ;
        Varset.exists
          (fun v ->
            assert (dom v) ;
            V.diseq u v
            &&
            let {Apply.funsym= f'; Apply.arg= y'} = apply v in
            assert (V.equal y y') ;
            Funsym.equal f f' )
          vs )
      us
end
