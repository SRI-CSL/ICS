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

module type POLYNOMIAL = Polynomial.P

module type INTERFACE = sig
  type var

  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val is_real : var -> bool
  val is_integer : var -> bool
  val union : var -> var -> unit
  val separate : var -> var -> unit
  val real : var -> unit
  val integer : var -> unit
end

module type INFSYS = sig
  type var
  type coeff
  type poly
  type t

  val empty : t

  module S : sig
    module Slacks : Sets.S with type elt = var

    type t

    val current : unit -> Slacks.t
    val mem : var -> bool
  end

  module R : sig
    type t

    val empty : t

    module Deps : Powermaps.S with type key = var
    module Constant : Maps.S with type key = var and type value = coeff
    module Solset : Maps.S with type key = var and type value = poly

    val constant : unit -> Constant.t
    val solset : unit -> Solset.t
    val dep : unit -> Deps.t
    val pp : Format.formatter -> unit
    val find : var -> poly
    val find_const : var -> coeff
    val inv : poly -> var
    val deps : var -> Deps.Values.t
    val dom : var -> bool
    val cod : var -> bool
  end

  module T : sig
    type t

    val empty : t

    module Deps : Powermaps.S with type key = var
    module Solset : Maps.S with type key = var and type value = poly

    val solset : unit -> Solset.t
    val dep : unit -> Deps.t
    val pp : Format.formatter -> unit
    val find : var -> poly
    val inv : poly -> var
    val deps : var -> Deps.Values.t
    val dom : var -> bool
    val cod : var -> bool
  end

  val current : unit -> t
  val is_empty : unit -> bool
  val find : var -> poly
  val find_const : var -> coeff
  val inv : poly -> var
  val inv_const : coeff -> var
  val feasible : unit -> bool
  val synchronized : unit -> bool
  val pp : Format.formatter -> unit
  val can : poly -> poly
  val max : poly -> poly
  val min : poly -> poly
  val restricted : poly -> bool
  val minimized : poly -> bool
  val maximized : poly -> bool
  val complete : bool ref
  val is_nonneg : poly -> bool
  val is_pos : poly -> bool
  val is_diseq0 : poly -> bool
  val is_equal0 : poly -> bool
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val alias : poly -> var
  val alias_const : coeff -> var

  exception Unsat

  val process_eq0 : poly -> unit
  val process_nonneg : poly -> unit
  val process_pos : poly -> unit
  val process_deq0 : poly -> unit
  val propagate_eq : var -> var -> unit
  val gc_slacks : bool ref
  val normalize : unit -> unit
end

module Make (P : POLYNOMIAL) (V : INTERFACE with type var = P.indet) =
struct
  module C = P.Coeff
  module Var = P.Indet

  exception Unsat

  let var_iter f =
    let f_c _ = () and f_m x _ = f x in
    P.iter f_c f_m

  let posvar_iter f =
    let f_c _ = () and f_m x c = if C.compare c C.zero > 0 then f x in
    P.iter f_c f_m

  let var_for_all p =
    let p_c _ = true and p_m x _ = p x in
    P.for_all p_c p_m

  let var_choose f p = fst (P.choose f p)

  let choose_var =
    let triv _ _ = true in
    var_choose triv

  let choose_posvar =
    let pos_monomial _ c = C.compare c C.zero > 0 in
    var_choose pos_monomial

  type poly = P.t
  type var = Var.t
  type coeff = C.t

  module Trace = struct
    let debug = ref true

    open Format

    let varpp = Var.pp err_formatter

    let rec varlpp = function
      | [] -> ()
      | [x] -> varpp x
      | x :: xl ->
          varpp x ;
          eprintf ", " ;
          varlpp xl

    let termpp = P.pp err_formatter
    let constpp = P.Coeff.pp err_formatter

    let slack x =
      if !debug then (
        eprintf "\nSlack(A): " ;
        varpp x ;
        eprintf "@?" ) ;
      true

    let union x y =
      if !debug then (
        eprintf "\nDeduce(A): " ;
        varpp x ;
        eprintf " = " ;
        varpp y ;
        eprintf "@?" ) ;
      true

    let separate x y =
      if !debug then (
        eprintf "\nDeduce(A): " ;
        varpp x ;
        eprintf " <> " ;
        varpp y ;
        eprintf "@?" ) ;
      true

    let real x =
      if !debug then (
        eprintf "\nDeduce(A): real(" ;
        varpp x ;
        eprintf ")@?" ) ;
      true

    let extend x p =
      if !debug then (
        eprintf "\nExtend(A): " ;
        varpp x ;
        eprintf " = " ;
        termpp p ;
        eprintf "@?" ) ;
      true

    let compose x p =
      if !debug then (
        eprintf "\nCompose(A): " ;
        varpp x ;
        eprintf " = " ;
        termpp p ;
        eprintf "@?" ) ;
      true

    let pivot x y =
      if !debug then (
        eprintf "\nPivot(A): " ;
        varpp x ;
        eprintf " " ;
        varpp y ;
        eprintf "@?" ) ;
      true

    let fuse_const x c =
      if !debug then (
        eprintf "\nFuse(A): " ;
        varpp x ;
        eprintf " = " ;
        constpp c ;
        eprintf "@?" ) ;
      true

    let fuse_var x y =
      if !debug then (
        eprintf "\nFuse(A): " ;
        varpp x ;
        eprintf " = " ;
        varpp y ;
        eprintf "@?" ) ;
      true

    let inc_bounded xl =
      if !debug then (
        eprintf "\nIncBounded(A): " ;
        varlpp xl ;
        eprintf "@?" ) ;
      true

    let inc_zeroes x p =
      if !debug then (
        eprintf "\nIncZeroes(A): " ;
        varpp x ;
        eprintf " = " ;
        termpp p ;
        eprintf "@?" ) ;
      true

    let set0 x =
      if !debug then (
        eprintf "\nSet0(A): " ;
        varpp x ;
        eprintf "@?" ) ;
      true

    let zbnd_star () =
      if !debug then eprintf "\nzbnd(A) @?" ;
      true

    let max0 p =
      if !debug then (
        eprintf "\nMax0(A): " ;
        termpp p ;
        eprintf "@?" ) ;
      true

    let find_zeroes () =
      if !debug then eprintf "\nfindZeroes(A) @?" ;
      true
  end

  module Term = struct
    type t = P.t

    let equal = P.equal
    let compare = Stdlib.compare
    let hash = P.hash
    let pp = P.pp
    let of_var = P.indet

    let iter f =
      let f_c _ = () and f_m x _ = f x in
      P.iter f_c f_m

    let map = P.map
  end

  module Subst = Maps.Make (Var) (Term)
  module Dep = Powermaps.Make (Var)

  module S = struct
    module Slacks = Sets.Make (Var)
    module Config = Config.Set (Slacks)

    type t = Slacks.t

    let empty = Slacks.empty
    let initialize = Config.initialize
    let current = Config.current
    let mem = Config.mem
    let add = Config.add
    let remove = Config.remove
    let pp fmt = Slacks.pp fmt (current ())
    let iter f = Slacks.iter f (current ())
  end

  let is_slack x = S.mem x

  let fresh_slack () =
    let k = Var.fresh () in
    assert (Trace.slack k) ;
    S.add k ;
    k

  let slacks = S.current

  let extern p =
    let extern_m x _ = not (is_slack x) in
    P.Map.for_all extern_m p.P.monomials

  let restricted p =
    let restr_m x _ = is_slack x in
    P.Map.for_all restr_m p.P.monomials

  let minimized p =
    let min_m x c = is_slack x && C.compare c C.zero > 0 in
    P.Map.for_all min_m p.P.monomials

  let maximized p =
    let max_m x c = is_slack x && C.compare c C.zero < 0 in
    P.Map.for_all max_m p.P.monomials

  let maximized_at_zero p = C.equal (P.const p) C.zero && maximized p
  let maximized_neg p = C.compare (P.const p) C.zero < 0 && maximized p
  let maximized_nonpos p = C.compare (P.const p) C.zero <= 0 && maximized p
  let minimized_nonneg p = C.compare (P.const p) C.zero >= 0 && minimized p
  let minimized_pos p = C.compare (P.const p) C.zero > 0 && minimized p

  module Deduce = struct
    let union x y =
      assert (not (is_slack x)) ;
      assert (not (is_slack y)) ;
      assert (Trace.union x y) ;
      V.union x y

    let separate x y =
      assert (not (is_slack x)) ;
      assert (not (is_slack y)) ;
      assert (Trace.separate x y) ;
      V.separate x y

    let real1 x =
      assert (not (is_slack x)) ;
      assert (Trace.real x) ;
      V.real x

    let real p =
      let add_real x = if not (is_slack x) then real1 (V.find x) in
      var_iter add_real p
  end

  (** Solution set with bindings [x |-> p] where [x] is an unrestricted
      variable and [p] is a polynomial with the additional restriction that
      [p] is not an unrestricted variable. *)
  module R = struct
    module Deps = Powermaps.Make (Var)
    module Subst0 = Maps.Make (Var) (C)
    module Constant = Subst0
    module Solset = Subst
    module Inv0 = Maps.Make (C) (Var)

    type t =
      { subst0: Subst0.t  (** [x |-> c] with [c] a constant *)
      ; inv0: Inv0.t
      ; subst: Subst.t  (** [x |-> p] with [p] a nonconstant polynomial *)
      ; dep: Dep.t }

    let empty =
      { subst0= Subst0.empty ()
      ; inv0= Inv0.empty ()
      ; subst= Subst.empty ()
      ; dep= Dep.empty () }

    let init = ref empty

    module Config = struct
      module Subst0 = Config.Map (Subst0)
      module Inv0 = Config.Map (Inv0)
      module Subst = Config.Map (Subst)
      module Dep = Config.Powermap (Dep)
    end

    let constant = Config.Subst0.current
    let solset = Config.Subst.current
    let dep = Config.Dep.current

    let initialize s =
      init := s ;
      Config.Subst0.initialize s.subst0 ;
      Config.Inv0.initialize s.inv0 ;
      Config.Subst.initialize s.subst ;
      Config.Dep.initialize s.dep

    let unchanged = Config.Subst.unchanged

    let current () =
      if unchanged () then !init
      else
        { subst0= Config.Subst0.current ()
        ; inv0= Config.Inv0.current ()
        ; subst= Config.Subst.current ()
        ; dep= Config.Dep.current () }

    let pp fmt =
      Subst0.pp fmt (Config.Subst0.current ()) ;
      Subst.pp fmt (Config.Subst.current ())

    let deps x = Config.Dep.find x
    let find_const x = Subst0.find x (Config.Subst0.current ())
    let find_nonconst x = Subst.find x (Config.Subst.current ())

    let find_slack x =
      let p = find_nonconst x in
      try P.d_indet p with P.Nonindet -> raise Not_found

    let find x =
      try find_nonconst x with Not_found -> P.constant (find_const x)

    let dom x = Subst.mem x (Config.Subst.current ())
    let cod y = Config.Dep.mem y
    let occ x = dom x || cod x

    let canonical p =
      let can_m x _ = not (dom x) in
      P.Map.for_all can_m p.P.monomials

    let wf_binding x p =
      (not (is_slack x))
      && (if P.is_indet p then is_slack (P.d_indet p) else true)
      && not (P.mem x p)

    let well_formed () =
      let debug = ref true in
      let check name pred =
        if not !debug then pred ()
        else
          pred ()
          ||
          ( Format.eprintf
              "\n\
               ERROR: well_formedness check %s for configuration R failed. \n\
              \                           Current state: \n"
              name ;
            pp Format.err_formatter ;
            Dep.pp Format.err_formatter (Config.Dep.current ()) ;
            Format.eprintf "\n@?" ;
            false )
      in
      let for_all0 p = Subst0.for_all p (Config.Subst0.current ()) in
      let for_all p = Subst.for_all p (Config.Subst.current ()) in
      let for_all_dep p = Dep.for_all p (Config.Dep.current ()) in
      let disjoint () =
        for_all (fun x _ -> for_all0 (fun y _ -> not (Var.equal x y)))
      and codomain () =
        for_all (fun x p ->
            (not (is_slack x))
            && (not (P.is_constant p))
            && try is_slack (P.d_indet p) with P.Nonindet -> canonical p )
      and deps1 () =
        for_all_dep (fun y xs ->
            Dep.Values.for_all
              (fun x -> dom x && P.mem y (find_nonconst x))
              xs )
      and deps2 () =
        for_all (fun x p ->
            let check_m y _ = Dep.Values.mem x (deps y) in
            P.Map.for_all check_m p.P.monomials )
      and injective_subst0 () =
        for_all0 (fun x c ->
            for_all0 (fun y d ->
                if C.equal c d then Var.equal x y else true ) )
      and injective_subst () =
        for_all (fun x p ->
            for_all (fun y q -> if P.equal p q then Var.equal x y else true) )
      in
      check "disjoint" disjoint
      && check "codomain" codomain
      && check "deps1" deps1
      && check "deps2" deps2
      && check "injective0" injective_subst0
      && check "injective" injective_subst

    let is_empty () =
      Subst.is_empty (Config.Subst.current ())
      && Subst0.is_empty (Config.Subst0.current ())

    let inv_const c = Inv0.find c (Config.Inv0.current ())

    exception Found

    let inv_slack =
      let found : Var.t ref = ref (Obj.magic 0) in
      fun v ->
        assert (is_slack v) ;
        let test x =
          assert (dom x) ;
          if Var.equal v (find_slack x) then (
            found := x ;
            raise Found )
        in
        try
          Dep.Values.iter test (deps v) ;
          raise Not_found
        with Found -> !found

    let inv_nonconst =
      let found : Var.t ref = ref (Obj.magic 0) in
      fun p ->
        assert (not (P.is_constant p)) ;
        let dx = deps (choose_var p) in
        let test x =
          assert (dom x) ;
          if P.equal p (find x) then (
            found := x ;
            raise Found )
        in
        try
          Dep.Values.iter test dx ;
          raise Not_found
        with Found -> !found

    let inv p =
      try inv_const (P.d_constant p) with P.Nonnum -> inv_nonconst p

    let rec update x p q =
      assert (dom x) ;
      assert (P.equal (find_nonconst x) p) ;
      try
        let y = inv q in
        Config.Subst.remove x ;
        var_iter (Config.Dep.rem x) p ;
        Deduce.union x y ;
        assert (well_formed ())
      with Not_found -> (
        try update_const x p (P.d_constant q)
        with P.Nonnum -> (
          try update_var x p (P.d_indet q)
          with P.Nonindet ->
            if not (wf_binding x q) then (
              Format.eprintf "\nERROR : " ;
              Var.pp Format.err_formatter x ;
              Format.eprintf " = " ;
              P.pp Format.err_formatter q ;
              Format.eprintf "@?" ) ;
            assert (wf_binding x q) ;
            Config.Subst.set x q ;
            let remdepx z = if not (P.mem z q) then Config.Dep.rem x z in
            let adddepx = Config.Dep.add x in
            var_iter remdepx p ;
            var_iter adddepx q ;
            assert (well_formed ()) ) )

    and update_const x p c =
      let d = P.const p in
      (* 1. [c = p <= d], 2. [c = p >= d]. *)
      if maximized p && C.compare c d > 0 then raise Unsat ;
      if minimized p && C.compare c d < 0 then raise Unsat ;
      Config.Subst0.set x c ;
      Config.Inv0.set c x ;
      Config.Subst.remove x ;
      var_iter (Config.Dep.rem x) p ;
      assert (well_formed ())

    and update_var x p y =
      if is_slack y then (
        assert (wf_binding x (P.indet y)) ;
        Config.Subst.set x (P.indet y) ;
        var_iter (Config.Dep.rem x) p ;
        Config.Dep.add x y ;
        assert (well_formed ()) )
      else (
        Config.Subst.remove x ;
        var_iter (Config.Dep.rem x) p ;
        Deduce.union x y ;
        assert (well_formed ()) )

    let rec fuse y t =
      assert (not (dom y)) ;
      assert (not (P.mem y t)) ;
      assert (canonical t) ;
      try fuse_const y (P.d_constant t)
      with P.Nonnum -> (
        try fuse_var y (P.d_indet t)
        with P.Nonindet ->
          let fuse1 x =
            try
              let s = find x in
              (* assert(P.mem y s); *)
              let s' = P.replace y t s in
              update x s s'
            with Not_found -> ()
          in
          Dep.Values.iter fuse1 (deps y) ;
          assert (not (cod y)) )

    and fuse_const y c =
      assert (Trace.fuse_const y c) ;
      assert (not (dom y)) ;
      let fuse1 x =
        (* assert(dom x); *)
        try
          let p = find_nonconst x in
          let q = P.instantiate y c p in
          (* assert(P.mem y p); *)
          assert (not (P.mem y q)) ;
          update x p q
        with Not_found -> ()
      in
      Dep.Values.iter fuse1 (deps y) ;
      assert (not (cod y))

    and fuse_var y z =
      assert (Trace.fuse_var y z) ;
      assert (not (dom y)) ;
      assert (not (Var.equal y z)) ;
      let fuse1 x =
        assert (dom x) ;
        try
          let p = find x in
          assert (P.mem y p) ;
          let q = P.rename y z p in
          assert (not (P.mem y q)) ;
          update x p q
        with Not_found -> ()
      in
      Dep.Values.iter fuse1 (deps y) ;
      assert (not (cod y))

    let rec extend x p =
      assert (not (is_slack x)) ;
      assert (Trace.extend x p) ;
      assert (not (dom x)) ;
      try extend_const x (P.d_constant p)
      with P.Nonnum -> extend_nonconst x p

    and extend_const x c =
      assert (not (dom x)) ;
      try
        let y = inv_const c in
        Deduce.union x y
      with Not_found ->
        Config.Subst0.set x c ;
        Config.Inv0.set c x ;
        assert (well_formed ())

    and extend_var x y =
      assert (not (is_slack x)) ;
      if is_slack y then (
        let p = P.indet y in
        assert (wf_binding x p) ;
        Config.Subst.set x p ;
        Config.Dep.add x y )
      else Deduce.union x y

    and extend_nonconst x p =
      assert (not (is_slack x)) ;
      assert (not (dom x)) ;
      assert (not (P.is_constant p)) ;
      try extend_var x (P.d_indet p)
      with P.Nonindet -> (
        try Deduce.union x (inv_nonconst p)
        with Not_found ->
          assert (wf_binding x p) ;
          Config.Subst.set x p ;
          var_iter (Config.Dep.add x) p ;
          assert (well_formed ()) )

    let rec compose x p =
      assert (Trace.compose x p) ;
      assert (not (dom x)) ;
      try compose_const x (P.d_constant p)
      with P.Nonnum -> compose_nonconst x p

    and compose_const x c =
      assert (not (dom x)) ;
      fuse_const x c ;
      extend_const x c

    and compose_var x y =
      assert (not (dom x)) ;
      if is_slack y then (
        try
          let z = inv_slack y in
          fuse_var x y ;
          Deduce.union x z
        with Not_found ->
          fuse_var x y ;
          assert (wf_binding x (P.indet y)) ;
          Config.Subst.set x (P.indet y) ;
          Config.Dep.add x y ;
          assert (well_formed ()) )
      else (
        fuse_var x y ;
        Deduce.union x y )

    and compose_nonconst x p =
      assert (not (dom x)) ;
      assert (not (P.is_constant p)) ;
      try compose_var x (P.d_indet p)
      with P.Nonindet -> (
        try
          let z = inv_nonconst p in
          fuse x p ;
          Deduce.union x z
        with Not_found ->
          fuse x p ;
          extend_nonconst x p )

    let restrict x =
      assert (well_formed ()) ;
      try
        let c = find_const x in
        Config.Subst0.remove x ;
        Config.Inv0.remove c ;
        assert (well_formed ())
      with Not_found -> (
        try
          let p = find_nonconst x in
          Config.Subst.remove x ;
          var_iter (Config.Dep.rem x) p ;
          assert (well_formed ())
        with Not_found -> () )
  end

  module T = struct
    module Deps = Powermaps.Make (Var)

    type t = {subst: Subst.t; dep: Dep.t}

    let empty = {subst= Subst.empty (); dep= Dep.empty ()}
    let init = ref empty

    module Config = struct
      module Subst = Config.Map (Subst)
      module Dep = Config.Powermap (Dep)
    end

    module Solset = Subst

    let solset = Config.Subst.current
    let dep = Config.Dep.current
    let pp fmt = Subst.pp fmt (solset ())
    let find x = Subst.find x (solset ())
    let deps x = Dep.find x (dep ())
    let dom x = Subst.mem x (solset ())
    let cod y = Dep.mem y (dep ())
    let occ x = dom x || cod x
    let is_empty () = Subst.is_empty (solset ())
    let iter f = Subst.iter f (solset ())

    let iter0 f =
      let f0 u p = if C.equal (P.const p) C.zero then f u p in
      Subst.iter f0 (solset ())

    let for_all p = Subst.for_all p (solset ())

    let wf_binding x p =
      is_slack x
      && restricted p
      && (not (P.is_indet p))
      && (not (P.is_constant p))
      && C.compare (P.const p) C.zero >= 0
      && not (P.mem x p)

    let well_formed () =
      let debug = ref true in
      let check name pred =
        if not !debug then pred ()
        else
          pred ()
          ||
          ( Format.eprintf
              "\n\
               ERROR: well_formedness check %s for configuration T failed. \n\
              \              Current state: \n"
              name ;
            Subst.pp Format.err_formatter (Config.Subst.current ()) ;
            Format.eprintf "\n with dependencies: \n" ;
            Dep.pp Format.err_formatter (Config.Dep.current ()) ;
            Format.eprintf "\n@?" ;
            false )
      in
      let for_all_find p = Subst.for_all p (Config.Subst.current ()) in
      let for_all_dep p =
        Dep.for_all
          (fun u -> Dep.Values.for_all (p u))
          (Config.Dep.current ())
      in
      let solved () =
        for_all_find (fun u p ->
            wf_binding u p && var_for_all (fun v -> not (dom v)) p )
      in
      let deps_ok () =
        for_all_dep (fun v u ->
            is_slack v && is_slack u && dom u && P.mem v (find u) )
      in
      check "solved" solved && check "deps" deps_ok

    let initialize s =
      init := s ;
      Config.Subst.initialize s.subst ;
      Config.Dep.initialize s.dep

    let unchanged = Config.Subst.unchanged

    let current () =
      if unchanged () then !init else {subst= solset (); dep= dep ()}

    let canonical p =
      let dom_m x _ = not (dom x) in
      P.Map.for_all dom_m p.P.monomials

    let can p =
      let q = P.map find p in
      assert (canonical q) ;
      q

    exception Found

    let inv =
      let found : Var.t ref = ref (Obj.magic 0) in
      fun p ->
        let us = deps (choose_var p) in
        let test_inv u =
          assert (dom u) ;
          if P.equal p (find u) then (
            found := u ;
            raise Found )
        in
        try
          Dep.Values.iter test_inv us ;
          raise Not_found
        with Found -> !found

    let restrict x p =
      assert (dom x) ;
      assert (P.equal (find x) p) ;
      Config.Subst.remove x ;
      var_iter (Config.Dep.rem x) p ;
      assert (well_formed ())

    let rec fuse y t =
      assert (not (dom y)) ;
      assert (not (P.mem y t)) ;
      assert (not (P.is_constant t)) ;
      assert (not (P.is_indet t)) ;
      let fuse1 x =
        try
          let s = find x in
          (* assert(P.mem y s); *)
          (* ?? *)
          let s' = P.replace y t s in
          assert (not (P.mem y s')) ;
          update x s s'
        with Not_found ->
          (* Might occur because of calls to [restrict] in this loop. *)
          ()
      in
      Dep.Values.iter fuse1 (deps y) ;
      assert (not (cod y))

    and fuse_var y z =
      assert (not (dom z)) ;
      let fuse1 u =
        try
          let p = find u in
          let q = P.rename y z p in
          assert (P.mem y p) ;
          (* ?? *)
          assert (not (P.mem y q)) ;
          update u p q
        with Not_found -> ()
      in
      Dep.Values.iter fuse1 (deps y) ;
      assert (not (cod y))

    and fuse_const y c =
      let fuse1 u =
        try
          let p = find u in
          let q = P.instantiate y c p in
          assert (P.mem y p) ;
          assert (not (P.mem y q)) ;
          update u p q
        with Not_found -> ()
      in
      Dep.Values.iter fuse1 (deps y) ;
      assert (not (cod y))

    and update x p q =
      assert (dom x) ;
      assert (P.equal (find x) p) ;
      try update_const x p (P.d_constant q)
      with P.Nonnum -> (
        try update_var x p (P.d_indet q)
        with P.Nonindet -> (
          assert (wf_binding x q) ;
          try
            let y = inv q in
            restrict y q ;
            R.fuse_var y x ;
            assert ((not (occ y)) && not (R.cod y)) ;
            assert (R.well_formed ()) ;
            Config.Subst.set x q ;
            var_iter (fun y -> if not (P.mem y q) then Config.Dep.rem x y) p ;
            var_iter (fun y -> Config.Dep.add x y) q ;
            assert (well_formed ())
          with Not_found ->
            if maximized_at_zero q then (
              (* derive equalities. *)
              restrict x p ;
              derive_zeros x q )
            else (
              Config.Subst.set x q ;
              var_iter
                (fun y -> if not (P.mem y q) then Config.Dep.rem x y)
                p ;
              var_iter (fun y -> Config.Dep.add x y) q ;
              assert (well_formed ()) ) ) )

    and update_const x p c =
      assert (dom x) ;
      assert (P.equal (find x) p) ;
      if C.compare c C.zero < 0 then raise Unsat
      else (
        restrict x p ;
        R.fuse_const x c ;
        assert (R.well_formed ()) )

    and update_var x p y =
      assert (dom x) ;
      assert (P.equal (find x) p) ;
      restrict x p ;
      R.fuse_var x y ;
      assert (R.well_formed ())

    and derive_zeros x q =
      assert (is_slack x) ;
      assert (maximized_at_zero q) ;
      R.fuse_const x C.zero ;
      let fuse0 y =
        R.fuse_const y C.zero ;
        fuse_const y C.zero
      in
      var_iter fuse0 q

    let extend x p =
      assert (not (dom x)) ;
      assert (wf_binding x p) ;
      if maximized_at_zero p then derive_zeros x p
      else
        try
          let y = inv p in
          restrict y p ;
          R.fuse_var y x ;
          assert ((not (occ y)) && not (R.cod y)) ;
          Config.Subst.set x p ;
          var_iter (Config.Dep.add x) p ;
          assert (well_formed ())
        with Not_found ->
          Config.Subst.set x p ;
          var_iter (Config.Dep.add x) p ;
          assert (well_formed ())

    let rec compose x p =
      assert (Trace.compose x p) ;
      assert (not (dom x)) ;
      assert (not (P.mem x p)) ;
      assert (restricted p) ;
      try compose_const x (P.d_constant p)
      with P.Nonnum -> (
        try compose_var x (P.d_indet p)
        with P.Nonindet ->
          if maximized_at_zero p then compose_max_at0 x p
          else (
            fuse x p ;
            extend x p ) )

    and compose_const x c =
      assert (not (dom x)) ;
      if C.compare c C.zero < 0 then raise Unsat
      else (
        R.fuse_const x c ;
        fuse_const x c ;
        assert (not (occ x)) ;
        assert (R.well_formed ()) )

    and compose_var x y =
      assert (not (dom x)) ;
      R.fuse_var x y ;
      fuse_var x y ;
      assert (not (occ x)) ;
      assert (R.well_formed ())

    and compose_max_at0 x p =
      assert (not (dom x)) ;
      assert (not (P.mem x p)) ;
      assert (restricted p) ;
      assert (C.equal (P.const p) C.zero && maximized p) ;
      R.fuse_const x C.zero ;
      let fuse0 y =
        R.fuse_const y C.zero ;
        fuse_const y C.zero
      in
      var_iter fuse0 p ;
      assert (R.well_formed ())

    let pivot u v =
      assert (Trace.pivot u v) ;
      assert (dom u) ;
      assert (P.mem v (find u)) ;
      assert (not (dom v)) ;
      let p = find u in
      assert (P.mem v p) ;
      let q = P.pivot u v p in
      assert (not (P.mem v q)) ;
      restrict u p ;
      compose v q

    let normalize () =
      let rem u = if (not (R.cod u)) && not (occ u) then S.remove u in
      S.iter rem
  end

  type t = {regular: R.t; tableau: T.t; slacks: S.t}

  let empty = {regular= R.empty; tableau= T.empty; slacks= S.empty ()}

  let pp fmt =
    Format.fprintf fmt "@[" ;
    Format.fprintf fmt "r: " ;
    R.pp fmt ;
    Format.fprintf fmt "\nt: " ;
    T.pp fmt ;
    Format.fprintf fmt "\nslacks: " ;
    S.pp fmt ;
    Format.fprintf fmt "@]@?"

  let init = ref empty

  let initialize s =
    init := s ;
    R.initialize s.regular ;
    T.initialize s.tableau ;
    S.initialize s.slacks

  let reset () = initialize empty
  let unchanged () = R.unchanged () && T.unchanged ()
  let dom x = R.dom x || T.dom x
  let cod x = R.cod x || T.cod x
  let occ x = dom x || cod x

  let synchronized () =
    let canonized x p = V.canonical x && var_for_all V.canonical p in
    true
    || (* disable for now *)
    Subst.for_all canonized (R.Config.Subst.current ())

  let feasible () =
    let nonneg_const _ p = C.compare (P.const p) C.zero >= 0 in
    T.for_all nonneg_const

  let combined_solset () = T.for_all (fun u _ -> not (R.cod u))

  (** Plug domain variables in [T] into codomain of [R] to ensure that the
      combined configuration [R; T] is a solution set. *)
  let ensure_combined_solset =
    let module Focus = struct
      let curr = Stacks.create ()
      let mem x = Stacks.mem Var.equal x curr
      let reset () = Stacks.clear curr
      let add x = if not (mem x) then Stacks.push x curr
      let is_empty () = Stacks.is_empty curr
      let next () = Stacks.pop curr
    end in
    fun () ->
      assert (synchronized ()) ;
      Focus.reset () ;
      T.iter (fun u _ -> Dep.Values.iter Focus.add (R.deps u)) ;
      while not (Focus.is_empty ()) do
        let x = Focus.next () in
        try
          let p = R.find x in
          let p' = P.map T.find p in
          R.update x p p'
        with Not_found -> ()
      done ;
      assert (combined_solset ())

  let current () =
    if unchanged () then !init
    else {regular= R.current (); tableau= T.current (); slacks= S.current ()}

  let is_empty () = R.is_empty () && T.is_empty ()

  exception Restricted

  let choose_unrestricted p =
    let unrestricted x _ = not (is_slack x) in
    try var_choose unrestricted p with Not_found -> raise Restricted

  let find_const = R.find_const
  let inv_const = R.inv_const
  let find x = try R.find x with Not_found -> T.find x
  let inv p = try R.inv p with Not_found -> T.inv p

  let canonical p =
    let can_m x _ = (not (dom x)) && V.canonical x in
    P.Map.for_all can_m p.P.monomials

  let can p =
    assert (synchronized ()) ;
    let q = P.map find p in
    (* assert(canonical q); *)
    q

  let all _ = true

  (** [v] is {i unbounded} in [T] iff [K(T(u), v) > 0] for all [u] in [T]. *)
  let unbounded t v =
    T.cod v
    && Dep.Values.for_all
         (fun u ->
           assert (T.dom u) ;
           t u && C.compare (P.coeff v (T.find u)) C.zero > 0 )
         (T.deps v)

  (** [v] is {i bounded} in [T] iff [K(T(u), v) < 0] for some [u] in [T]. *)
  let bounded t v =
    Dep.Values.exists
      (fun u ->
        assert (T.dom u) ;
        t u && C.compare (P.coeff v (T.find u)) C.zero < 0 )
      (T.deps v)

  let unbounded_poly t p =
    let unb x c = C.compare c C.zero > 0 && unbounded t x in
    P.Map.exists unb p.P.monomials

  let choose_pos_unbounded t =
    let pos_unbounded x c = C.compare c C.zero > 0 && unbounded t x in
    var_choose pos_unbounded

  let pivot_candidate t u v =
    t u && T.dom u && C.compare (P.coeff v (T.find u)) C.zero < 0

  let gain t u v =
    assert (pivot_candidate t u v) ;
    let p = T.find u in
    C.mult (C.minus (P.const p)) (C.inv (P.coeff v p))

  let pivotable t u v =
    pivot_candidate t u v
    &&
    let g = gain t u v in
    let smallest_gain u' =
      if pivot_candidate t u' v then g <= gain t u' v else true
    in
    Dep.Values.for_all smallest_gain (T.deps v)

  (** Given a bounded [v] search for [u] such that [pivotable u v] according
      to Bland's rule. *)
  let bland_pivot =
    let arbg = C.minus C.one and arbu = Obj.magic 0 in
    let ming = ref arbg and minu = ref arbu in
    fun t v ->
      assert (bounded t v) ;
      let us = T.deps v in
      assert (not (Dep.Values.is_empty us)) ;
      let pivot u =
        assert (dom u) ;
        if not (t u) then ()
        else
          let p = find u in
          let c = P.const p in
          let d = P.coeff v p in
          (* (u = c + d * v + p'] with [d < 0]. *)
          assert (C.compare c C.zero >= 0) ;
          if C.compare d C.zero < 0 then (
            let g = C.mult (C.minus c) (C.inv d) in
            assert (C.compare g C.zero >= 0) ;
            if C.compare !ming C.zero < 0 then (
              (* undefined *)
              ming := g ;
              minu := u )
            else
              let cmp = C.compare !ming g in
              if cmp > 0 || (cmp = 0 && Var.compare !minu u > 0) then (
                ming := g ;
                minu := u ) )
      in
      ming := arbg ;
      minu := arbu ;
      Dep.Values.iter pivot us ;
      assert (C.compare !ming C.zero >= 0) ;
      !minu

  exception Unbounded

  let bland t p =
    assert (restricted p) ;
    let pos_bounded x c = C.compare c C.zero > 0 && bounded t x in
    try
      let v = var_choose pos_bounded p in
      let u = bland_pivot t v in
      assert (pivotable t u v) ;
      (u, v)
    with Not_found ->
      (* assert(unboundedPoly t p || maximized p); *)
      raise Unbounded

  let alias p =
    assert (synchronized ()) ;
    let p = can p in
    try V.find (inv p)
    with Not_found ->
      let v = Var.fresh () in
      Deduce.real1 v ;
      R.extend v p ;
      v

  let alias_const c =
    try V.find (R.inv_const c)
    with Not_found ->
      let v = Var.fresh () in
      Deduce.real1 v ;
      R.extend_const v c ;
      v

  let alias_slack w =
    assert (is_slack w) ;
    try V.find (R.inv_slack w)
    with Not_found ->
      let x = Var.fresh () in
      R.extend_var x w ;
      x

  let rec max p =
    assert (restricted p) ;
    assert (synchronized ()) ;
    let p = T.can p in
    try
      let u, v = bland all p in
      T.pivot u v ;
      max p
    with Unbounded -> (* assert(unboundedPoly all p || maximized p); *)
                      p

  let min p =
    assert (synchronized ()) ;
    P.minus (max (P.minus p))

  let is_equal0 p = P.equal (can p) P.zero
  let complete = ref true

  let is_nonneg p =
    let p = can p in
    minimized_nonneg p || (restricted p && minimized_nonneg (min p))

  let is_diseq0_var p =
    (* try
     *   let x = P.d_indet p in
     *   let z = R.inv_const C.zero in
     *   (* V.Varset.mem x (V.deqs z) *)
     *   false
     * with _ -> *)
    false

  let is_diseq0 p =
    let p = can p in
    is_diseq0_var p
    || minimized_pos p
    || maximized_neg p
    || !complete
       && restricted p
       && (minimized_pos (min p) || maximized_neg (max p))

  let is_pos p = is_nonneg p && is_diseq0 p

  (** Make implicit variable equalities explicit. *)
  module Derive0 = struct
    (** Focus contains a subset of the current domain variables of [T]. *)
    module Focus = struct
      module Vars = Hashtbl.Make (Var)

      let focus = Vars.create 7
      let is_empty () = Vars.length focus = 0

      let clear () =
        if not (is_empty ()) then (
          Vars.clear focus ;
          assert (is_empty ()) )

      let mem u = Vars.mem focus u
      let add u = if not (mem u) then Vars.add focus u true
      let rem u = Vars.remove focus u
      let to_list () = Vars.fold (fun x _ acc -> x :: acc) focus []

      let iter f =
        let f' x _ = f x in
        Vars.iter f' focus

      exception Found of Var.t

      let choose () =
        let get x _ = raise (Found x) in
        try
          Vars.iter get focus ;
          raise Not_found
        with Found x -> x
    end

    let init_diff w ws =
      Focus.clear () ;
      Focus.add w ;
      Dep.Values.iter Focus.add ws

    let filter_nonzero () =
      let rem_nonzero u =
        try
          let p = T.find u in
          if C.compare (P.const p) C.zero <> 0 then Focus.rem u
        with Not_found ->
          (* Entry has been deleted by [T.compose]. *)
          Focus.rem u
      in
      Focus.iter rem_nonzero

    (** Compute all entries [u = p] that contain positive occurrences of a
        variable [v] in [p] that occurs negatively in an entry that might be
        maximized at [0]. *)
    let rec inc_bounded () =
      assert (Trace.inc_bounded (Focus.to_list ())) ;
      let changed = ref false in
      let rec inspect_entry0 u p =
        assert (C.equal (P.const p) C.zero) ;
        posvar_iter inspect_posvar p
      and inspect_posvar v =
        if (not (Focus.mem v)) && occurs_neg v then (
          changed := true ;
          Focus.add v )
      and occurs_neg v =
        let test u =
          Focus.mem u
          &&
          try
            let p = T.find u in
            let c = P.coeff v p in
            C.compare c C.zero < 0
          with Not_found -> false
        in
        Dep.Values.exists test (T.deps v)
      in
      T.iter0 inspect_entry0 ;
      if !changed then inc_bounded ()
      else assert (Trace.inc_bounded (Focus.to_list ()))

    let rec max0 p =
      assert (Trace.max0 p) ;
      assert (restricted p) ;
      let p = T.can p in
      if C.compare (P.const p) C.zero > 0 then p
      else
        try
          let u, v = bland Focus.mem p in
          Focus.add v ;
          (* ensure only domain variables are in [focus]. *)
          Focus.rem u ;
          T.pivot u v ;
          max0 p
        with Unbounded -> p

    let rec choose_entry () =
      let u = Focus.choose () in
      try
        let p = T.find u in
        (u, p)
      with Not_found ->
        Focus.rem u ;
        choose_entry ()

    (** [zbnd()] removes all entries [x = p] from the current [focus] with
        constant [p] non-zero or [p] unbounded wrt current [focus]. *)
    let rec zbnd_star () =
      assert (Trace.zbnd_star ()) ;
      let changed = ref false in
      let remove u =
        if Focus.mem u then (
          changed := true ;
          Focus.rem u )
      in
      let zbnd () =
        let test u =
          try
            let p = T.find u in
            if
              (not (C.equal (P.const p) C.zero))
              || unbounded_poly Focus.mem p
            then remove u
          with Not_found -> remove u
        in
        Focus.iter test
      in
      zbnd () ;
      if !changed then zbnd_star ()

    let set0 z =
      assert (Trace.set0 z) ;
      assert (is_slack z) ;
      ( try
          let p = T.find z in
          T.restrict z p ;
          R.fuse_const z C.zero ;
          Focus.rem z
        with Not_found ->
          R.fuse_const z C.zero ;
          T.fuse_const z C.zero ) ;
      assert (not (occ z))

    let rec find_zeroes () =
      if not (is_empty ()) then (
        assert (Trace.find_zeroes ()) ;
        zbnd_star () ;
        maxentries () )

    and maxentries () =
      try
        let u, p = choose_entry () in
        let p' = max0 p in
        assert (is_slack u) ;
        assert (restricted p') ;
        if maximized_at_zero p' then (
          set0 u ;
          var_iter set0 p' ;
          Focus.rem u ;
          (* ?? *)
          () (* find_zeroes() *) )
        else (* unbounded in [focus]. *)
          ()
        (* find_zeroes() *)
      with Not_found -> ()

    let inc_zeroes w p =
      assert (Trace.inc_zeroes w p) ;
      assert (is_slack w) ;
      init_diff w (T.deps w) ;
      (* Initialize with the entries to be modified. *)
      T.compose w p ;
      filter_nonzero () ;
      (* Filter out all nonzero entries after composition. *)
      inc_bounded () ;
      find_zeroes ()
  end

  let rec process_nonneg p =
    assert (synchronized ()) ;
    let p = can p in
    let c = P.const p in
    if C.compare c C.zero >= 0 && minimized p then ()
    else if C.compare c C.zero < 0 && maximized p then raise Unsat
    else (
      add_nonneg p ;
      Deduce.real p ;
      ensure_combined_solset () )

  and add_nonneg p =
    assert (canonical p) ;
    try
      let y = choose_unrestricted p in
      assert (not (is_slack y)) ;
      add_ineq_r y p
    with Restricted ->
      assert (restricted p) ;
      add_ineq_t p

  and add_ineq_r y p =
    assert (not (is_slack y)) ;
    assert (canonical p) ;
    let w = fresh_slack () in
    let q = P.pivot w y p in
    assert (not (P.mem y q)) ;
    R.compose y q

  and add_ineq_t p =
    assert (restricted p) ;
    assert (canonical p) ;
    if minimized_nonneg p then ()
    else
      let w = fresh_slack () in
      add_ineq w p

  and add_ineq w p =
    assert (is_slack w) ;
    assert (restricted p) ;
    assert (T.canonical p) ;
    let c = C.compare (P.const p) C.zero in
    if c < 0 && maximized p then (* Maximize *)
      raise Unsat
    else if c >= 0 && minimized p then (* Minimize *)
      ()
    else if c = 0 && maximized p then
      (* FeasibleZero *)
      Derive0.inc_zeroes w p
    else if c > 0 then (* FeasibleNonzero *)
      T.compose w p
    else if c = 0 && unbounded_poly all p then
      (* FeasibleUnbounded *)
      T.compose w p
    else if c <= 0 then (
      try
        let u, v = bland all p in
        (* Pivot *)
        T.pivot u v ;
        let q = T.can p in
        assert (T.canonical q) ;
        add_ineq w q
      with Unbounded ->
        assert (not (maximized p)) ;
        assert (unbounded_poly all p) ;
        let v = choose_pos_unbounded all p in
        (* Unbounded *)
        T.compose v (P.pivot v w p) )
    else failwith "add_ineq: Unreachable"

  let rec process_pos p =
    assert (synchronized ()) ;
    let p = can p in
    let c = P.const p in
    if C.compare c C.zero > 0 && minimized p then ()
    else if C.compare c C.zero <= 0 && maximized p then raise Unsat
    else (
      add_strict_ineq p ;
      Deduce.real p ;
      ensure_combined_solset () )

  and add_strict_ineq p =
    assert (canonical p) ;
    try
      let y = choose_unrestricted p in
      assert (not (is_slack y)) ;
      add_strict_ineq_r y p
    with Restricted ->
      assert (restricted p) ;
      add_strict_ineq_t p

  and add_strict_ineq_r y p =
    assert (not (is_slack y)) ;
    assert (canonical p) ;
    let z = alias_const C.zero in
    let w = fresh_slack () in
    let q = P.pivot w y p in
    assert (not (P.mem y q)) ;
    R.compose y q ;
    Deduce.separate y z

  and add_strict_ineq_t p =
    assert (restricted p) ;
    assert (canonical p) ;
    let w = fresh_slack () in
    add_ineq w p ;
    let z = alias_const C.zero in
    let x = alias_slack w in
    assert (not (is_slack z)) ;
    assert (not (is_slack x)) ;
    Deduce.separate x z

  let rec process_deq0 p =
    assert (synchronized ()) ;
    let p = can p in
    if P.is_zero p then raise Unsat
    else
      let c = C.compare (P.const p) C.zero in
      if (c > 0 && minimized p) || (c < 0 && maximized p) then ()
      else (
        add_deq0 p ;
        Deduce.real p ;
        ensure_combined_solset () )

  and add_deq0 p =
    assert (canonical p) ;
    let p = if C.compare (P.const p) C.zero >= 0 then p else P.minus p in
    let z = alias_const C.zero in
    let x = alias p in
    assert (not (is_slack z)) ;
    assert (not (is_slack x)) ;
    Deduce.separate x z

  let rec process_eq0 p =
    assert (synchronized ()) ;
    let p = can p in
    add_eq0 p ;
    Deduce.real p ;
    ensure_combined_solset ()

  and add_eq0 p =
    assert (canonical p) ;
    try
      let c = P.d_constant p in
      if C.equal c C.zero then () else raise Unsat
    with P.Nonnum -> (
      try
        let y = choose_unrestricted p in
        assert (not (is_slack y)) ;
        add_eq_r y p
      with Restricted ->
        assert (restricted p) ;
        assert (T.canonical p) ;
        if C.compare (P.const p) C.zero <= 0 then add_eq_t p
        else add_eq_t (P.minus p) )

  and add_eq_r y p =
    assert (not (is_slack y)) ;
    assert (C.compare (P.coeff y p) C.zero <> 0) ;
    assert (canonical p) ;
    let q = P.solve0_for y p in
    assert (not (P.mem y q)) ;
    R.compose y q

  and add_eq_t p =
    assert (restricted p) ;
    assert (T.canonical p) ;
    assert (C.compare (P.const p) C.zero <= 0) ;
    let c = P.const p in
    let cmp = C.compare c C.zero in
    if cmp = 0 then
      if P.is_zero p then ()
      else
        (* EqDelete *)
        let v = choose_var p in
        (* Eq0 *)
        let q = P.solve0_for v p in
        Derive0.inc_zeroes v q
    else if maximized p then (* EqMax *)
      raise Unsat
    else
      try
        let u, v = bland all p in
        let cmp =
          C.compare
            (C.mult (C.minus c) (C.inv (P.coeff v p)))
            (gain all u v)
        in
        if cmp < 0 then
          (* EqSwap *)
          let q = P.solve0_for v p in
          T.compose v q
        else if cmp = 0 then
          (* EqSwap0 *)
          let q = P.solve0_for v p in
          Derive0.inc_zeroes v q
        else (
          (* EqPivotUp *)
          T.pivot u v ;
          let q = T.can p in
          assert (T.canonical q) ;
          add_eq_t q )
      with Unbounded ->
        (* EqSwap *)
        assert (not (maximized p)) ;
        assert (unbounded_poly all p) ;
        let v = choose_posvar p in
        let q = P.solve0_for v p in
        T.compose v q

  let propagate_eq x y =
    assert (not (is_slack x)) ;
    assert (not (is_slack y)) ;
    assert (V.equal x y) ;
    assert (not (V.canonical x)) ;
    assert (V.canonical y) ;
    assert (R.well_formed ()) ;
    ( try
        let p = T.can (R.find x) in
        R.restrict x ;
        try
          (* [R;T] not a solution set, therefore slacks in [find] need to be
             instantiated. *)
          let q = T.can (R.find y) in
          if P.equal p q then ()
          else
            let r = P.sub p q in
            assert (canonical r) ;
            add_eq0 r
        with Not_found ->
          let r = P.sub p (P.indet y) in
          assert (canonical r) ;
          add_eq0 r
      with Not_found -> (
        if not (cod x) then ()
        else try R.fuse x (R.find y) with Not_found -> R.fuse_var x y ) ) ;
    ensure_combined_solset () ;
    assert (R.well_formed ()) ;
    assert (not (R.occ x))

  let gc_slacks = ref true

  let normalize () =
    assert (synchronized ()) ;
    if !gc_slacks then T.normalize ()
end
