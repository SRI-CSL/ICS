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
  val isReal : var -> bool
  val isInteger : var -> bool
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
    module Slacks : (Sets.S with type elt = var)
    type t
    val current : unit -> Slacks.t
    val mem : var -> bool
  end
  module R : sig
    type t
    val empty: t  
    module Deps : (Powermaps.S with type key = var)
    module Constant : (Maps.S with type key = var and type value = coeff)
    module Solset : (Maps.S with type key = var and type value = poly)
    val constant : unit -> Constant.t
    val solset : unit -> Solset.t
    val dep : unit -> Deps.t
    val pp : Format.formatter -> unit
    val find : var -> poly
    val findConst : var -> coeff
    val inv : poly -> var
    val deps : var -> Deps.Values.t
    val dom : var -> bool
    val cod : var -> bool
  end 
  module T : sig
    type t
    val empty : t
    module Deps : (Powermaps.S with type key = var)
    module Solset : (Maps.S with type key = var and type value = poly)
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
  val findConst : var -> coeff
  val inv : poly -> var
  val invConst : coeff -> var
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
  val isNonneg : poly -> bool
  val isPos : poly -> bool
  val isDiseq0 : poly -> bool
  val isEqual0 : poly -> bool
  val initialize : t -> unit 
  val reset : unit -> unit
  val unchanged : unit -> bool
  val alias : poly -> var
  val aliasConst : coeff -> var
  exception Unsat
  val processEq0 : poly -> unit
  val processNonneg : poly -> unit 
  val processPos : poly -> unit 
  val processDeq0 : poly -> unit
  val propagateEq : var -> var -> unit
  val gcSlacks : bool ref
  val normalize : unit -> unit
end

module Make
  (P: POLYNOMIAL)
  (V: INTERFACE with type var = P.indet) = 
struct
  module C = P.Coeff
  module Var = P.Indet

  exception Unsat

  let varIter f = 
    let fC _ = () and fM x _ = f x in
      P.iter fC fM

  let posvarIter f = 
    let fC _ = () and fM x c = if C.compare c C.zero > 0 then f x in
      P.iter fC fM

  let varForAll p = 
    let pC _ =  true and pM x _ = p x in
      P.for_all pC pM

  let varChoose f p =
    fst(P.choose f p)

  let chooseVar =  
    let triv _ _ = true in
      varChoose triv

  let choosePosvar = 
    let posMonomial _ c = C.compare c C.zero > 0 in
      varChoose posMonomial
 
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
      | x :: xl -> varpp x; eprintf ", "; varlpp xl
    let termpp = P.pp err_formatter 
    let constpp = P.Coeff.pp err_formatter
    let slack x = if !debug then (eprintf "\nSlack(A): "; varpp x; eprintf "@?"); true
    let union x y = if !debug then (eprintf "\nDeduce(A): "; varpp x; eprintf " = "; varpp y; eprintf "@?"); true
    let separate x y = if !debug then (eprintf "\nDeduce(A): "; varpp x; eprintf " <> "; varpp y; eprintf "@?"); true
    let real x = if !debug then (eprintf "\nDeduce(A): real("; varpp x; eprintf ")@?"); true
    let extend x p = if !debug then (eprintf "\nExtend(A): "; varpp x; eprintf " = "; termpp p; eprintf "@?"); true
    let compose x p = if !debug then (eprintf "\nCompose(A): "; varpp x; eprintf " = "; termpp p; eprintf "@?"); true
    let pivot x y = if !debug then (eprintf "\nPivot(A): "; varpp x; eprintf " "; varpp y; eprintf "@?"); true
    let fuseConst x c = if !debug then (eprintf "\nFuse(A): "; varpp x; eprintf " = "; constpp c; eprintf "@?"); true
    let fuseVar x y = if !debug then (eprintf "\nFuse(A): "; varpp x; eprintf " = "; varpp y; eprintf "@?"); true
    let incBounded xl = if !debug then (eprintf "\nIncBounded(A): "; varlpp xl; eprintf "@?"); true
    let incZeroes x p = if !debug then (eprintf "\nIncZeroes(A): "; varpp x; eprintf " = "; termpp p; eprintf "@?"); true
    let set0 x = if !debug then (eprintf "\nSet0(A): "; varpp x; eprintf "@?"); true
    let zbndStar() = if !debug then (eprintf "\nzbnd(A) @?"); true
    let max0 p = if !debug then (eprintf "\nMax0(A): "; termpp p; eprintf "@?"); true
    let findZeroes() = if !debug then (eprintf "\nfindZeroes(A) @?"); true
  end

  module Term = struct
    type t = P.t
    let equal = P.equal
    let compare = Pervasives.compare
    let hash = P.hash
    let pp = P.pp
    type var = Var.t
    let of_var = P.indet
    let iter f = 
      let fC _ = () and fM x _ = f x in
	P.iter fC fM
    let map = P.map
  end 

  module Subst = Maps.Make(Var)(Term)
  module Dep = Powermaps.Make(Var)

  module S = struct
    module Slacks = Sets.Make(Var)
    module Config = Config.Set(Slacks)
    type t = Slacks.t
    let empty = Slacks.empty
    let initialize = Config.initialize
    let current = Config.current
    let mem = Config.mem
    let add = Config.add
    let remove = Config.remove
    let pp fmt = Slacks.pp fmt (current())
    let iter f = Slacks.iter f (current())
  end

  let is_slack x = 
    S.mem x

  let freshSlack () = 
    let k = Var.fresh() in
      assert(Trace.slack k);
      S.add k;
      k

  let slacks = S.current

  let extern p = 
    let externM x _ = not(is_slack x) in
      P.Map.for_all externM p.P.monomials
	
  let restricted p = 
    let restrM x _ = is_slack x in
      P.Map.for_all restrM p.P.monomials

  let minimized p = 
    let minM x c = is_slack x && C.compare c C.zero > 0 in
      P.Map.for_all minM p.P.monomials

  let maximized p = 
    let maxM x c = is_slack x && C.compare c C.zero < 0 in
      P.Map.for_all maxM p.P.monomials

  let maximizedAtZero p = C.equal (P.const p) C.zero && maximized p
  let maximizedNeg p = C.compare (P.const p) C.zero < 0 && maximized p
  let maximizedNonpos p = C.compare (P.const p) C.zero <= 0 && maximized p
  let minimizedNonneg p = C.compare (P.const p) C.zero >= 0 && minimized p 
  let minimizedPos p = C.compare (P.const p) C.zero > 0 && minimized p 

  module Deduce = struct
    let union x y =
      assert(not(is_slack x));
      assert(not(is_slack y));
      assert(Trace.union x y);
      V.union x y
    let separate x y =  
      assert(not(is_slack x));
      assert(not(is_slack y));
      assert(Trace.separate x y);
      V.separate x y
    let real1 x = 
      assert(not(is_slack x));
      assert(Trace.real x);
      V.real x
      
    let real p = 
      let addReal x = if not(is_slack x) then real1 (V.find x)in
	varIter addReal p
  end

  (** Solution set with bindings [x |-> p] where [x] is an unrestricted variable 
    and [p] is a polynomial with the additional restriction that [p] is not an unrestricted variable. *)
  module R = struct
    type var = Var.t
    type poly = P.t 
    module Deps = Powermaps.Make(Var)
    module Subst0 = Maps.Make(Var)(C)
    module Constant = Subst0
    module Solset = Subst
    module Inv0 = Maps.Make(C)(Var)

    type t = { 
      subst0 : Subst0.t;   (* [x |-> c] with [c] a constant *)
      inv0 : Inv0.t;  
      subst : Subst.t;     (* [x |-> p] with [p] a nonconstant polynomial. *)
      dep : Dep.t 
    }
 
    let empty = { 
      subst0 = Subst0.empty(); 
      inv0 = Inv0.empty(); 
      subst = Subst.empty(); 
      dep = Dep.empty()
    }

    let init = ref empty
		 
    module Config = struct  
      module Subst0 = Config.Map(Subst0)
      module Inv0 = Config.Map(Inv0)
      module Subst = Config.Map(Subst)
      module Dep = Config.Powermap(Dep)
    end

    let constant = Config.Subst0.current
    let solset = Config.Subst.current
    let dep = Config.Dep.current
      
    let initialize s =
      init := s;
      Config.Subst0.initialize s.subst0;
      Config.Inv0.initialize s.inv0;
      Config.Subst.initialize s.subst;
      Config.Dep.initialize s.dep
		      
    let unchanged = Config.Subst.unchanged
      
    let current () =
      if unchanged() then !init else { 
	subst0 = Config.Subst0.current();  
	inv0 = Config.Inv0.current(); 
	subst = Config.Subst.current(); 
	dep = Config.Dep.current() 
      }
		     
    let pp fmt = 
      Subst0.pp fmt (Config.Subst0.current());
      Subst.pp fmt (Config.Subst.current())

    let deps x = Config.Dep.find x

    let findConst x = 
      Subst0.find x (Config.Subst0.current())

    let findNonconst x = 
      Subst.find x (Config.Subst.current())

    let findSlack x = 
      let p = findNonconst x in
	try P.d_indet p with P.Nonindet -> raise Not_found

    let find x =
      try findNonconst x with Not_found -> 
	P.constant (findConst x)
	
    let dom x = Subst.mem x (Config.Subst.current())
    let cod y = Config.Dep.mem y
    let occ x = dom x || cod x

    let canonical p = 
      let canM x _ = not(dom x) in
	P.Map.for_all canM p.P.monomials

    let wfBinding x p = 
      not(is_slack x) &&
      (if P.is_indet p then is_slack (P.d_indet p) else true) &&
      not(P.mem x p)

    let well_formed () =
      let debug = ref true in
      let check name pred = 
	if not !debug then pred() else 
	  pred() ||
	  (Format.eprintf "\nERROR: well_formedness check %s for configuration R failed. 
                           Current state: \n" name;
	   pp Format.err_formatter;
	   Dep.pp Format.err_formatter (Config.Dep.current());
	   Format.eprintf "\n@?";
	   false)
      in
      let for_all0 p = Subst0.for_all p (Config.Subst0.current() )in
      let for_all p = Subst.for_all p (Config.Subst.current()) in
      let for_all_dep p = Dep.for_all p (Config.Dep.current()) in
      let disjoint () = 
	for_all (fun x _ -> for_all0 (fun y _ -> not(Var.equal x y)))
      and codomain () = 
	for_all 
	  (fun x p -> 
	     not(is_slack x) &&
	     not(P.is_constant p) &&
	     (try is_slack (P.d_indet p) with P.Nonindet -> canonical p))
      and deps1 () = 
	for_all_dep
	  (fun y xs ->
	     Dep.Values.for_all 
	        (fun x -> dom x && (P.mem y (findNonconst x))) xs)
      and deps2 () = 
	for_all
	  (fun x p -> 
	     let checkM y _ = Dep.Values.mem x (deps y) in
	       P.Map.for_all checkM p.P.monomials)
      and injectiveSubst0 () = 
	for_all0 (fun x c -> for_all0 (fun y d ->  if C.equal c d then Var.equal x y else true))
      and injectiveSubst () = 
	for_all (fun x p -> for_all (fun y q -> if P.equal p q then Var.equal x y else true))
      in
	check "disjoint" disjoint && 
	check "codomain" codomain && 
	check "deps1"deps1 && 
	check "deps2" deps2  && 
	check "injective0" injectiveSubst0 && 
	check "injective" injectiveSubst 
	  
    let is_empty () = 
      Subst.is_empty (Config.Subst.current()) &&
      Subst0.is_empty (Config.Subst0.current())

    let invConst c = 
      Inv0.find c (Config.Inv0.current())

    exception Found
    let invSlack = 
      let found: Var.t ref = ref (Obj.magic 0) in
	fun v -> 
	  assert(is_slack v);
	  let test x =
	    assert(dom x);
	    if Var.equal v (findSlack x) then (found := x; raise Found)
	  in
	    try
	      Dep.Values.iter test (deps v);
	      raise Not_found
	    with
		Found -> !found

    let invNonconst = 
      let found: Var.t ref = ref (Obj.magic 0) in
	fun p -> 
	  assert(not(P.is_constant p));
	  let dx = deps (chooseVar p) in
	  let test x = 
	    assert(dom x);
	    if P.equal p (find x) then (found := x; raise Found) 
	  in
	    try
	      Dep.Values.iter test dx;
	      raise Not_found
	    with
		Found -> !found
			
    let inv p = 
      try invConst (P.d_constant p) with P.Nonnum -> 
	invNonconst p

    let rec update x p q = 
      assert(dom x);
      assert(P.equal (findNonconst x) p);
      try
	let y = inv q in
	  Config.Subst.remove x;
	  varIter (Config.Dep.rem x) p;
	  Deduce.union x y;
	  assert(well_formed())
      with
	  Not_found -> 
	    try updateConst x p (P.d_constant q) with P.Nonnum -> 
	      try updateVar x p (P.d_indet q) with P.Nonindet -> 
		if not(wfBinding x q) then
		  begin
		    Format.eprintf "\nERROR : ";
		    Var.pp Format.err_formatter x;
		    Format.eprintf " = ";
		    P.pp Format.err_formatter q;
		    Format.eprintf "@?"
		  end;
		assert(wfBinding x q);
		Config.Subst.set x q;
		let remdepx z = if not(P.mem z q) then Config.Dep.rem x z in
		let adddepx = Config.Dep.add x in
		  varIter remdepx p;
		  varIter adddepx q;
		  assert(well_formed())
		    
    and updateConst x p c =
      let d = P.const p in       (* 1. [c = p <= d], 2. [c = p >= d]. *)
	if maximized p && C.compare c d > 0 then raise Unsat;
	if minimized p && C.compare c d < 0 then raise Unsat;
	Config.Subst0.set x c;
	Config.Inv0.set c x;
	Config.Subst.remove x;
	varIter (Config.Dep.rem x) p;
	assert(well_formed())

    and updateVar x p y = 
      if is_slack y then 
	begin
	  assert(wfBinding x (P.indet y));
	  Config.Subst.set x (P.indet y);
	  varIter (Config.Dep.rem x) p;
	  Config.Dep.add x y;
	  assert(well_formed())
	end
      else 
	begin
	  Config.Subst.remove x;
	  varIter (Config.Dep.rem x) p;
	  Deduce.union x y;
	  assert(well_formed())
	end
      	    
    let rec fuse y t = 
      assert(not(dom y));
      assert(not(P.mem y t));
      assert(canonical t);
      try fuseConst y (P.d_constant t) with P.Nonnum -> 
	try fuseVar y (P.d_indet t) with P.Nonindet -> 
	  let fuse1 x = 
	    try
	      let s = find x in
		(* assert(P.mem y s); *)
		let s' = P.replace y t s in
		  update x s s'
	    with
		Not_found -> ()
	  in
	    Dep.Values.iter fuse1 (deps y);
	    assert(not(cod y))

    and fuseConst y c = 
      assert(Trace.fuseConst y c);
      assert(not(dom y));
      let fuse1 x = 
	(* assert(dom x); *)
	try
	  let p = findNonconst x in
	  let q = P.instantiate y c p in
	    (* assert(P.mem y p); *)
	    assert(not(P.mem y q));
	    update x p q
	with
	    Not_found -> ()
      in
	Dep.Values.iter fuse1 (deps y);
	assert(not(cod y))

    and fuseVar y z = 
      assert(Trace.fuseVar y z);
      assert(not(dom y));
      assert(not(Var.equal y z));
      let fuse1 x = 
	assert(dom x);
	try
	  let p = find x in
	    assert(P.mem y p);
	    let q = P.rename y z p in
	      assert(not(P.mem y q));
	      update x p q
	with
	    Not_found -> ()
      in
	Dep.Values.iter fuse1 (deps y);
	assert(not(cod y))

    let rec extend x p = 
      assert(not(is_slack x));
      assert(Trace.extend x p);
      assert(not(dom x));
      try extendConst x (P.d_constant p) with P.Nonnum ->
	 extendNonconst x p

    and extendConst x c = 
      assert(not(dom x));
      try
	let y = invConst c in
	  Deduce.union x y
      with
	  Not_found -> 
	    Config.Subst0.set x c;
	    Config.Inv0.set c x;
	    assert(well_formed())

    and extendVar x y = 
      assert(not(is_slack x));
      if is_slack y then 
	let p = P.indet y in
	  assert(wfBinding x p);
	  Config.Subst.set x p;
	  Config.Dep.add x y
      else
	Deduce.union x y
      
    and extendNonconst x p = 
      assert(not(is_slack x));
      assert(not(dom x));
      assert(not(P.is_constant p));
      try extendVar x (P.d_indet p) with P.Nonindet -> 
	try Deduce.union x (invNonconst p) with Not_found ->
  	  assert(wfBinding x p);
	  Config.Subst.set x p;
	  varIter (Config.Dep.add x) p;
	  assert(well_formed())

    let rec compose x p =
      assert(Trace.compose x p);
      assert(not(dom x));
      try composeConst x (P.d_constant p) with P.Nonnum -> 
	composeNonconst x p

    and composeConst x c = 
      assert(not(dom x));
      fuseConst x c;
      extendConst x c

    and composeVar x y = 
      assert(not(dom x));
      if is_slack y then 
	try
	  let z = invSlack y in
	    fuseVar x y;
	    Deduce.union x z
	with
	    Not_found -> 
	      fuseVar x y;
	      assert(wfBinding x (P.indet y));
	      Config.Subst.set x (P.indet y);
	      Config.Dep.add x y;
	      assert(well_formed())
      else
	begin
	  fuseVar x y;
	  Deduce.union x y
	end 

    and composeNonconst x p = 
      assert(not(dom x));
      assert(not(P.is_constant p));	
      try composeVar x (P.d_indet p) with P.Nonindet -> 
	try
	  let z = invNonconst p in
	    fuse x p;
	    Deduce.union x z
	with
	    Not_found -> 
	      fuse x p;
	      extendNonconst x p
      
    let restrict x = 
      assert(well_formed());
      try
	let c = findConst x in
	  Config.Subst0.remove x;
	  Config.Inv0.remove c;
	  assert(well_formed())
      with
	  Not_found -> 
	    try
	      let p = findNonconst x in
		Config.Subst.remove x;
		varIter (Config.Dep.rem x) p;
		assert(well_formed())
	    with 
		Not_found -> ()
  end

  module T = struct
    type var = Var.t
    type poly = P.t

    module Deps = Powermaps.Make(Var)

    type t = { subst : Subst.t; dep : Dep.t }
 
    let empty = { subst = Subst.empty(); dep = Dep.empty() }

    let init = ref empty
		 
    module Config = struct
      module Subst = Config.Map(Subst)
      module Dep = Config.Powermap(Dep)
    end 

    module Solset = Subst
    let solset = Config.Subst.current
    let dep = Config.Dep.current
		
    let pp fmt = Subst.pp fmt (solset())
    let find x = Subst.find x (solset())
    let deps x = Dep.find x (dep())
		   
    let dom x = Subst.mem x (solset())
    let cod y = Dep.mem y (dep())
    let occ x = dom x || cod x

    let is_empty () = Subst.is_empty (solset())

    let iter f = Subst.iter f (solset())

    let iter0 f = 
      let f0 u p = if C.equal (P.const p) C.zero then f u p in
	Subst.iter f0 (solset())

    let for_all p = Subst.for_all p (solset())

    let wfBinding x p = 
      is_slack x &&
      restricted p &&
      not(P.is_indet p) &&
      not(P.is_constant p) &&
      C.compare (P.const p) C.zero >= 0 &&
      not(P.mem x p)
					
    let well_formed () = 
      let debug = ref true in
      let check name pred = 
	if not !debug then pred() else 
	  pred() ||
	  (Format.eprintf 
	     "\nERROR: well_formedness check %s for configuration T failed. 
              Current state: \n" name;
	   Subst.pp Format.err_formatter (Config.Subst.current());
	   Format.eprintf "\n with dependencies: \n";
	   Dep.pp Format.err_formatter (Config.Dep.current());
	   Format.eprintf "\n@?";
	   false)
      in
      let for_all_find p = Subst.for_all p (Config.Subst.current()) in
      let for_all_dep p = 
	Dep.for_all (fun u -> Dep.Values.for_all (p u)) (Config.Dep.current()) 
      in
      let solved () = 
	for_all_find 
	  (fun u p -> 
	     wfBinding u p && varForAll (fun v -> not (dom v)) p) 
      in
      let depsOK () = 
	for_all_dep 
	  (fun v u -> is_slack v && is_slack u && dom u && P.mem v (find u)) 
      in
	check "solved" solved && check "deps" depsOK
      
    let initialize s =
      init := s; 
      Config.Subst.initialize s.subst;
      Config.Dep.initialize s.dep
		      
    let unchanged = Config.Subst.unchanged
      
    let current () =
      if unchanged() then !init else
	{ subst = solset(); dep = dep() }

    let canonical p = 
      let domM x _ = not (dom x) in
	P.Map.for_all domM p.P.monomials

    let can p = 
      let q = P.map find p in
	assert(canonical q);
	q
			
    exception Found
    let inv = 
      let found: Var.t ref = ref (Obj.magic 0) in
	fun p -> 
	  let us = deps (chooseVar p) in
	  let testInv u = 
	    assert(dom u);
	    if P.equal p (find u) then (found := u; raise Found) 
	  in
	    try
	      Dep.Values.iter testInv us;
	      raise Not_found
	    with
		Found -> !found

    let restrict x p = 
      assert(dom x);
      assert(P.equal (find x) p);
      Config.Subst.remove x;
      varIter (Config.Dep.rem x) p;
      assert(well_formed())
	    
    let rec fuse y t =
      assert(not(dom y));
      assert(not(P.mem y t));  
      assert(not(P.is_constant t));
      assert(not(P.is_indet t));
      let fuse1 x = 
	try
	  let s = find x in
	    (* assert(P.mem y s);  *)(* ?? *)
	    let s' = P.replace y t s in
	      assert(not(P.mem y s'));
	      update x s s'
	with
	    Not_found -> () (* Might occur because of calls to *)
      in                           (* [restrict] in this loop. *)
	Dep.Values.iter fuse1 (deps y);
	assert(not (cod y))

    and fuseVar y z = 
      assert(not(dom z));
      let fuse1 u = 
	try
	  let p = find u in
	  let q = P.rename y z p in   
	    assert(P.mem y p); (* ?? *)
	    assert(not(P.mem y q));
	    update u p q
	with
	    Not_found -> ()
      in
	Dep.Values.iter fuse1 (deps y);
	assert(not(cod y))

    and fuseConst y c = 
      let fuse1 u = 
	try
	  let p = find u in
	  let q = P.instantiate y c p in   
	    assert(P.mem y p);
	    assert(not(P.mem y q));
	    update u p q
	with
	    Not_found -> ()
      in
	Dep.Values.iter fuse1 (deps y);
	assert(not(cod y))

    and update x p q = 
      assert(dom x);
      assert(P.equal (find x) p);
      try updateConst x p (P.d_constant q) with P.Nonnum -> 
	try updateVar x p (P.d_indet q) with P.Nonindet -> 	
	  assert(wfBinding x q);
	  try
	    let y = inv q in
	      restrict y q; 
	      R.fuseVar y x; 
	      assert(not(occ y) && not(R.cod y));
	      assert(R.well_formed());
	      Config.Subst.set x q;
	      varIter (fun y -> if not(P.mem y q) then Config.Dep.rem x y) p;
	      varIter (fun y -> Config.Dep.add x y) q;
	      assert(well_formed())
	  with
	      Not_found -> 	
		if maximizedAtZero q then           (* derive equalities. *)
		  begin
		    restrict x p;
		    deriveZeros x q 
		  end
		else
		  begin
		    Config.Subst.set x q;
		    varIter (fun y -> if not(P.mem y q) then Config.Dep.rem x y) p;
		    varIter (fun y -> Config.Dep.add x y) q;
		    assert(well_formed())
		  end
		  
    and updateConst x p c = 
      assert(dom x);
      assert(P.equal (find x) p);
      if C.compare c C.zero < 0 then raise Unsat else
	(restrict x p; 
	 R.fuseConst x c;
	 assert(R.well_formed()))
	
    and updateVar x p y = 
      assert(dom x);
      assert(P.equal (find x) p);
      restrict x p; 
      R.fuseVar x y;
      assert(R.well_formed())
	
    and deriveZeros x q =
      assert(is_slack x);
      assert(maximizedAtZero q);
      R.fuseConst x C.zero;
      let fuse0 y = 
	R.fuseConst y C.zero; 
	fuseConst y C.zero 
      in
	varIter fuse0 q
	
    let extend x p = 
      assert(not (dom x)); 
      assert(wfBinding x p); 
      if maximizedAtZero p then deriveZeros x p else
	try
	  let y = inv p in
	    restrict y p;
	    R.fuseVar y x;
	    assert(not(occ y) && not(R.cod y));
	    Config.Subst.set x p;
	    varIter (Config.Dep.add x) p;  
	    assert(well_formed())
	with
	    Not_found ->  
	      Config.Subst.set x p;
	      varIter (Config.Dep.add x) p;
	      assert(well_formed())

    let rec compose x p =
      assert(Trace.compose x p);
      assert(not(dom x));
      assert(not(P.mem x p));
      assert(restricted p);
      try composeConst x (P.d_constant p) with P.Nonnum -> 
	try composeVar x (P.d_indet p) with P.Nonindet -> 
	  if maximizedAtZero p then composeMaxAt0 x p else
	    (fuse x p; extend x p)
     
    and composeConst x c = 
      assert(not(dom x));
      if C.compare c C.zero < 0 then raise Unsat else
	(R.fuseConst x c; 
	 fuseConst x c;
	 assert(not(occ x));
	 assert(R.well_formed()))
    
    and composeVar x y =  
      assert(not(dom x));
      R.fuseVar x y;
      fuseVar x y;
      assert(not(occ x));
      assert(R.well_formed())

    and composeMaxAt0 x p = 
      assert(not(dom x));
      assert(not(P.mem x p));
      assert(restricted p);
      assert(C.equal (P.const p) C.zero && maximized p);
      R.fuseConst x C.zero;
      let fuse0 y = R.fuseConst y C.zero; fuseConst y C.zero in
	varIter fuse0 p;
	assert(R.well_formed())
      
    let pivot u v =
      assert(Trace.pivot u v);
      assert(dom u);
      assert(P.mem v (find u));
      assert(not(dom v));
      let p = find u in
	assert(P.mem v p);
	let q = P.pivot u v p in 
	  assert(not(P.mem v q));
	  restrict u p;
	  compose v q

    let normalize () =  
      let rem u = 
	if not(R.cod u) && not(occ u) then
	  S.remove u
      in
	S.iter rem 
	 
  end

  type t = {
    regular : R.t; 
    tableau : T.t; 
    slacks : S.t
  }
      
  let empty = {
    regular = R.empty; 
    tableau = T.empty; 
    slacks = S.empty()
  }

  let pp fmt = 
    Format.fprintf fmt "@[";
    Format.fprintf fmt "r: "; R.pp fmt;
    Format.fprintf fmt "\nt: "; T.pp fmt;
    Format.fprintf fmt "\nslacks: "; S.pp fmt;
    Format.fprintf fmt "@]@?"
		
  let init = ref empty

  let initialize s = 
    init := s;
    R.initialize s.regular; 
    T.initialize s.tableau; 
    S.initialize s.slacks

  let reset () =
    initialize empty

  let unchanged() = 
    R.unchanged() && 
    T.unchanged()
	    
  let dom x = R.dom x || T.dom x

  let cod x = R.cod x || T.cod x

  let occ x = dom x || cod x

  let synchronized() = 
    let canonized x p = 
      V.canonical x &&
      varForAll V.canonical p
    in
      true || (* disable for now *)
      Subst.for_all canonized (R.Config.Subst.current())

  let feasible () =
    let nonnegConst _ p = C.compare (P.const p) C.zero >= 0 in
      T.for_all nonnegConst

  let combinedSolset() = 
    T.for_all (fun u _ -> not(R.cod u))

  (** Plug domain variables in [T] into codomain of [R] to ensure that
    the combined configuration [R; T] is a solution set. *)
  let ensureCombinedSolset = 
    let module Focus = struct
      let curr = Stacks.create()
      let mem x = Stacks.mem Var.equal  x curr
      let reset () = Stacks.clear curr
      let add x = if not(mem x) then Stacks.push x curr
      let is_empty () = Stacks.is_empty curr
      let next () = Stacks.pop curr
    end
    in 
      fun () -> 
	assert(synchronized());
	Focus.reset();
	T.iter (fun u _ -> Dep.Values.iter Focus.add (R.deps u));
	while not(Focus.is_empty()) do
	  let x = Focus.next() in
	    try
	      let p = R.find x in
	      let p' = P.map T.find p in
		R.update x p p'
	    with
		Not_found -> ()
	done;
	assert(combinedSolset())

  let current() =
    if unchanged() then !init else {
      regular = R.current(); 
      tableau = T.current(); 
      slacks = S.current() 
    }

  let is_empty() =  R.is_empty() && T.is_empty()

  exception Restricted
  let chooseUnrestricted p = 
    let unrestricted x _ = not(is_slack x) in
      try varChoose unrestricted p with Not_found -> raise Restricted


  let findConst = R.findConst

  let invConst = R.invConst

  let find x = 
    try R.find x with Not_found -> T.find x

  let inv p = 
     try R.inv p with Not_found -> T.inv p

  let canonical p = 
    let canM x _ = not(dom x) && V.canonical x in
      P.Map.for_all canM p.P.monomials

  let can p = 
    assert(synchronized());
    let q = P.map find p in
      (* assert(canonical q); *)
      q

  let all _ = true

  (** [v] is {i unbounded} in [T] iff [K(T(u), v) > 0] for all [u] in [T]. *)
  let unbounded t v = 
    T.cod v &&
    Dep.Values.for_all  
      (fun u -> 
	 assert(T.dom u);
	 t u && C.compare (P.coeff v (T.find u)) C.zero > 0)
      (T.deps v)

  (** [v] is {i bounded} in [T] iff [K(T(u), v) < 0] for some [u] in [T]. *)
  let bounded t v = 
    Dep.Values.exists
      (fun u -> 
	 assert(T.dom u);
	 t u && C.compare (P.coeff v (T.find u)) C.zero < 0)
      (T.deps v)

  let unboundedPoly t p = 
    let unb x c = C.compare c C.zero > 0 && unbounded t x in
      P.Map.exists unb p.P.monomials

  let choosePosUnbounded t =
    let posUnbounded x c = C.compare c C.zero > 0 && unbounded t x in
      varChoose posUnbounded

  let pivotCandidate t u v = 
    t u && T.dom u && C.compare (P.coeff v (T.find u)) C.zero < 0

  let gain t u v = 
    assert(pivotCandidate t u v);
    let p = T.find u in
      C.mult (C.minus (P.const p)) (C.inv (P.coeff v p))

  let pivotable t u v = 
    pivotCandidate t u v &&
    let g = gain t u v in
    let smallestGain u' =
      if pivotCandidate t u' v then g <= gain t u' v else true
    in
      Dep.Values.for_all smallestGain (T.deps v)

  (** Given a bounded [v] search for [u] such that 
    [pivotable u v] according to Bland's rule. *)
  let blandPivot = 
    let arbg = C.minus C.one and arbu = Obj.magic 0 in
    let ming = ref arbg and minu = ref arbu in
      fun t v -> 
	assert(bounded t v);
	let us = T.deps v in
	  assert(not(Dep.Values.is_empty us));
	  let pivot u = 
	    assert(dom u);
	    if not(t u) then () else
	      let p = find u in
	      let c = P.const p in
	      let d = P.coeff v p in (* (u = c + d * v + p'] with [d < 0]. *)
		assert(C.compare c C.zero >= 0);
		if C.compare d C.zero < 0 then
		  let g = C.mult (C.minus c) (C.inv d) in
		    assert(C.compare g C.zero >= 0);
		    if C.compare !ming C.zero < 0 then (* undefined *)
		      (ming := g; minu := u)
		    else
		      let cmp = C.compare !ming g in
			if cmp > 0 || (cmp = 0 && Var.compare !minu u > 0) then
			  (ming := g; minu := u)
	  in	 
 	    ming := arbg; minu := arbu;
	    Dep.Values.iter pivot us;
	    assert(C.compare !ming C.zero >= 0);
	    !minu
	  
  exception Unbounded	   
  let rec bland t p = 
    assert(restricted p);
    let posBounded x c = C.compare c C.zero > 0 && bounded t x in
      try
	let v = varChoose posBounded p in
	let u = blandPivot t v in
	  assert(pivotable t u v);
	  (u, v)
      with
	  Not_found ->
	   (*  assert(unboundedPoly t p || maximized p); *)
	    raise Unbounded

  let alias p =
    assert(synchronized());
    let p = can p in
      try V.find (inv p) with Not_found -> 
	let v = Var.fresh () in
	  Deduce.real1 v;
	  R.extend v p;
	  v

  let aliasConst c = 
    try V.find (R.invConst c) with Not_found -> 
      let v = Var.fresh() in
	Deduce.real1 v;
	R.extendConst v c;
	v

  let aliasSlack w = 
    assert(is_slack w);
    try V.find (R.invSlack w) with Not_found -> 
      let x = Var.fresh() in
	R.extendVar x w;
	x
 
  let rec max p = 
    assert(restricted p);
    assert(synchronized());
    let p = T.can p in
      try
	let u, v = bland all p in
	  T.pivot u v;
	  max p
      with
	  Unbounded -> 
	    (* assert(unboundedPoly all p || maximized p); *)
	    p

  let min p = 
    assert(synchronized());
    P.minus (max (P.minus p))

  let isEqual0 p = 
    P.equal (can p) P.zero

  let complete = ref true

  let isNonneg p = 
    let p = can p in
      (minimizedNonneg p) ||
      (restricted p && minimizedNonneg (min p))

  let isDiseq0Var p =
    try 
      let x = P.d_indet p in
      let z = R.invConst C.zero in
	(* V.Varset.mem x (V.deqs z) *)
	false
    with
	_ -> false

  let isDiseq0 p = 
    let p = can p in 
      (isDiseq0Var p) ||
      (minimizedPos p) ||
      (maximizedNeg p) ||
      (!complete &&
       restricted p && (minimizedPos (min p) || maximizedNeg (max p)))
      
  let isPos p = 
    isNonneg p && isDiseq0 p

 (** Make implicit variable equalities explicit. *)
  module Derive0 = struct

    (** Focus contains a subset of the current domain variables of [T]. *)
    module Focus = struct
      module Vars = Hashtbl.Make(Var)
      let focus = Vars.create 7
      let is_empty () = Vars.length focus = 0
      let clear () = if not(is_empty()) then (Vars.clear focus; assert(is_empty()))
      let mem u = Vars.mem focus u
      let add u = if not(mem u) then Vars.add focus u true
      let rem u = Vars.remove focus u
      let to_list () = Vars.fold (fun x _ acc -> x :: acc) focus []
      let iter f = 
	let f' x _ = f x in
	  Vars.iter f' focus
      exception Found of Var.t
      let choose() = 
	let get x _ = raise(Found(x)) in
	  try Vars.iter get focus; raise Not_found with Found(x) -> x
    end
	
    let initDiff w ws = 
      Focus.clear(); 
      Focus.add w; 
      Dep.Values.iter Focus.add ws

    let filterNonzero () =  
      let remNonzero u =
	try 
	  let p = T.find u in
	    if C.compare (P.const p) C.zero <> 0 then Focus.rem u
	with
	    Not_found -> Focus.rem u  (* Entry has been deleted by [T.compose]. *)
      in
	Focus.iter remNonzero
      
    (** Compute all entries [u = p] that contain positive occurrences of a
      variable [v] in [p] that occurs negatively in an entry that 
      might be maximized at [0]. *)
    let rec incBounded () =   

      assert(Trace.incBounded (Focus.to_list()));
      let changed = ref false in
      let rec inspectEntry0 u p = 
	assert(C.equal (P.const p) C.zero);
	posvarIter inspectPosvar p
      and inspectPosvar v = 
	if not(Focus.mem v) && occursNeg v then
	  begin
	    changed := true;
	    Focus.add v
	  end 
      and occursNeg v = 
	let test u = 
	  Focus.mem u &&
	  (try 
	     let p = T.find u in
	     let c = P.coeff v p in
	       C.compare c C.zero < 0
	   with
	       Not_found -> false)
	in
	  Dep.Values.exists test (T.deps v)
      in
	T.iter0 inspectEntry0;
	if !changed then incBounded() else
	  assert(Trace.incBounded (Focus.to_list()))

    let rec max0 p = 
      assert(Trace.max0 p);
      assert(restricted p);
      let p = T.can p in
	if C.compare (P.const p) C.zero > 0 then p else
	  try
	    let u, v = bland Focus.mem p in
	      Focus.add v; 
	      Focus.rem u;    (* ensure only domain variables are in [focus]. *)
	      T.pivot u v;
	      max0 p
	  with
	      Unbounded -> p

    let rec chooseEntry () = 
      let u = Focus.choose() in
	try
	  let p = T.find u in
	    u, p
	with
	    Not_found -> (Focus.rem u; chooseEntry())

    (** [zbnd()] removes all entries [x = p] from the current [focus] with
      constant [p] non-zero or [p] unbounded wrt current [focus]. *)
    let rec zbndStar() =
      assert(Trace.zbndStar());
      let changed = ref false in
      let remove u = if Focus.mem u then (changed := true; Focus.rem u) in
      let zbnd () = 
	let test u = 
	  try
	    let p = T.find u in
	      if not(C.equal (P.const p) C.zero) || 
		unboundedPoly Focus.mem p 
	      then
		remove u
	  with
	      Not_found -> remove u
	in
	  Focus.iter test
      in
	zbnd();
	if !changed then zbndStar()

    let set0 z = 
      assert(Trace.set0 z);
      assert(is_slack z);
      (try
	 let p = T.find z in
	   T.restrict z p;
	   R.fuseConst z C.zero;  
	   Focus.rem z;	 
       with
	   Not_found -> 
	     R.fuseConst z C.zero; 
	     T.fuseConst z C.zero);
      assert(not(occ z))
	
    let rec findZeroes() = 
      if not(is_empty()) then
	begin 
	  assert(Trace.findZeroes());
	  zbndStar();
	  maxentries()
	end
	
    and maxentries() = 
      try
	let u, p = chooseEntry() in
	let p' = max0 p in
	  assert(is_slack u);
	  assert(restricted p');
	  if maximizedAtZero p' then
	    begin
	      set0 u;
	      varIter set0 p';
	      Focus.rem u;   (* ?? *)
	      ()   (* findZeroes() *)
	    end 
	  else (* unbounded in [focus]. *)
	    ()
	    (* findZeroes() *)
      with
	  Not_found -> ()
	
    let incZeroes w p = 
      assert(Trace.incZeroes w p);
      assert(is_slack w);
      initDiff w (T.deps w);   (* Initialize with the entries to be modified. *)
      T.compose w p;
      filterNonzero();   (* Filter out all nonzero entries after composition. *)
      incBounded();
      findZeroes()
  end
      
  let rec processNonneg p =
    assert(synchronized());
    let p = can p in
    let c = P.const p in
      if C.compare c C.zero >= 0  && minimized p then () else
	if C.compare c C.zero < 0  && maximized p then raise Unsat else
	  begin
	    addNonneg p;
	    Deduce.real p;
	    ensureCombinedSolset()
	  end

  and addNonneg p = 
    assert(canonical p);
    try
      let y = chooseUnrestricted p in
	assert(not(is_slack y));
	addIneqR y p
    with
	Restricted -> 
	  assert(restricted p);
	  addIneqT p
	  
  and addIneqR y p = 
    assert(not(is_slack y));
    assert(canonical p);
    let w = freshSlack () in
    let q = P.pivot w y p in
      assert(not(P.mem y q));
      R.compose y q

  and addIneqT p = 
    assert(restricted p);
    assert(canonical p); 
    if minimizedNonneg p then () else
      let w = freshSlack () in
	addIneq w p

  and addIneq w p = 
    assert(is_slack w);
    assert(restricted p);
    assert(T.canonical p);
    let c = C.compare (P.const p) C.zero in
      if c < 0 && maximized p then              (* Maximize *)
	raise Unsat
      else if c >= 0 && minimized p then        (* Minimize *)
	()
      else if c = 0 && maximized p then         (* FeasibleZero *)
	Derive0.incZeroes w p
      else if c > 0 then                        (* FeasibleNonzero *)
	T.compose w p
      else if c = 0 && unboundedPoly all p then  (* FeasibleUnbounded *)
	T.compose w p
      else if c <= 0 then
	try
	  let u, v = bland all p in              (* Pivot *)    
	    T.pivot u v;
	    let q = T.can p in
	      assert(T.canonical q);
	      addIneq w q
	with
	    Unbounded -> 
	      assert(not(maximized p));
	      assert(unboundedPoly all p);
	      let v = choosePosUnbounded all p in   (* Unbounded *)
		T.compose v (P.pivot v w p)
      else 
	failwith "addIneq: Unreachable"

  let rec processPos p = 
    assert(synchronized());
    let p = can p in
    let c = P.const p in 
      if C.compare c C.zero > 0 && minimized p then () else
	if C.compare c C.zero <= 0 && maximized p then raise Unsat else
	  begin
	    addStrictIneq p;
	    Deduce.real p;
	    ensureCombinedSolset()
	  end

  and addStrictIneq p = 
    assert(canonical p);
    try
      let y = chooseUnrestricted p in
	assert(not(is_slack y));
	addStrictIneqR y p
    with
	Restricted -> 
	  assert(restricted p);
	  addStrictIneqT p
	  
  and addStrictIneqR y p = 
    assert(not(is_slack y));
    assert(canonical p);
    let z = aliasConst C.zero in
    let w = freshSlack () in
    let q = P.pivot w y p in
      assert(not(P.mem y q));
      R.compose y q;
      Deduce.separate y z

  and addStrictIneqT p = 
    assert(restricted p);
    assert(canonical p); 
    let w = freshSlack () in
      addIneq w p;
      let z = aliasConst C.zero in
      let x = aliasSlack w in
	assert(not(is_slack z));
	assert(not(is_slack x));
	Deduce.separate x z

  let rec processDeq0 p = 
    assert(synchronized());
    let p = can p in
      if P.is_zero p then raise Unsat else
	let c = C.compare (P.const p) C.zero in
	  if (c > 0 && minimized p) || (c < 0 && maximized p) then () else
	    begin
	      addDeq0 p; 
	      Deduce.real p;
	      ensureCombinedSolset()
	    end

  and addDeq0 p = 
    assert(canonical p);
    let p = if C.compare (P.const p) C.zero >= 0 then p else P.minus p in 
    let z = aliasConst C.zero in
    let x = alias p in
      assert(not(is_slack z));
      assert(not(is_slack x));
      Deduce.separate x z
     
  let rec processEq0 p = 
    assert(synchronized());
    let p = can p in
      addEq0 p;
      Deduce.real p;
      ensureCombinedSolset()

  and addEq0 p = 
    assert(canonical p);
    try 
      let c = P.d_constant p in
	if C.equal c C.zero then () else
	  raise Unsat
    with
	P.Nonnum -> 
	  try 
	    (let y = chooseUnrestricted p in
	       assert(not(is_slack y));
	       addEqR y p)
	  with 
	      Restricted -> 
		assert(restricted p);
		assert(T.canonical p);
		if C.compare (P.const p) C.zero <= 0 then addEqT p else 
		  addEqT (P.minus p)
		    
  and addEqR y p = 
    assert(not(is_slack y));
    assert(C.compare (P.coeff y p) C.zero <> 0);
    assert(canonical p);
    let q = P.solve0For y p in
      assert(not(P.mem y q));
      R.compose y q

  and addEqT p =
    assert(restricted p);
    assert(T.canonical p);
    assert(C.compare (P.const p) C.zero <= 0);
    let c = P.const p in
    let cmp = C.compare c C.zero in
      if cmp = 0 then  
	if P.is_zero p then () else        (* EqDelete *)
	  let v = chooseVar p in           (* Eq0 *)
	  let q = P.solve0For v p in
	    Derive0.incZeroes v q
      else if maximized p then             (* EqMax *)
	raise Unsat
      else
	try
	  let u, v = bland all p in
	  let cmp = 
	    C.compare (C.mult (C.minus c) (C.inv (P.coeff v p))) (gain all u v) 
	  in
	    if cmp < 0 then                (* EqSwap *)
	      let q = P.solve0For v p in
		T.compose v q
	    else if cmp = 0 then           (* EqSwap0 *)
	      let q = P.solve0For v p in
		Derive0.incZeroes v q
	    else                           (* EqPivotUp *)
	      begin
		T.pivot u v;
		let q = T.can p in
		  assert(T.canonical q);
		  addEqT q
	      end

	with
	    Unbounded ->                   (* EqSwap *)
	      assert(not(maximized p));
	      assert(unboundedPoly all p);  
	      let v = choosePosvar p in
	      let q = P.solve0For v p in
		T.compose v q
    
  let propagateEq x y = 
    assert(not(is_slack x));
    assert(not(is_slack y));
    assert(V.equal x y);
    assert(not(V.canonical x));
    assert(V.canonical y);
    assert(R.well_formed());
    (try
      let p = T.can (R.find x) in
	R.restrict x; 
	(try                (* [R;T] not a solution set, therefore. *)
	  let q = T.can (R.find y) in   (* slacks in [find] need to *)
	    if P.equal p q then () else         (* be instantiated. *)
	      let r = P.sub p q in 
		assert(canonical r);
		addEq0 r
	with
	    Not_found -> 
	      let r = P.sub p (P.indet y) in
		assert(canonical r);
		addEq0 r)
    with
	Not_found -> 
	  if not(cod x) then () else
	    (try R.fuse x (R.find y) with
		 Not_found -> R.fuseVar x y));
    ensureCombinedSolset();
    assert(R.well_formed());
    assert(not(R.occ x))

  let gcSlacks = ref true

  let normalize () =  
    assert(synchronized());
    if !gcSlacks then T.normalize()
	 
end
