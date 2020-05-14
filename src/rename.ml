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



module type PROPVAR = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val fresh : unit -> t
end

module type PREDSYM = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val sub : t -> t -> bool
  val disjoint : t -> t -> bool
end

module type VAR = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type INTERFACE = sig
  type propvar 
  type predsym
  type var
  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val equiv : propvar -> propvar -> unit
  val disjoint : propvar -> propvar -> unit 
  val implies : propvar -> propvar -> unit
  val valid0 : propvar -> unit
  val unsat0 : propvar -> unit
  val valid1 : predsym -> var -> unit
  val unsat1: predsym -> var -> unit 
  val union : var -> var -> unit
  val separate : var -> var -> unit
end

module type INFSYS = sig
  type propvar  
  type predsym
  type var 
  type t
  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val is_empty : unit -> bool
  val current : unit -> t
  module Monadic : (Maps.S with type key=propvar and type value = predsym*var) 
  module Equal : (Maps.S with type key = propvar and type value = var*var)
  val monadic: unit -> Monadic.t
  val equal : unit -> Equal.t
  val aliasMonadic : predsym -> var -> propvar
  val aliasEqual : var -> var -> propvar
  val propagateEq : var -> var -> unit 
  val propagateDeq : var -> var -> unit
  val propagateValid0 : propvar -> unit
  val propagateUnsat0 : propvar -> unit
  val propagateValid1 : predsym -> var -> unit
  val propagateUnsat1 : predsym -> var -> unit
end

module Make(Propvar: PROPVAR)(Sym: PREDSYM)(Var: VAR)
  (I : INTERFACE with type propvar = Propvar.t
		 and type predsym = Sym.t
		 and type var = Var.t) = 
struct
  type var = Var.t
  type predsym = Sym.t
  type propvar = Propvar.t

  module Apply = struct
    type t = predsym*var
    let equal (p, x) (q, y) = 
      Sym.equal p q && Var.equal x y
    let pp fmt (p, x) = 
      Sym.pp fmt p; 
      Format.fprintf fmt "("; 
      Var.pp fmt x;
      Format.fprintf fmt ")@?"
  end
 
  module Var2 = struct
    type t = var*var
    let equal (x1, y1) (x2, y2) = Var.equal x1 x2 && Var.equal y1 y2
    let pp fmt (x, y) = Var.pp fmt x; Format.fprintf fmt " = "; Var.pp fmt y
  end
 
  module Monadic = Maps.Make(Propvar)(Apply)
  module Equal = Maps.Make(Propvar)(Var2)

  module Propvars = Sets.Make(Propvar)
  module Dep = Maps.Make(Var)(Propvars)

  (** Renamings of the form
    - [u |-> p(x)] for [monadic] predicates [p(x)] and propositional variable [u], and
    - [v |-> x = y] for variable [equal]ities. [x = y] and propositional variable [v].

   The index [dep] has bindings [x |-> {u{1},...,u{n}}] with
    - [x] is [V]-canonical
    - [u{i}] for [i=1,...,n] is in the domain of either [monadic] or [find]. *)
  type t = {
    mutable monadic : Monadic.t; 
    mutable equal : Equal.t;   
    mutable dep : Dep.t
  }

  let empty = { 
    monadic = Monadic.empty();
    equal = Equal.empty();
    dep = Dep.empty()
  }
  
  let init = ref empty
   
  module Config = struct
    module Monadic = Config.Map(Monadic)
    module Equal = Config.Map(Equal)
    module Dep = Config.Map(Dep)
  end

  let monadic = Config.Monadic.current
  let equal = Config.Equal.current

  let domMonadic v = Monadic.mem v (monadic())
  let domEqual v = Equal.mem v (equal())
    
  let dom v = domMonadic v || domEqual v

  let findMonadic v = Monadic.find v (monadic())
  let findEqual v = Equal.find v (equal()) 

  let deps = 
    let empty = Propvars.empty() in
      fun x -> 
	try Config.Dep.find (I.find x) with Not_found -> 
	  assert(Propvars.is_empty empty);
	  empty

  let synchronized() = 
    Dep.for_all (fun x _ -> I.canonical x) (Config.Dep.current())

  let well_formed() = 
    let forallDep p = Dep.for_all (fun x us -> Propvars.for_all (fun u -> (p u x)) us) (Config.Dep.current()) in
    let forallMonadic p = Monadic.for_all p (Config.Monadic.current()) in
    let forallEqual p = Equal.for_all p (Config.Equal.current()) in
    let check name pred = 
      pred() ||
      (Format.eprintf "\nERROR: well_formedness check %s for renaming failed. Current: \n" name;
       Format.eprintf "\nmonadic: "; Monadic.pp Format.err_formatter (Config.Monadic.current());
       Format.eprintf "\nequal: "; Equal.pp Format.err_formatter (Config.Equal.current());
       Format.eprintf "\nequal: "; Dep.pp Format.err_formatter (Config.Dep.current());
       Format.eprintf "\n@?";
       false)
    in
    let depWF () = 
      forallDep 
	(fun u x -> 
	   (domMonadic u && 
	    let (_, y) = findMonadic u in
	      I.equal x y) ||
	   (domEqual u && 
	    let (y, z) = findEqual u in
	      I.equal x y || I.equal x z))
    in
    let disjoint () = 
      forallMonadic 
	(fun u _ -> 
	   (forallEqual (fun v _ -> not(Propvar.equal u v))))
    in
      check "depWF" depWF &&
      check "disjoint" disjoint

  module Trace = struct
    let debug = ref true
    open Format
    let varPrint = Var.pp err_formatter
    let pvarPrint = Propvar.pp err_formatter
    let applyPrint = Apply.pp err_formatter 
    let aliasEqual u x y = if !debug then (eprintf "\nAlias(R): "; pvarPrint u; eprintf " |->"; varPrint x; eprintf " = "; varPrint y; eprintf "@?")
    let aliasMonadic u p y = if !debug then (eprintf "\nAlias(R): "; pvarPrint u; eprintf " |->"; applyPrint (p, y); eprintf "@?")
    let propEq x y= if !debug then (eprintf "\nProp(R): "; varPrint x; eprintf " = "; varPrint y; eprintf "@?")
    let propDeq x y = if !debug then (eprintf "\nProp(R): "; varPrint x; eprintf " <> "; varPrint y; eprintf "@?")
    let propValid0 u = if !debug then (eprintf "\nProp(R): "; pvarPrint u; eprintf "@?")
    let propUnsat0 u = if !debug then (eprintf "\nProp(R): ~"; pvarPrint u; eprintf "@?")
    let propValid1 p x = if !debug then (eprintf "\nProp(R): "; applyPrint (p, x); eprintf "@?")
    let propUnsat1 p x = if !debug then (eprintf "\nProp(R): ~"; applyPrint (p, x); eprintf "@?")
  end
	   
  let initialize s =
    init := s;
    Config.Monadic.initialize s.monadic;
    Config.Equal.initialize s.equal;
    Config.Dep.initialize s.dep

  let reset () = 
    initialize empty
      
  let unchanged () = 
    Config.Monadic.unchanged() &&
    Config.Equal.unchanged() &&
    Config.Dep.unchanged()

  let is_empty () = 
    Monadic.is_empty (Config.Monadic.current()) &&
    Equal.is_empty (Config.Equal.current())
      
  let current () = 
    if unchanged () then !init else {
      monadic = Config.Monadic.current();
      equal = Config.Equal.current();
      dep = Config.Dep.current()
    }

  exception Found of propvar
  let invMonadic p x = 
    try
      Propvars.iter
	(fun v -> 
	   assert(dom v);
	   try
	     let q, y = Config.Monadic.find v in
	       if Sym.sub q p  && I.equal x y then 
		 raise(Found(v))
	   with
	       Not_found -> ())
	(deps x);
      raise Not_found
    with
	Found(v) -> v

  let invEqual x y =
    try
      Propvars.iter
	(fun v -> 
	   assert(dom v);
	   try
	     let z1,z2 = Config.Equal.find v in
	       if (I.equal x z1 && I.equal y z2) ||
		 (I.equal x z2 && I.equal y z1)
	       then
		 raise(Found(v))
	   with
	       Not_found -> ())
	(deps x);
	raise Not_found
    with
	Found(v) -> v
      
  let aliasMonadic p x =   
    assert(synchronized());
    try invMonadic p x with Not_found -> 
      let x = I.find x in
      let u = Propvar.fresh() in
	assert(Trace.aliasMonadic u p x; true);
	Config.Monadic.set u (p, x);
	Config.Dep.set x (Propvars.singleton u);
	assert(well_formed());
	u

  let rec aliasEqual x y = 
    assert(synchronized());
    try invEqual x y with Not_found -> 
      let x = I.find x and y = I.find y in
      let u = Propvar.fresh() in
	assert(Trace.aliasEqual u x y; true);
	Config.Equal.set u (x, y);
	addDep u x;
	addDep u y;
	assert(well_formed());
	u

  and addDep u x = 
    assert(I.canonical x);
    let vs = deps x in 
      if Propvars.mem u vs then () else
	let vs' = Propvars.copy vs in
	  Propvars.add u vs';
	  Config.Dep.set x vs'

  let rec propagateEq x y =  
    assert(Trace.propEq x y; true);
    assert(I.equal x y);
    assert(I.canonical y);
    assert(not(I.canonical x));
    deduceMonadic x y;
    deduceEqual x y;
    mergeDeps x y;
    assert(well_formed())

  and mergeDeps x y = 
    assert(I.equal x y);
    assert(I.canonical y); 
    assert(not(I.canonical x));
    try
      let us = Config.Dep.find x in
	Config.Dep.remove x;
	(try
	   let vs = Config.Dep.find y in
	   let vs' = Propvars.copy vs in
	     Propvars.union us vs';
	     Config.Dep.set y vs'
	 with
	     Not_found -> Config.Dep.set y us)
	
    with
	Not_found -> ()

  and deduceMonadic x y = 
    try 
      let us = Config.Dep.find x in
      let vs = Config.Dep.find y in
	Propvars.iter
	  (fun u -> 
	     assert(dom u);
	     try
	       let p, y = Config.Monadic.find u in           (* [u |-> p(y)] *)
		 Propvars.iter 
		   (fun v -> 
		      assert(dom v);
		      try
			if Propvar.equal u v then () else
			  let q,z = Config.Monadic.find v in (* [v |-> q(z)].*)
			    assert(I.equal y z);
			    if Sym.equal p q then I.equiv u v else 
			      if Sym.disjoint p q then I.disjoint u v else 
				if Sym.sub p q then I.implies u v else
				  if Sym.sub q p then I.implies v u
		      with
			  Not_found -> ())
		   vs
	     with
		 Not_found -> ())
	  us
    with
	Not_found -> ()

  and deduceEqual x y = 
    try 
      let us = Config.Dep.find x in
      let vs = Config.Dep.find y in
	Propvars.iter
	  (fun u -> 
	     assert(dom u);
	     try
	       let x1,x2 = Config.Equal.find u in             (* [u|->x1=x2] *)
		 if I.equal x1 x2 then I.valid0 u;
		 Propvars.iter 
		   (fun v -> 
		      assert(dom v);
		      try
			if Propvar.equal u v then () else
			  let y1,y2 = Config.Equal.find v in (* [v|->y1=y2]. *)
			    if I.equal y1 y2 then I.valid0 v;
			    if (I.equal x1 y1 && I.equal x2 y2) ||
			      (I.equal x1 y2 && I.equal x2 y2) then
				I.equiv u v
		      with
			  Not_found -> ())
		   vs
	     with
		 Not_found -> ())
	  us
    with
	Not_found -> ()

  let propagateDeq x y =  
    assert(Trace.propDeq x y; true);
    assert(I.diseq x y);
    Propvars.iter
      (fun u -> 
	 assert(dom u); 
	 try
	   let z1,z2 = Config.Equal.find u in    (* [u |-> z1 = z2] *)
	     if (I.equal x z1 && I.equal y z2) ||
	       (I.equal x z2 && I.equal y z1) 
	     then
	       I.unsat0 u
	 with
	     Not_found -> ())
      (deps x)

  let propagateValid0 u = 
    assert(Trace.propValid0 u; true);
    try 
      let p,x = Config.Monadic.find u in
	I.valid1 p (I.find x)
    with
	Not_found ->
	  try
	    let x,y = Config.Equal.find u in
	      if not(I.equal x y) then
		I.union x y
	  with
	      Not_found -> ()

  let propagateUnsat0 u = 
    assert(Trace.propUnsat0 u; true);
    try 
      let p,x = Config.Monadic.find u in
	I.unsat1 p (I.find x)
    with
	Not_found ->
	  try
	    let x,y = Config.Equal.find u in
	      I.separate x y
	  with
	      Not_found -> ()

  let propagateValid1 p x = 
    assert(Trace.propValid1 p x; true);
    try 
      let u = invMonadic p x in
	I.valid0 u
    with
	Not_found -> ()

  let propagateUnsat1 p x =
    assert(Trace.propUnsat1 p x; true);
    try 
      let u = invMonadic p x in
	I.unsat0 u
    with
	Not_found -> ()
end

