(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module type VAR = sig
  type t
  val equal : t -> t -> bool 
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val fresh : unit -> t
end

module type FLAT  = sig
  type var
  type t = private   
	   | Update of var * var * var
	   | Lookup of var * var
  val update : var -> var -> var -> t
  val lookup : var -> var -> t  
  val map : (var -> var) -> t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end 

module Flat(Var: VAR) = struct
  type var = Var.t
  type t = 
    | Update of Var.t * Var.t * Var.t
    | Lookup of Var.t * Var.t
  let update a i x = Update(a, i, x)
  let lookup a i = Lookup(a, i)
  let map f = 
    let rec mapf t = 
      match t with
	| Update(a, i, x) ->
	    let a' = f a and i' = f i and x' = f x in
	      if a' == a && i' == i && x' == x then t else
		update a' i' x'
	| Lookup(a, i) -> 
	    let a' = f a and i' = f i in
	      if a' == a && i' == i then t else
		lookup a' i'
    in
      mapf 
  let hash = function
    | Update(a, i, x) ->
	(Var.hash a + Var.hash i + Var.hash x) land 0x3FFFFFFF
    | Lookup(a, i) -> 
	(Var.hash a + Var.hash i) land 0x3FFFFFFF
  let equal s t = 
    match s, t with
      | Update(a, i, x), Update(b, j, y) -> 
	  Var.equal a b && Var.equal i j && Var.equal x y
      | Lookup(a, i), Lookup(b, j) ->
	  Var.equal a b && Var.equal i j
      | _ -> 
	  false
  let compare s t = 
    match s, t with
      | Update(a, i, x), Update(b, j, y) -> 
	  let c = Var.compare a b in
	    if c <> 0 then c else
	      let d = Var.compare i j in
		if d <> 0 then d else Var.compare x y
      | Lookup(a, i), Lookup(b, j) -> 
	  let c = Var.compare a b in
	    if c <> 0 then c else Var.compare i j
      | Update _, Lookup _ -> 1
      | Lookup _, Update _ -> -1
  let rec pp fmt = function
    | Update(a, i, x) -> ppUpdate fmt a i x
      | Lookup(a, i) -> ppLookup fmt a i
  and ppUpdate fmt a i x = 
    Format.fprintf fmt "@[";
    Var.pp fmt a;
    Format.fprintf fmt "[";
    Var.pp fmt i;
    Format.fprintf fmt ":=";
    Var.pp fmt x;
    Format.fprintf fmt "]@]@?"
  and ppLookup fmt a i = 
    Format.fprintf fmt "@["; 
    Var.pp fmt a;
    Format.fprintf fmt "[";
    Var.pp fmt i;
    Format.fprintf fmt "]@]@?"
end 

module type INTERFACE = sig
  type var
  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool 
  val chooseEquiv : (var -> bool) -> var -> var
  val iterEquiv : (var -> unit) -> var -> unit
  val iterDiseqs : (var -> var -> unit) -> var -> unit
  val union : var -> var -> unit 
  val ite : var * var -> var * var -> var * var -> unit
  val array : var -> unit
end

module type INFSYS = sig
  type var  
  type flat
  type t  
  val empty : t
  val initialize : t -> unit
  val reset : unit -> unit
  val unchanged : unit -> bool
  val current : unit -> t
  val pp : unit -> unit
  module Cfg : (Maps.S with type key = var and type value = flat)
  val config : unit -> Cfg.t
  val find : var -> flat
  val inv: flat -> var
  val alias : flat -> var
  val propagateEq : var -> var -> unit
  val propagateDeq : var -> var -> unit
end

module Make
  (Var: VAR)
  (Flat: FLAT with type var = Var.t)
  (V: INTERFACE with type var = Var.t) = 
struct
  
  type var = Var.t
  type flat = Flat.t
      
  module Find = Maps.Make(Var)(Flat)
  module Dep =  Powermaps.Make(Var)
  module Deps = Dep.Values

  type t = { find : Find.t; dep : Dep.t }

  let empty = { find = Find.empty(); dep = Dep.empty() }

  let init = ref empty

  module Config = struct
    module Find = Config.Map(Find)
    module Dep = Config.Powermap(Dep)
  end

  let pp () = 
    let fmt = Format.std_formatter in
      Format.fprintf fmt "@[\nfind: "; Find.pp fmt (Config.Find.current());
      Format.fprintf fmt "@[\ndep: "; Dep.pp fmt (Config.Dep.current());
      Format.fprintf fmt "@]@?"

  let dom x = Config.Find.mem x

  let lookup = Config.Find.find

  let well_formed () = 
    let depOk j u = 
      dom u &&
      (match lookup u with
	 | Flat.Update(_, i, _) -> V.equal i j
	 | Flat.Lookup(_, i) -> V.equal i j)
    in
    let depOkAll j us = Dep.Values.for_all (depOk j) us in
      Dep.for_all depOkAll (Config.Dep.current())

  module Cfg = Find
  let config = Config.Find.current

  let initialize s =
    init := s;
    Config.Find.initialize s.find;
    Config.Dep.initialize s.dep

  let reset () = initialize empty

  let unchanged () = 
    Config.Find.unchanged()

  let current() = 
    if unchanged() then !init else {
      find = Config.Find.current();
      dep = Config.Dep.current();
    }

  let find u = 
    let u' = V.chooseEquiv dom u in
      Config.Find.find u'

  exception FoundLookup of var * var
  let findLookup u = 
    let choose v = 
      assert(V.equal u v);
      try 
	(match Config.Find.find v with
	   | Flat.Lookup(a, i) -> raise (FoundLookup(a, i))
	   | _ -> ())
      with
	  Not_found -> ()
      
    in
      try 
	V.iterEquiv choose u; 
	raise Not_found 
      with FoundLookup(a,i) -> (a, i)


  exception FoundUpdate of var * var * var
  let findUpdate u = 
    let choose v = 
      assert(V.equal u v);
      try 
	(match Config.Find.find v with
	   | Flat.Update(a, i, x) -> raise (FoundUpdate(a, i, x))
	   | _ -> ())
      with
	  Not_found -> ()
      
    in
      try 
	V.iterEquiv choose u; 
	raise Not_found 
      with FoundUpdate(a,i,x) -> (a, i, x)

  let memUpdate u a i x = 
    try
      let a',i',x' = findUpdate u in
	V.equal a a' && V.equal i i' && V.equal x x'
    with
	Not_found -> false

  let memLookup u a i = 
    try
      let a', i' = findLookup u in
	 V.equal a a' && V.equal i i'
    with
	Not_found -> false

  let dep y = Config.Dep.find (V.find y)

  let equalFlat s t = 
    match s, t with
      | Flat.Update(a, i, x), Flat.Update(b, j, y) -> 
	  V.equal a b && V.equal i j && V.equal x y
      | Flat.Lookup(a, i), Flat.Lookup(b, j) -> 
	  V.equal a b && V.equal i j
      | _ -> 
	  false

  let rec inv = function
    | Flat.Update(a, i, x) -> invUpdate a i x
    | Flat.Lookup(a, i) -> invLookup a i

  and invUpdate a i x = 
    let reprUpdate u = 
      assert(dom u);
      match lookup u with
	| Flat.Update(a',i',x') -> 
	    V.equal a a' && V.equal i i' && V.equal x x'
	| _ -> false
    in
      Dep.Values.choose_if reprUpdate (dep i)

  and invLookup a i = 
    let reprLookup u = 
      assert(dom u);
      match lookup u with
	| Flat.Lookup(a',i') -> 
	    V.equal a a' && V.equal i i'
	| _ -> false
    in
      Dep.Values.choose_if reprLookup (dep i)

  let iterateUpdateIndex i f = 
    let i = V.find i in
    let applyUpdate u =
      try
	(match lookup u with
	  | Flat.Update(a, j, x) when V.equal i j -> 
	      f u (V.find a) i (V.find x)
	  | _ -> ())
      with
	  Not_found -> ()
    in
      Dep.Values.iter applyUpdate (dep i)

  let debug = ref false

  module Trace = struct
    let alias x t = 
      (if !debug then
	 let stderr = Format.err_formatter in
	 let varpp = Var.pp Format.err_formatter in
	   Format.eprintf "\nAlias(F): ";
	   Var.pp stderr x; Format.eprintf " = "; 
	   Flat.pp stderr t;
	   Format.eprintf "@?");
      true
	
    let split i j = 
      (if !debug then
	 let varpp = Var.pp Format.err_formatter in
	   Format.eprintf "\nSplit(F) on ";
	   varpp i; Format.eprintf " <> "; varpp j; Format.eprintf "@?");
      true
	
    let deduce x y = 
      (if !debug then
	 let varpp = Var.pp Format.err_formatter in
	   Format.eprintf "\nDeduce(F): ";
	   varpp x; Format.eprintf " = "; varpp y; Format.eprintf "@?");
      true
  end

  let deduce x y =
    assert(Trace.deduce x y);
    V.union x y

  let depAdd u = function
    | Flat.Update(a, i, x) ->	
	Config.Dep.add u a;
	Config.Dep.add u i;
	Config.Dep.add u x;
    | Flat.Lookup(a, i) ->  
	Config.Dep.add u a;
	Config.Dep.add u i

  let rec alias t =
    let x = match t with
      | Flat.Update(a, i, x) -> aliasUpdate a i x
      | Flat.Lookup(a, i) -> aliasLookup a i
    in
      assert(Trace.alias x t);
      x

  and aliasUpdate a i x = 
    let a = V.find a and i = V.find i and x = V.find x in
      try invUpdate a i x with Not_found -> 
	let u = Var.fresh() in
	let t = Flat.update a i x in
	  Config.Find.set u t;
	  depAdd u t;   
	  chainOnUpdate u a i x;
	  splitOnUpdate u a i x;
	  V.array u;
	  V.array a;
	  u       

  and aliasLookup a i = 
    let a = V.find a and i = V.find i in
      try invLookup a i with Not_found -> 
	let u = Var.fresh() in
	let t = Flat.lookup a i in
	  Config.Find.set u t;
	  depAdd u t;
	  splitOnLookup u a i;
	  V.array a;
	  u  

  (** From [u = a[i:=x]], [v = b[j]], [u = b] derive
    [if i = j then v = x else v = a[j]]. *)
  and splitOnUpdate u a i x =
    assert(memUpdate u a i x);
    let split v = function
      | Flat.Lookup(b, j) when V.equal u b -> 
	  assert(memLookup v u j);
	    doSplit i j v x a
      | _ -> ()
    in
      Find.iter split (Config.Find.current())

  (** From [v = b[j]], [u = a[i:=x]], [u = b] derive
      [if i = j then v = x else v = a[j]]. *)
  and splitOnLookup v b j = 
    assert(memLookup v b j);
    let split u = 
      try
	(match lookup u with
	   | Flat.Update(a, i, x) ->
	       assert(memUpdate u a i x);
	       doSplit i j v x a
	   | _ -> ())
      with
	  Not_found -> ()
    in
      V.iterEquiv split b

  and doSplit i j v x a =
    if V.equal i j then deduce v x else
      if V.diseq i j then deduce v (aliasLookup a j) else
	begin
	  assert(Trace.split i j);
	  V.ite (i, j) (v, x) (v, aliasLookup a j)
	end

  (** Forward chainining on new updates [u = a[i:=x]]. *)
  and chainOnUpdate u a i x = 
    chainOnUpdate1 u a i x;
    chainOnUpdate2 u a i x;
    chainOnUpdate3one u a i x;
    chainOnUpdate3two u a i x;
    chainOnUpdate4one u a i x;
    chainOnUpdate4two u a i x
  
  (** [u = a[i:=x]] ==> [x = u[i]] *)
  and chainOnUpdate1 u a i x =
    assert(memUpdate u a i x);
    deduce x (aliasLookup u i)

  (** [u = a[i:=x]], [i' <> j], [i = i'] ==> [u[j] = a[j]] *)
  and chainOnUpdate2 u a i x = 
    assert(memUpdate u a i x);
    let chainDiseq  i' j = 
      assert(V.diseq i' j); 
      assert(V.equal i i');
      deduce (aliasLookup u j) (aliasLookup a j)
    in
      V.iterDiseqs chainDiseq i

  (** [u = a[i:=x]], [i<>j], [v = u'[j:=y]], [u = u'] ==> [v = a[j:=y][i:=x]] *)
  and chainOnUpdate3one u a i x = 
    assert(memUpdate u a i x);
    let chain3 i' j = 
      assert(V.diseq i' j);
      assert(V.equal i i');
      iterateUpdateIndex j
	(fun v u' j' y -> 
	   assert(V.equal j j');
	   if Var.equal u u' then
	     deduce v (aliasUpdate (aliasUpdate a j y) i x))
    in
      V.iterDiseqs chain3 i

  (** [v = u[j:=y]],[j <> i],[u'=a[i':=x]],[u=u'],[i=i']==>[v=a[j:=y][i:=x]] *)
  and chainOnUpdate3two v u j y = 
    assert(memUpdate v u j y);
    let chain3 j i = 
      assert(V.diseq j i);
      iterateUpdateIndex i
	(fun u' a i' x -> 
	   assert(memUpdate u' a i' x);
	   assert(V.equal i i');
	   if Var.equal u u' then
	     deduce v (aliasUpdate (aliasUpdate a j y) i x))
    in
      V.iterDiseqs chain3 j

  (** [v = a[j := y]], [u = v'[i:=x]], [v = v'], [i = j] ==> [u = a[i:=x]] *)
  and chainOnUpdate4one v a j y = 
    assert(memUpdate v a j y);
    let chain4 u = 
      assert(dom u);
      if not(Var.equal u v) then
	match lookup u with
	  | Flat.Update(v', i, x) when V.equal v v' -> 
	      if V.equal i j then  (* should always hold. *)
		deduce u (aliasUpdate a i x)
	  | _ -> ()
    in
      Dep.Values.iter chain4 (dep j)

  (** [u = v[i:=x]], [v' = a[i' := y]], [v = v'], [i = i'] ==> [u = a[i:=x]] *)
  and chainOnUpdate4two u v i x = 
    assert(memUpdate u v i x);
    let chain4 v' = 
      assert(dom v');
      if not(Var.equal u v') then
	match lookup v' with
	  | Flat.Update(a, i', y) when V.equal v v' && V.equal i i' -> 
	      deduce u (aliasUpdate a i x)
	  | _ -> ()
    in
      Dep.Values.iter chain4 (dep i)

  (** Forward chain: [i<>j], [u = a[i':=x]], [i = i'] ==> [u[j] = a[j]] *)
  let chainOnDiseq2 i j =
    assert(V.diseq i j);
    let deduceFrom u a i' x = 
      assert(memUpdate u a i' x);
      assert(V.equal i i');
      deduce (aliasLookup u j) (aliasLookup a j)
    in
      iterateUpdateIndex i deduceFrom

  (** [i<>j], [u=a[i:=x]], [v=u[j:=y]] ==> [v=a[j:=y][i:=x]] *)
  let chainOnDiseq3 i j = 
    assert(V.diseq i j);
    iterateUpdateIndex i
      (fun u a i x -> 
	 assert(memUpdate u a i x);
	 (iterateUpdateIndex j
	    (fun v u' j y -> 
	       assert(memUpdate v u' j y);
	       if  not(Var.equal u v) && V.equal u u' then
		 deduce v (aliasUpdate (aliasUpdate a j y) i x))))

  (** [i=j], [u=v'[i:=x]], [v = a[j := y]], [v'=v]==> [u = a[i:=x]] *)
  let chainOnEqual4 i j = 
    assert(V.equal i j);
    iterateUpdateIndex i
      (fun u v' i' x -> 
	 assert(V.equal i i');
	 assert(memUpdate u v' i' x);
	 (iterateUpdateIndex j
	    (fun v a j' y ->
	       assert(V.equal j j');
	       assert(memUpdate v a j y);
	       if not(Var.equal u v) && V.equal v v' then
		 deduce u (aliasUpdate a i x))))

  let restrict u = 
    try
      (match find u with
	 | Flat.Update(a, i, x) -> 
	     Config.Dep.rem u a;
	     Config.Dep.rem u i;
	     Config.Dep.rem u x;
	 | Flat.Lookup(a, i) -> 
	     Config.Dep.rem u a;
	     Config.Dep.rem u i);
      Config.Find.remove u
    with
	Not_found -> ()

  let congClose x y = 
    let du = Config.Dep.find x and dv = Config.Dep.find y in
      Dep.Values.iter 
	(fun u -> 
	   try
	     let s = lookup u in
	       Dep.Values.iter
		 (fun v -> 
		    if not(Var.equal u v) then
		      try
			let t = lookup v in
			  if equalFlat s t then
			    (restrict u; 
			     deduce u v)
		      with
			  Not_found -> ())
		 dv
	   with
	       Not_found -> ())
	du

  (** [u = b], [v = b[j]], [u = a[i:=x]] derive
      [if i = j then v = x else v = a[j]]. *)
  let splitOnEqual u b = 
    assert(V.equal u b);
    let db = dep b in
      Dep.Values.iter
	(fun v -> 
	   (try
	      let b', j = findLookup v in
		if V.equal b b' then 
		  let trySplit u' = 
		    assert(V.equal u u');
		    try
		      (match Config.Find.find u' with
			 | Flat.Update(a,i,x) -> doSplit i j v x (aliasLookup a j)
			 | Flat.Lookup _ -> ())
		    with
			Not_found -> ()
		  in
		    V.iterEquiv trySplit u
	    with
		Not_found -> ()))
	db
   
  let propagateEq x y =
    assert(V.equal x y);
    assert(V.canonical y);
    assert(not(V.canonical x));
    congClose x y;        (* needs to be called before merging dependencies. *)
    Config.Dep.merge x y;
    splitOnEqual x y;
    chainOnEqual4 x y

  let propagateDeq x y =
    assert(V.diseq x y);
    assert(V.canonical x);
    assert(V.canonical y);
    chainOnDiseq2 x y;
    chainOnDiseq2 y x;
    chainOnDiseq3 x y;
    chainOnDiseq3 y x  
end
