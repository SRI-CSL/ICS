(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.1 (the ``License''); you may not use this file except in
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

module type TERM = sig
  type var
  type t
  val equal : t -> t -> bool
  val diseq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
  val is_var : t -> bool
  val of_var : var -> t
  val to_var : t -> var
  val iter : (var -> unit) -> t -> unit
  val map : (var -> t) -> t -> t
  val for_all : (var -> bool) -> t -> bool
  val occurs : var -> t -> bool
  val choose : t -> var
  module Subst : Subst.S with type var = var and type trm = t
  exception Unsat
  val solve : t -> t -> Subst.t
end

module type V = sig
  type var
  val find : var -> var
  val canonical : var -> bool
  val equal : var -> var -> bool
  val diseq : var -> var -> bool
  val union : var -> var -> unit
  val separate : var -> var -> unit
end

module type INFSYS = sig
  type var
  type trm
  type t   
  module Subst : (Subst.S with type var = var and type trm = trm)
  val config : unit -> Subst.t
  val pp : Format.formatter -> unit
  val can : trm -> trm
  val find : var -> trm
  val inv : trm -> var
  val dom : var -> bool
  val cod : var -> bool 
  val local : var -> bool
  val empty : t  
  val is_empty : unit -> bool 
  val unchanged : unit -> bool
  val initialize : t -> unit
  val reset : unit -> unit
  val current : unit -> t
  val canonical : trm -> bool
  val diseq : var -> var -> bool
  val processEq : trm -> trm -> unit
  val alias : trm -> var
  val propagate : var -> var -> unit
  val confluent : unit -> bool
  val normalize : unit -> unit
  val close : unit -> unit
end

module Make
  (Var: VAR)
  (Term: TERM with type var = Var.t)
  (V: V with type var = Var.t) = 
struct
  type var = Var.t
  type trm = Term.t
      
  module Subst = Subst.Make(Var)(Term)   
  module Locals = Sets.Make(Var)
  module Dep = Powermaps.Make(Var)
    
  type t = {
    subst : Subst.t;
    dep : Dep.t;
    locals : Locals.t;
  }
      
  let empty = {
    subst = Subst.empty();
    dep = Dep.empty();
    locals = Locals.empty();
  }
		  
  let init = ref empty
	       
  module Config = struct
    module Subst = Config.Subst(Subst)
    module Dep = Config.Powermap(Dep)
    module Locals = Config.Set(Locals)
  end

  let config = Config.Subst.current

  let pp fmt = Subst.pp fmt (Config.Subst.current())

  let is_local x = 
    Term.is_var x && Locals.mem (Term.to_var x) (Config.Locals.current())

  let wfBinding x t = 
    let rho = Config.Subst.current() in
      not(is_local (Term.of_var x)) &&
      Term.for_all (fun x -> not(Subst.dom x rho)) t

  let confluent () = 
    Config.Subst.for_all
      (fun x t -> 
	 V.canonical x &&
	 Term.for_all V.canonical t)

  let rec well_formed () = 
    let debug = ref true in
    let check name pred = 
      if not !debug then pred() else 
	pred() ||
	(Format.eprintf "\nERROR: well_formedness check %s failed. 
                           Current state: \n" name;
	 pp Format.err_formatter;
	 Format.eprintf "\n@?";
	 false)
    in
    check "isSolved" isSolved &&
    check "isInverseFunctional" isInverseFunctional &&
    check "depWF" depWF 

  and isSolved () = 
    let rho = Config.Subst.current() in
    Subst.for_all wfBinding rho

  and isInverseFunctional () = 
    let for_all p = Subst.for_all p (Config.Subst.current()) in
      for_all (fun x t -> 
	(for_all (fun y s -> 
	   Var.equal x y || not(Term.equal s t))))

  and depWF () =
    let rho = Config.Subst.current() in
    let dep = Config.Dep.current() in
      Dep.for_all 
	(fun y dy -> 
	   (Dep.Values.for_all
	      (fun x -> 
		 Subst.dom x rho &&
		 Term.occurs y (Subst.lookup rho x))
	     dy))
	dep
		     
  let initialize s =
    init := s; 
    Config.Subst.initialize s.subst;
    Config.Dep.initialize s.dep;
    Config.Locals.initialize s.locals;
    assert(well_formed());
    assert(confluent())
	
  let unchanged = Config.Subst.unchanged
		    
  let current () =
    assert(confluent());
    if unchanged() then !init else { 
      subst = Config.Subst.current(); 
      dep = Config.Dep.current(); 
      locals = Config.Locals.current() 
    }
      
  let reset () = initialize empty
		   
  let dom x = Subst.dom x (Config.Subst.current())
  let cod y = Dep.mem y (Config.Dep.current())
  let local y = Locals.mem y (Config.Locals.current())
  let occ x = dom x || cod x
  let is_empty () = Subst.is_empty (Config.Subst.current())
  let find x = Subst.lookup (Config.Subst.current()) x
  let deps x = Dep.find x (Config.Dep.current())

  let canonical = 
    let independent x = 
      not(dom x) && 
      V.canonical x 
    in
      Term.for_all independent
		 
  let can t =
    let applyVar x = 
      let findv y = 
	let y' = V.find y in
	  if Var.equal y y' then raise Not_found else Term.of_var y'
      in
	try
	  let t = find x in
	  let t' = Term.map findv t in
	    assert(canonical t');
	    t'
	with
	    Not_found -> Term.of_var (V.find x)
    in
    let s = Term.map applyVar t in
      assert(canonical s);
      s

  exception Ground
  let chooseVar t = 
    try Term.choose t with Not_found -> raise Ground
		      
  (** For nonground terms, dependency index is used to compute inverse lookup. *)
  exception Found of var
  let rec inv t = 
    try
      let y = chooseVar t in
	invNonGround y t
    with
	Ground -> Subst.inv (Config.Subst.current()) t

  and invNonGround y t = 
    assert(Term.occurs y t);
    let checkInv x = 
      assert(dom x);
      if Term.equal t (find x) then raise(Found(x))
    in
      try
	Dep.Values.iter checkInv (deps y);
	raise Not_found
      with
	  Found(x) -> x

  let restrict x =
    assert(well_formed());
    try
      let t = find x in
	Config.Subst.remove x;
	Term.iter (Config.Dep.rem x) t;
	assert(well_formed());
    with
	Not_found -> ()
	    
  let extend x t = 
    try
      let y = inv t in
	V.union x y
    with
	Not_found -> 
	  Config.Subst.update x t;
	  Term.iter (Config.Dep.add x) t;
	  assert(well_formed())

  let replace u t t' = 
    assert(dom u);
    assert(Term.equal t (find u));
    assert(Term.for_all (fun x -> not(dom x)) t');
    assert(well_formed());
    if Term.is_var t' && not(is_local t') then
      let v = Term.to_var t' in
	restrict u; V.union u v;
	assert(well_formed())
    else 
      try
	let v = inv t' in
	  restrict u; V.union u v;
	  assert(well_formed())
      with
	  Not_found -> 
	    Config.Subst.update u t';
	    Term.iter (fun y -> if not(Term.occurs y t') then Config.Dep.rem u y) t;
	    Term.iter (Config.Dep.add u) t';
	    assert(well_formed())

  let getScopus acc rho = 
    Stacks.clear acc;
    let add1 y = 
      if Stacks.mem Var.equal y acc then () else
	Stacks.push y acc
    in
    let add x _ = 
      Dep.Values.iter add1 (deps x) 
    in
      Term.Subst.iter add rho
			 
  let fuse =
    let scopus = Stacks.create () in
      fun rho -> 
	let fuse1 x = 
	  assert(dom x);
	  let s = find x in
	  let s' = Term.Subst.apply rho s in
	    replace x s s'
	in
	  getScopus scopus rho;
	  while not(Stacks.is_empty scopus) do
	    fuse1 (Stacks.pop scopus)
	  done
	   
  let compose rho =
    assert(well_formed());
    fuse rho;
    Term.Subst.iter extend rho

  let diseq x y = 
    assert(confluent());
    try
      let s = find x and t = find y in
        Term.diseq s t
    with
	Not_found -> false
	  
  let alias t = 
    assert(confluent());
    if Term.is_var t then V.find (Term.to_var t) else
      let t = can t in
	assert(canonical t);
	try V.find (inv t) with Not_found -> 
	  let v = Var.fresh () in
	    extend v t;
	    v
	  
  let rec processEq s t =
    assert(confluent());
    let s = can s and t = can t in
      if Term.equal s t then () else
	if Term.diseq s t then raise Term.Unsat else
	  addEq s t

  and addEq s t = 
    let rho = Term.solve s t in
    let installFresh _ r = 
      let install x = 
	if not(Term.occurs x s) && not(Term.occurs x t) then
	  Config.Locals.add x
      in
	Term.iter install r
    in
      Term.Subst.iter installFresh rho;
      compose rho
	    
  let rec propagate x y =
    assert(not(V.canonical x));
    assert(V.canonical y);
    assert(V.equal x y);
    try
      let s = find x in
	restrict x;
	(try
	  let t = find y in
	    if not(Term.equal s t) then 
	      addEq s t
	with
	    Not_found -> addEq (Term.of_var y) s)
    with
	Not_found -> 
	  let t = try find y with Not_found -> Term.of_var y in
	    fuse (Term.Subst.singleton x t)

  let normalize () =
    assert(confluent());
    Locals.iter
      (fun z -> 
	 assert(not(dom z));
	 if not(cod z) then
	   Config.Locals.remove z)
      (Config.Locals.current())
	   
  let close () = 
    let rho = Config.Subst.current() in
      Subst.iter
	(fun u _ -> 
	   Subst.iter 
	     (fun v _ -> 
		if not(Var.equal u v) && 
		  not(V.diseq u v) && 
		  diseq u v &&
		  not(V.diseq u v)
		then 
		  V.separate u v)     
	   rho)
	rho
end
