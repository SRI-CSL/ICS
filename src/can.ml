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

type iter = (Judgement.equal -> unit) -> unit

(** A {i canonizable} theory [th] is specified by means of a
  - canonizer [can],
  - forward chaining rules [on_vareq], [on_vardiseq], [on_flateq], and
  - a branching function [disjunction]. *)
module type T = sig
  val th : Theory.t
  val can : Funsym.t -> Term.Args.t -> Term.t
  val chains : Axioms.Chain.t list
  val disjunction : iter -> Judgement.disjunction
end

let is_flat a =
  not (Term.is_var a) &&
  Term.Args.for_all Term.is_var (Term.args_of a)

module J = Judgement

(** Representation of a set of equalities [{x{1} = a{1},...,x{n} = a{n}}] with
  distinct [a{i}] {i flat terms} but the [x{i}] are not necessarily pairwise disjoint. *)
module Config = struct

  exception Found
 
  module Map = Term.Map
  module Set = Term.Set
    
  (** Bindings [x |-> {e{1},...,e{m}}] with [e{i} |- x{i} = t{i}] where [t{i}] is flat. *)
  type t = {
    mutable find : J.Equals.t Map.t;
    mutable dep : Dep.t;
  }

  let is_empty s = Map.is_empty s.find
		     
  let equalities s =
    let acc = J.Equals.empty() in
      Map.iter 
	(fun x es -> 
	   J.Equals.iter (fun e -> J.Equals.add e acc) es)
	s.find;
      acc
      
  let pp fmt s =
    J.Equals.pp fmt (equalities s);
    if (Version.debug() >= 1) then
      (Format.fprintf fmt "\ndep: "; Dep.pp fmt s.dep)
	
  let apply s x =
    let es = Map.find x s.find in
    let e = J.Equals.choose es in
    let t = e#rhs in
      t, e
	
  let find s x = 
    try apply s x with Not_found -> x, J.mk_refl x
      
  (** [parses t] returns set of terms equal to [t] such that [t] does not
    contain left-hand side variables.
    - [parses s x = {t{1},...,t{m}}] if [x = t{1}],...,[x = t{m}] in [s]. 
    - [parses s f(t{1},...,t{n}) = { f(a{1},...,a{n}) | a{i} in parses(t{i}) }] *)
  let parses s =
    let rec accumulate t =
      if Term.is_var t then
	try 
	  Map.find t s.find 
	with 
	    Not_found -> 
	      J.Equals.singleton (J.mk_refl t)
      else 
	let f = Term.sym_of t and a = Term.args_of t in
	  failwith "to do"
    in
      accumulate 
	
  let iter f s =
    let setiter _ es = J.Equals.iter f es in
      Map.iter setiter s.find
	
  let iter_on x f s = 
    Map.iter 
      (fun y es -> 
	 if Term.eq x y then 
	   J.Equals.iter f es
	 else
	   J.Equals.iter
	     (fun e -> 
		assert(Term.eq x e#lhs);
		if Term.occurs x e#rhs then f e)
	     es)
      s.find
	
  let fold f s acc0 =
    let acc = ref acc0 in
    let g e = acc := f e !acc in
      iter g s;
      !acc
	
  let mem x s =
    Map.mem x s.find

  exception Empty
      
  let dep s x =
    assert(Term.is_var x);
    try Dep.find s.dep x with Not_found -> raise Empty
      
  let occ x s =
    mem x s || Dep.mem x s.dep
  
  let in_dom s x = 
    assert(Term.is_var x);
    Map.mem x s.find
    
  let in_cod s x = 
    assert(Term.is_var x);
    Dep.mem x s.dep 

  exception FoundEqual of J.equal
      
  (** Return [(x, rho)] if [rho |- x = a] is in [s]. 
    If [a] is a constant, then traverse the data structure
    to find the inverse.  Otherwise, the dependency index
    on some variable [y] in [a] is used to find [x = a]. *)
  let rec inv s t =
    if is_flat t then
      try
	let x = Term.choose Term.is_var t in
	  inv_noncnstnt x s t
      with
	  Not_found -> inv_cnstnt s t
    else
      raise Not_found

  and inv_cnstnt s t =
    try
      iter 
	(fun e -> 
	   if Term.eq t e#rhs then
	     raise(FoundEqual(e)))
	s;
      raise Not_found
    with
	FoundEqual(e) -> e#lhs, e

  and inv_noncnstnt y s t =
    let xs = dep s y in
      try
	Dep.Set.iter
	  (fun x -> 
	     try
	       let es = Map.find x s.find in
		 J.Equals.iter
		   (fun e -> 
		      assert(Term.eq x e#lhs);
		      if Term.eq t e#rhs then
			raise(FoundEqual(e)))
		   es
	     with
		 Not_found -> invalid_arg "Imprecise dependency")
	  xs;
	raise Not_found
      with
	  FoundEqual(e) -> e#lhs, e
	
  let empty () = {
    find = Map.empty();
    dep = Dep.empty();
  }
		 	 
  let is_canonical v s =
    let is_canonical_var x = 
      Term.is_var x && V.Config.is_canonical v x 
    in
    let is_canonical_flat a =
      is_flat a && 
      Term.Args.for_all is_canonical_var (Term.args_of a)
    in
      Map.for_all
	(fun x es ->
	   is_canonical_var x && 
	   (J.Equals.for_all
	      (fun e -> 
		 Term.eq x e#lhs &&
		 is_canonical_var x &&
		 is_canonical_flat e#rhs)
	      es))
	s.find
	
  let interp s =
    fold 
      (fun e acc -> 
	 let x = e#lhs and t = e#rhs in
	 let f, a = Term.destruct t in
	   Term.Interp.update f a x acc)
      s Term.Interp.empty
      
  let model s =
    let id = Term.Assign.empty in
      (interp s, id)

  let copy s = {
    find = Term.Map.copy s.find;   
    dep = Dep.copy s.dep
  }

  let restrict s x =
    assert(Term.is_var x);
    try
      let es = Map.find x s.find in
	Map.remove x s.find;
	J.Equals.iter
	  (fun e -> 
	     let x' = e#lhs and t = e#rhs in
	       Term.iter (fun y -> Dep.remove y x' s.dep) t)
	  es
    with
	Not_found -> ()
    
  let extend s e =
    let x = e#lhs and t = e#rhs in
      assert(not(in_dom s x));
      Map.set x (J.Equals.singleton e) s.find;
      Term.iter (fun y -> Dep.add x y s.dep) t
    
end 

  
(** Facts deduced by forward chaining. *)
module Derived = struct
   
  let equals = Stacks.create ()
  let diseqs = Stacks.create ()
		 
  let init () =
    Stacks.clear equals;
    Stacks.clear diseqs
      
  let add_equal e = 
    Stacks.push e equals
      
  let add_diseq d = 
    Stacks.push d diseqs
      
  (** Process derived facts until completion. *)
  let process (process_equal, process_diseq) =
    let rec close () = 
      try
	let e = Stacks.pop equals in
	  process_equal e;
	  close ()
      with
	  Stacks.Empty -> 
	    (try
	       let d = Stacks.pop diseqs in
		 process_diseq d;
		 close ()
	     with
		 Stacks.Empty -> ())
    in
      close ()
	    
end

module type INFSYS =
sig
  val current : unit -> Config.t
  val finalize : unit -> Config.t
  val reset : unit -> unit
  val initialize : Config.t -> unit
  val is_unchanged : unit -> bool
  val can : Term.t -> Term.t * Judgement.equal
  val remove : Judgement.equal -> unit
  val abstract : Term.t -> Judgement.atom -> unit
  val process_equal : Judgement.equal -> unit
  val propagate_equal : Term.t -> unit
  val process_diseq : Judgement.equal -> unit
  val propagate_diseq : Judgement.diseq -> unit
  val branch : unit -> Judgement.disjunction option
  val normalize : unit -> unit
end

(** Inference system from a canonizable theory. *)
module Infsys(Can: T): INFSYS =  struct

  let th = Can.th

  (** A {i pure} term is built up with function symbols from theory [Can.th]. *)
  let is_pure = Term.is_pure th
    
  (** A {i flat} term is of the form [f(x{1},...,x{n})] with [f] a function symbol
    in theory [th] and the [x{i}] are variables. *)
  let is_flat t =
    not (Term.is_var t) &&
    ((Term.theory_of t = th) && 
     Term.Args.for_all Term.is_var (Term.args_of t)) 
      
  (** Current configuration. *)
  let s = ref (Config.empty())
  let changed = ref (Term.Set.empty())
  let unchanged = ref true

  let current () = !s

  let is_unchanged () = Term.Set.is_empty !changed

  let initialize s0 =
    changed := Term.Set.empty();
    unchanged := true;
    s := s0 

  let reset () = 
    initialize (Config.empty())
      
  let finalize () = 
    if is_unchanged () then !s else Config.copy !s

(*
  module Update = Flat.Update(
    struct
      let can = Can.can
      let map = Can.map
      let effect e =
	unchanged := false;
	ForwardChain.on_flateq e
    end)
*)

  let remove e =
    let x = e#lhs and a = e#rhs in
      assert(Term.is_var x);
      assert(is_flat a);
      try
	let es = Config.Map.find x !s.Config.find in
	  if J.Equals.mem e es then
	    begin
	      J.Equals.remove e es;
	      Config.Map.set x es !s.Config.find;
	      failwith "dep: To do"
	    end
      with
	  Not_found -> ()

  (** Flatten a pure term [a] and introduce variables as
    necessary for naming subterms. The result is a variable 
    [u] equal to [a] in the extended context. 
    - [can x = t]          if [(x = t) in s]
    - [can x = find(v)(x)] otherwise
    - [can(f(t)) = can(f(y))] if [(y = t) in s]
    - [can(f(t1,...,tn)) = v] otherwise.
    Here [y = can(t)], and [v = can(t)] extends [s]. *)
  let rec can t =
    assert(is_pure t);
    failwith "to do"
(*
    if Term.is_var t then canvar t else 
      let f = Term.sym_of t and a = Term.args_of t in
	canapp f a
    
  and canvar x =
    assert(Term.is_var x);
    try snd(Config.apply !s x) with Not_found -> 
      snd(V.Infsys.can x)

  and canapp t =
    assert(Term.is_app t);
    let f = Term.sym_of t and a = Term.args_of t in
    let b, el = canargs a in                (* [e{i} |- a{i} = b{i}] *)
      assert(Term.Args.for_all Term.is_var b); 
      try
	let y, e1 = inv t in                (* [e1 |- b = y]. *)
	  (*  let e2 = J.mk_cong f el in  (* [e2 |- f(a) = f(b)]. *) *)
	let e2 = failwith "to do" in
	let e3 = J.mk_replace_in_equal [e1] e2 in
	  y, e3
      with
	  Not_found -> 
	    let u = Term.mk_fresh_var "u" in
	      unchanged := false;
	      (* let e = J.mk_cong f el in  (* [e |- f(a) = f(b)]. *) *)
	      let e = failwith "to do" in 
		Config.extend e
		u, J.mk_refl u

  and canargs a =
    let el = ref [] in
    let b = 
      Term.Args.map
	(fun t -> 
	   let x, e = can t in
	     el := e :: !el; x)
	a;
    in
      b, List.rev !el
*)
      
  let abstract t atm =
    assert(is_pure t && not(Term.is_var t));
    let x, e = can t in                 (* [e |- t = x]. *)
      assert(Term.is_var x);
      failwith "To do"
(*
      let concl' = Atom.replace t x atm#concl in
      let atm' = J.mk_replace [e] atm in
	G.Infsys.put atm'
*)
      
  let process_equal1 e =                    (* [e |- t1 = t2]. *)
    let t1 = e#lhs and t2 = e#rhs in
      assert(is_pure t1 && is_pure t2);
      if Term.eq t1 t2 then () else
	let x, e1 = can t1 in           (* [e1 |- t1 = x]. *)
	let y, e2 = can t2 in           (* [e2 |- t2 = y]. *)
	  assert(Term.is_var x && Term.is_var y); 
	  if Term.eq x y then
	    let e' = J.mk_replace_in_equal [e1; e2] e in       
	      V.Infsys.process_equal e'

  let rec process_diseq1 d =
    let t1 = d#lhs and t2 = d#rhs in
    assert(is_pure t1 && is_pure t2);
    let x, e1 = can t1 in
    let y, e2 = can t2 in
      assert(Term.is_var x && Term.is_var y);
      let d' = J.mk_replace_in_diseq e2 e2 d in
	V.Infsys.process_diseq d'
   
  let toplevel f a =
    Derived.init ();
    f a;
    Derived.process (process_equal1, process_diseq1)

  let process_equal e = 
    toplevel process_equal1 e

  let process_diseq d = 
    toplevel process_diseq1 d

  let rec propagate_equal x =  
    toplevel propagate_equal1 x

  and propagate_equal1 x =
    assert(Term.is_var x);
    failwith "to do"
(*
    if occ x then
      let e = V.Infsys.find x in
	fuse e;
	collapse e;
	Can.Close.on_vareq e iter
*)

  and fuse e =
    failwith "inst: to do"

  and collapse e =
    let x = e#lhs and y = e#rhs in
      assert(Term.is_var x && Term.is_var y);
      try
	let ex = Config.Map.find x !s.Config.find 
	and ey = Config.Map.find y !s.Config.find in
	let exy = J.Equals.copy ex in  (* to do. *)
	  J.Equals.union ey exy;
	  unchanged := true;
	  Config.Map.set y exy !s.Config.find;
	  failwith "dep: to do"
      with
	  Not_found -> ()
      
  let rec propagate_diseq d =   
    toplevel propagate_diseq1 d;

  and propagate_diseq1 d =
    assert(Term.is_var d#lhs);
    assert(Term.is_var d#rhs);
    failwith "To do"
(*
    functionality d;
    Can.Close.on_vardiseq d iter

  (** [x = f(u1,...ri...,un), y = f(u1,...si...,un), x <> y ==> ri <> si] *)
  and functionality d =
    let x = d#lhs and y = d#rhs in
    try
      let ex = Config.Map.find x !s.Config.find 
      and ey = Config.Map.find y !s.Config.find in
	J.Equals.iter
	  (fun e1 -> 
	     assert(Term.eq x e1#lhs);
	     let t1 = e1#rhs in
	       J.Equals.iter
	         (fun e2 -> 
		    assert(Term.eq y e2#lhs);
		    let t2 = e2#rhs in
		      if Funsym.eq (Term.sym_of t1) (Term.sym_of t2) then
			match Term.Args.eqbut (Term.args_of t1) (Term.args_of t2) with
			  | None -> ()
			  | Some(r, s) -> 
			      let d' = J.mk_func 7 (failwith "to do") in
				V.Infsys.process_diseq d')
		 ey)
	  ex
    with
	Not_found -> ()
	  *)

  (** No branching for convex theories. *)
  let branch () = 
    failwith "to do"
(*
    try
      let dl = Can.disjunction iter in
	Some(dl)
    with
	Not_found -> None
*)
	
  let rec normalize _ =
    assert(Config.is_canonical (V.Infsys.current()) !s);
    ()
		      
end 



module Component(Can: T): E.COMPONENT = struct
  let th = Can.th
  module Eqs = Config
  module I = Infsys(Can)
  module Infsys = struct
    type eqs = Eqs.t
    let current = I.current
    let reset = I.reset
    let initialize = I.initialize
    let is_unchanged = I.is_unchanged
    let finalize = I.finalize
    let abstract = I.abstract
    let process_equal = Some(I.process_equal)
    let process_diseq = Some(I.process_diseq)
    let process_pos = None
    let process_nonneg = None
    let propagate_equal = Some(I.propagate_equal)
    let propagate_diseq = Some(I.propagate_diseq)
    let propagate_cnstrnt = None
    let propagate_nonneg = None
    let branch = I.branch
    let normalize = I.normalize
  end
end
